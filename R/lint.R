##' Find likely translation errors using static source code analysis.
##' We look into a series of R source files and identify all calls to
##' \code{t_} and examine the key and interpolation data used within
##' these calls.
##'
##' This will expand in future and is currently quite limited.
##' Currently, only calls to \code{t_()} are inspected - not calls to
##' \code{$t()} in a translator object, not calls to \code{$replace}
##' etc.  The \code{package} and \code{name} arguments to \code{t_()}
##' are also currently ignored, which will lead to spurious errors.  A
##' version of this that automatically works for packages will also be
##' written.  The key and interpolation data must (currently) be
##' literals - you cannot save the key or data as a variable and pass
##' that by variable name.
##'
##' The function \code{lint_translations_package} is a wrapper around
##' \code{lint_translations} to make working with a package a bit
##' easier.  It's a bit flakey at the moment because the translator
##' will be loaded from the currently installed package, but the usage
##' will be checked in the source tree.  This will be addressed in a
##' future version (see reside-90)
##'
##' @title Lint translations
##'
##' @param path Path to the translations to test.  This can be a
##'   character vector of file names and directory names - directory
##'   names are expanded (non-recursively) to find all \code{.R} files.
##'
##' @param obj A translator object.
##'
##' @param language Optional language to use as the language to lint.
##'   Each language must be at present linted separately
##'
##' @param root A root directory, below which the files in \code{path}
##'   will be treated relatively (e.g., pass a package's root
##'   directory here, and then set \code{path = "R"}).  Using this
##'   shortens the reported filenames considerably
##'
##' @param title An optional title that will be used in reporting
##'
##' @return A \code{lint_translations} object, which can be passed
##'   through to \code{\link{lint_translations_report}}
##'
##' @export
lint_translations <- function(path, obj, language = NULL, root = NULL,
                              title = NULL) {
  if (!is.null(root)) {
    owd <- setwd(root)
    on.exit(setwd(owd))
  }
  options <- obj$options()
  common <- list(default_namespace = obj$default_namespace(),
                 language = language %||% obj$language(),
                 prefix = options$interpolation[["prefix"]] %||% "{{",
                 suffix = options$interpolation[["suffix"]] %||% "}}",
                 escape = options$interpolation[["escape"]] %||% "- ")

  usage <- lapply(expand_paths(path), lint_get_usage_file)
  results <- lapply(usage, lint_compare_usage_file, obj, common)

  attr(results, "title") <- title %||% "Translation lint report"
  class(results) <- "lint_translations"
  results
}


##' @export
##' @rdname lint_translations
lint_translations_package <- function(root, language = NULL) {
  desc <- file.path(root, "DESCRIPTION")
  if (!file.exists(desc)) {
    stop(sprintf("Did not find DESCRIPTION at '%s'", root))
  }
  package <- read.dcf(desc, "Package")[[1]]
  if (is.na(package)) {
    stop("Invalid DESCRIPTION - failed to read package name")
  }
  ## TODO: See reside-90
  loadNamespace(package)
  obj <- translator(package = package)
  title <- sprintf("{%s}", package)
  lint_translations("R", obj, language, root, title = title)
}


##' Report on translation linting.  At present this produces a
##' single-page html report that can be opened in a web browser.
##' Future versions will include reporting to the terminal.
##'
##' @title Show translation lint report
##'
##' @param x A \code{lint_translations} object from
##'   \code{\link{lint_translations}}
##'
##' @param file File to save the report in (default is to use a
##'   temporary file with a file extension of \code{.html}).
##'
##' @return This function is called for the side effect of viewing a
##'   report.  It will return (invisibly) the path to the produced
##'   report.
##'
##' @export
lint_translations_report <- function(x, file = tempfile(fileext = ".html")) {
  html <- lint_translations_html_report(x)
  browse_html(html, file = file)
}


lint_get_usage_file <- function(path) {
  text <- readLines(path)
  exprs <- parse(file = path, keep.source = TRUE)
  data <- utils::getParseData(exprs)
  data$depth <- cumsum((data$token == "'('") - (data$token == "')'"))
  data$index <- seq_len(nrow(data))
  idx <- data$token == "SYMBOL_FUNCTION_CALL" & data$text == "t_"
  usage <- lapply(which(idx), lint_get_usage_expr, data)
  list(path = path, text = text, data = data, usage = usage)
}


lint_get_usage_expr <- function(i, data) {
  stopifnot(data$token[[i + 2L]] == "'('")
  stopifnot(data$token[[i - 1L]] == "expr")

  ## This is the overall expression id - if this is highlighted then
  ## the whole relevant part of the translation will be.
  ##
  ## The more robust way of getting this looks to be to get the parent
  ## of this, but we're working with basically undocumented data
  ## structures here...
  index <- data$index[[i - 1L]]

  args <- parse_data_match_call(i, data, function(key, data, ...) {})
  key_index <- args$key$value[[1]]

  ## The idea here is that strings are easy to resolve, symbols might
  ## be possible, anything else (e.g., a function call) will not be.
  ## So assume missing unless we're able to determine it:
  key <- list(namespace = NULL, key = NA_character_, index = key_index)
  if (data$token[key_index] == "STR_CONST") {
    key$key <- strip_quotes(data$text[key_index])
    if (grepl(":", key$key, fixed = TRUE)) {
      key_split <- strsplit(key$key, ":", fixed = TRUE)[[1]]
      if (length(key_split) == 2) {
        key$namespace <- key_split[[1]]
        key$key <- key_split[[2]]
      }
    }
  }

  if (is.null(args$data)) {
    interpolation <- list(found = TRUE)
  } else {
    data_index <- args$data$value[[2]]
    if (data$token[[data_index]] == "SYMBOL_FUNCTION_CALL" &&
        data$text[[data_index]] == "list") {
      interpolation <- list(
        data = parse_data_match_call(data_index, data, function(...) {}),
        found = TRUE)
    } else {
      interpolation <- list(found = FALSE)
    }
  }

  list(index = index,
       key = key,
       interpolation = interpolation)
}


lint_compare_usage_file <- function(usage, obj, common) {
  usage$usage <- lapply(usage$usage, lint_compare_usage_expr,
                        usage$data, obj, common)

  usage$info <- Markup$new(usage$text, usage$data)
  for (x in usage$usage) {
    lint_collect_errors_expr(x, usage$info)
  }

  usage
}


lint_compare_usage_expr <- function(x, data, obj, common) {
  x <- lint_compare_usage_expr_key(x, data, obj, common)
  x <- lint_compare_usage_expr_interpolation(x, data, obj, common)
  x
}


lint_compare_usage_expr_key <- function(x, data, obj, common) {
  namespace <- x$key$namespace %||% common$default_namespace
  if (is.na(x$key$key)) {
    x$key$value <- NA_character_
    x$key$namespace_computed <- NA_character_
    x$key$exists <- NA
  } else {
    x$key$value <- obj$get_resource(common$language, namespace, x$key$key)
    x$key$namespace_computed <- namespace
    x$key$exists <- !is.null(x$key$value)
  }
  x
}


lint_compare_usage_expr_interpolation <- function(x, data, obj, common) {
  if (!isTRUE(x$key$exists)) {
    return(x)
  }

  fields <- glue_extract(x$key$value, common$prefix, common$suffix)
  i <- starts_with(fields$text, common$escape)
  if (any(i)) {
    fields$text[i] <- substr(fields$text[i], nchar(common$escape) + 1L,
                             nchar(fields$text[i]))
  }

  x$interpolation$fields <- fields
  if (x$interpolation$found) {
    given <- names(x$interpolation$data) %||% character(0)
    needed <- fields$text
    x$interpolation$missing <- setdiff(needed, given)
    x$interpolation$unused <- setdiff(given, c(needed, c("count", "context")))
    x$interpolation$valid <-
      length(x$interpolation$missing) == 0 &&
      length(x$interpolation$unused) == 0
  } else {
    x$interpolation$valid <- NA
  }

  x
}


lint_collect_errors_expr <- function(x, m) {
  m$start()
  m$add("EXPR", x$index)

  if (is.na(x$key$exists)) {
    msg <- "Translation key could not be determined without string"
    m$add("UNKNOWN_KEY", x$key$index, msg)
  } else if (!x$key$exists) {
    msg <- sprintf("Translation key '%s:%s' not found",
                   x$key$namespace_computed, x$key$key)
    m$add("MISSING_KEY", x$key$index, msg)
  } else if (is.na(x$interpolation$valid)) {
    msg <- "Interpolation data could not be determined"
    m$add("UNKNOWN_DATA", x$key$index, msg)
  } else if (!x$interpolation$valid) {
    for (i in x$interpolation$unused) {
      m$add("INTERPOLATION_UNUSED", x$interpolation$data[[i]]$name,
            sprintf("Interpolation key '%s' unused", i))
    }
    ## TODO: these should be concatenated here I think
    for (i in x$interpolation$missing) {
      m$add("INTERPOLATION_MISSING", x$key$index,
            sprintf("Interpolation key '%s' missing", i))
    }
  } else {
    m$add("VALID", x$index)
  }
}


lint_tags <- function(tags) {
  if (!setequal(names(tags), lint_tags_names())) {
    stop("Invalid tag names")
  }
  tags
}


lint_tags_names <- function() {
  c("EXPR", "VALID", "UNKNOWN_KEY", "UNKNOWN_DATA", "MISSING_KEY",
    "INTERPOLATION_UNUSED", "INTERPOLATION_MISSING")
}
