lint_translations <- function(path, obj, language = NULL) {
  options <- obj$options()
  common <- list(default_namespace = obj$default_namespace(),
                 language = language %||% obj$language(),
                 prefix = options$interpolation[["prefix"]] %||% "{{",
                 suffix = options$interpolation[["suffix"]] %||% "}}",
                 escape = options$interpolation[["escape"]] %||% "- ")

  usage <- lapply(expand_paths(path), lint_get_usage)
  results <- lapply(usage, lint_compare_usage, obj, common)

  class(results) <- "lint_translations"
  results
}

## TODO: Can't use when translation key is a symbol
## TODO: Can't use when interpolation data is a symbol
## TODO: After above, can't deal with setNames-style data creation
## TODO: No equivalent for use within $replace contexts
## TODO: Not searching for $t() usage

lint_get_usage <- function(path) {
  text <- readLines(path)
  exprs <- parse(file = path, keep.source = TRUE)
  data <- utils::getParseData(exprs)
  data$depth <- cumsum((data$token == "'('") - (data$token == "')'"))
  data$index <- seq_len(nrow(data))
  idx <- data$token == "SYMBOL_FUNCTION_CALL" & data$text == "t_"
  usage <- lapply(which(idx), lint_get_usage1, data)
  list(path = path, text = text, data = data, usage = usage)
}


lint_get_usage1 <- function(i, data) {
  stopifnot(data$token[[i + 2L]] == "'('")
  stopifnot(data$token[[i - 1L]] == "expr")

  ## This is the overall expression id - if this is highlighted then
  ## the whole relevant part of the translation will be.
  ##
  ## The more robust way of getting this looks to be to get the parent
  ## of this, but we're working with basically undocumented data
  ## structures here...
  index <- data$index[[i - 1L]]

  res <- parse_data_match_call(i, data, function(key, data, ...) {})
  ## TODO: if this is a symbol, then we could try and look it up, or
  ## flag it as unknowable.
  stopifnot(data$token[res$key$value[[1]]] == "STR_CONST")
  key <- list(namespace = NULL,
              key = strip_quotes(data$text[res$key$value[[1]]]),
              index = res$key$value[[1]])
  if (grepl(":", key$key, fixed = TRUE)) {
    key_split <- strsplit(key$key, ":", fixed = TRUE)[[1]]
    if (length(key_split) == 2) {
      key$namespace <- key_split[[1]]
      key$key <- key_split[[2]]
    }
  }

  ## Then the interpolation data, if present.  Getting this from a
  ## symbol (i.e., when not inline) is more important because it won't
  ## always be convenient to write it that way.
  if (is.null(res$data)) {
    interpolation <- NULL
  } else {
    stopifnot(data$token[res$data$value[[2]]] == "SYMBOL_FUNCTION_CALL")
    interpolation <- list(data = parse_data_match_call(
      res$data$value[[2]], data, function(...) {}))
  }

  list(index = index,
       key = key,
       interpolation = interpolation)
}


lint_compare_usage <- function(usage, obj, common) {
  usage$usage <- lapply(usage$usage, lint_compare_usage1,
                        usage$data, obj, common)
  usage
}


lint_compare_usage1 <- function(x, data, obj, common) {
  x <- lint_compare_usage1_key(x, data, obj, common)
  x <- lint_compare_usage1_interpolation(x, data, obj, common)
  x
}


lint_compare_usage1_key <- function(x, data, obj, common) {
  namespace <- x$key$namespace %||% common$default_namespace
  x$key$value <- obj$get_resource(common$language, namespace, x$key$key)
  x$key$namespace_computed <- namespace
  x$key$exists <- !is.null(x$key$value)
  x
}


lint_compare_usage1_interpolation <- function(x, data, obj, common) {
  if (!x$key$exists) {
    return(x)
  }

  fields <- glue_extract(x$key$value, common$prefix, common$suffix)
  i <- starts_with(fields, common$escape)
  if (any(i)) {
    fields[i] <- substr(fields[i], nchar(common$escape) + 1L, nchar(fields))
  }

  ## TODO: we might want to treat the case of no interpolation data
  ## specially?
  given <- names(x$interpolation$data) %||% character(0)

  x$interpolation$missing <- setdiff(fields, given)
  x$interpolation$unused <- setdiff(given, c(fields, c("count", "context")))
  x$interpolation$valid <-
    length(x$interpolation$missing) == 0 &&
    length(x$interpolation$unused) == 0
  x
}


## There's some common code here that could be factored out, because
## we'll do similar things elsewhere.
test_package_translations <- function(language, root, obj) {
  files <- dir(file.path(root, "R"), pattern = "\\.[Rr]$", full.names = TRUE)
  usage <- lapply(files, function(p) lint_get_usage(p, basename(p)))
  common <- list(default_namespace = obj$default_namespace(),
                 language = language %||% obj$language(),
                 ## These should come from the object
                 prefix = "{{",
                 suffix = "}}",
                 escape = "- ")
  results <- lapply(unlist(usage, FALSE), lint_compare_usage1, obj, common)



  ## Probably the easiest way here is to create a template?

  browser()
  test_that(label, {
    for (x in results) {
      expect_translation_correct(x)
    }
  })
}


expect_translation_correct <- function(x) {
  if (x$status$success) {
    testthat::succeed()
    return(invisible())
  }
  testthat::fail(x$status$message)
}


parse_data_match_call <- function(i, data, definition) {
  stopifnot(data$token[[i + 2L]] == "'('")

  j <- which(data$index > i + 2L & data$depth == data$depth[[i]])[[1]]
  d <- data[(i + 3L):(j - 1L), ]
  comma <- d$token == "','" & d$depth == data$depth[[i]] + 1L
  d$arg <- cumsum(comma) + 1L
  d$arg[comma] <- 0L

  ## Then for each argument group we're just looking to see if they
  ## are *named* or not; that's just a presence of a SYMBOL_SUB,
  ## EQ_SUB as the first two members.
  args <- vector("list", max(d$arg))
  nms <- character(length(args))

  for (k in seq_along(args)) {
    sub <- d[d$arg == k, ]
    if (identical(sub$token[1:2], c("SYMBOL_SUB", "EQ_SUB"))) {
      args[[k]] <- list(name = sub$index[[1]], value = sub$index[-(1:2)])
      nms[[k]] <- sub$text[[1L]]
    } else {
      args[[k]] <- list(name = NULL, value = sub$index)
    }
  }

  call <- as.call(c(list(quote(.)), set_names(args, nms)))
  as.list(match.call(definition, call))[-1L]
}


parse_data <- function(exprs) {
  data <- utils::getParseData(exprs)
  data$depth <- cumsum((data$token == "'('") - (data$token == "')'"))
  data$index <- seq_len(nrow(data))
  data
}
