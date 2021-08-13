`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}


traduire_file <- function(path) {
  system.file(path, package = "traduire", mustWork = TRUE)
}


read_file <- function(path) {
  paste(readLines(path, encoding = "UTF-8"), collapse = "\n")
}


## Same logic as jsonvalidate, possibly overkill here. Perhaps we
## always want a path to the input?
read_input <- function(x, name = deparse(substitute(x))) {
  if (is.null(x)) {
    ## This might need to be tuneable - for the initial load we need a
    ## null at least.
    return(jsonlite::unbox("null"))
  }
  if (read_input_is_filename(x)) {
    if (!file.exists(x)) {
      stop(sprintf("'%s' looks like a filename but does not exist", name),
           call. = FALSE)
    }
    x <- read_file(x)
  }

  tryCatch(
    jsonlite::fromJSON(x),
    error = function(e) stop("Failed to parse json input: ", e$message))

  jsonlite::unbox(x)
}


read_input_is_filename <- function(x) {
  RE_JSON <- "[{['\"]"
  !(length(x) != 1 || inherits(x, "AsIs") || grepl(RE_JSON, x))
}


safe_js_null <- function(x) {
  if (is.null(x)) {
    return(V8::JS("null"))
  }
  x
}


is_missing <- function(x) {
  is.null(x) || (length(x) == 1L && is.na(x))
}


scalar <- function(x) {
  jsonlite::unbox(x)
}


squote <- function(x) {
  sprintf("'%s'", x)
}


is_named <- function(x) {
  !is.null(names(x))
}


vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}


starts_with <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
}


expand_paths <- function(paths) {
  stopifnot(all(file.exists(paths)))
  i <- file.exists(paths) & file.info(paths, extra_cols = FALSE)$isdir
  if (any(i)) {
    paths <- as.list(paths)
    paths[i] <- lapply(paths[i], dir, pattern = "\\.[Rr]$", full.names = TRUE)
    paths <- unlist(paths, FALSE, TRUE)
  }
  paths
}


set_names <- function(x, nms) {
  names(x) <- nms
  x
}


strip_quotes <- function(x) {
  gsub("(^[\"']|[\"']$)", "", x)
}


data_frame <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}


vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}


viapply <- function(X, FUN, ...) {
  vapply(X, FUN, integer(1), ...)
}


vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}


vnapply <- function(X, FUN, ...) {
  vapply(X, FUN, numeric(1), ...)
}


browse_html <- function(html, ..., file = tempfile(fileext = ".html")) {
  writeLines(html, file)
  utils::browseURL(file, ...)
  invisible(file)
}


parse_data <- function(exprs) {
  data <- utils::getParseData(exprs)
  data$depth <- cumsum((data$token == "'('") - (data$token == "')'"))
  data$index <- seq_len(nrow(data))
  data
}


## Like match.call, but for the result of the data.frame of parse data
## returned by getParseData.
##
## The idea is to, for the call represented by the expression at
## position 'i' in the parse data identify the locations of the
## arguments corresponding to the *formal* arguments to the function.
##
## So given definition
##
##   function(a, b, c) {}
##
## and a data frame of parse data corresponding to the call
##
##   f(2, a = 1, 3)
##
## We this function will return a list with elements 'a', 'b', and
## 'c', each element of which is a list with entries for the location
## within the parse data for the argument name (if present, NULL if
## not) and the expression correspinding to the value.  See the
## example in test-util.R for details.
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


parse_data_find_call <- function(i, data) {
  stopifnot(data$token[[i + 2L]] == "'('")
  j <- which(data$index > i + 2L & data$depth == data$depth[[i]])[[1]]
  d <- data[i:j, ]
  n <- nrow(d)
  start <- list(line = d$line1[[1]], col = d$col1[[1]])
  end <- list(line = d$line1[[n]], col = d$col1[[n]])

  if (start$line != end$line) {
    stop("CHECKME")
  }

  ## Could also get this from the input too?
  text <- paste(d$text, collapse = "")

  list(text = text, start = start, end = end)
}


string_common_prefix <- function(x) {
  common <- ""
  for (i in seq_len(min(nchar(x)))) {
    prefix <- substr(x, 1L, i)
    if (all(prefix == prefix[[1L]])) {
      common <- prefix[[1L]]
    } else {
      break
    }
  }
  common
}


common_filename <- function(files) {
  ## TODO: this has a terrible failure mode where we could end up with
  ## [path/a.html, path/b.html] giving "foo/.html" when we should
  ## error.
  prefix <- string_common_prefix(files)
  sprintf("%s.%s", sub("-$", "", prefix), tools::file_ext(files[[1]]))
}
