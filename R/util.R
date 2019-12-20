`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}


traduire_file <- function(path) {
  system.file(path, package = "traduire", mustWork = TRUE)
}


read_file <- function(path) {
  paste(readLines(path), collapse = "\n")
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


glue_extract <- function(text, prefix, suffix) {
  found <- character()
  extractor <- function(text, envir) {
    found <<- c(found, trimws(text))
    text
  }
  glue::glue(text,
             .transformer = extractor,
             .open = prefix, .close = suffix)
  found
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


read_file <- function(path) {
  paste(readLines(path), collapse = "\n")
}


browse_html <- function(html) {
  tmp <- tempfile()
  writeLines(html, tmp)
  browseURL(tmp)
  invisible(tmp)
}
