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


env_to_name <- function(e) {
  str <- utils::capture.output(print(e))
  re <- "^<environment: 0x([[:xdigit:]]+)>$"
  stopifnot(is.environment(e), length(str) == 1, grepl(re, str))
  paste0("env", sub(re, "\\1", str))
}


list_to_character <- function(x) {
  vapply(x, identity, "", USE.NAMES = FALSE)
}
