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
  if (read_input_is_filename(x)) {
    if (!file.exists(x)) {
      stop(sprintf("'%s' looks like a filename but does not exist", name),
           call. = FALSE)
    }
    x <- read_file(x)
  }
  V8::JS(x)
}


read_input_is_filename <- function(x) {
  RE_JSON <- "[{['\"]"
  !(length(x) != 1 || inherits(x, "AsIs") || grepl(RE_JSON, x))
}
