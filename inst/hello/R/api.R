##' Runs an API server that says hello
##' @title Hello as a service
##' @param port Port to use
##' @export
api <- function(port = 8888) {
  path <- system.file("plumber.R", package = "hello", mustWork = TRUE)
  pr <- plumber::plumb(path)
  pr$registerHook("preroute", api_set_language)
  pr$registerHook("postserialize", api_reset_language)
  pr$run(port = port)
}


## This a hook for use with plumber to change the language at the
## start of the request.
api_set_language <- function(data, req, res) {
  if ("accept-language" %in% names(req$HEADERS)) {
    language <- req$HEADERS[["accept-language"]]
    data$reset_language <- traduire::translator_set_language(language)
  }
}


## And this wll be used to reset it to whatever was used at the start
## of the request.
api_reset_language <- function(data, req, res, value) {
  if (!is.null(data$reset)) {
    data$reset_language()
  }
  value
}
