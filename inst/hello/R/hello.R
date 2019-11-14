##' Say Hello with a cow
##' @title Say hello with a cow
##' @param ... Arguments passed to \code{cowsay::say}
##' @export
hello <- function(...) {
  cowsay::say("Hello", "cow", ...)
}


##' Say hello, in some language, with a cow.  This version uses an
##' explicit language argument
##' @title Say hello to the world with a cow
##' @param ... Arguments passed to \code{cowsay::say}
##' @param language Language to translate into
##' @export
##' @importFrom traduire t_
world <- function(language = "en", ...) {
  cowsay::say(t_("hello", language = language), "cow", ...)
}


##' Say hello, in some language, with a cow.  This version uses an
##' implicit language argument
##' @title Say hello to the world with a cow
##' @param ... Arguments passed to \code{cowsay::say}
##' @export
monde <- function(...) {
  cowsay::say(t_("hello"), ...)
}


## This enables use of traduire::translate
.onLoad <- function(...) {
  path <- system.file("traduire.json", package = "hello", mustWork = TRUE)
  traduire::translator_register(path, "en")
}
