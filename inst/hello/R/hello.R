##' Say Hello with a cow, possiblty internationalised.
##'
##' \itemize{
##' \item{hello always says hello, always with a cow}
##'
##' \item{world says "Hello world!" in a number of languages, using an
##' explicit trasnslation argument, and always uses a cow}
##'
##' \item{monde says "Hello world!" in a number of languages, using an
##' implicit trasnslation argument, and can use many animals}
##' }
##'
##' @title Say hello with a cow (or other animal)
##' @param ... Arguments passed to \code{cowsay::say}
##' @export
##' @rdname hello
hello <- function(...) {
  cowsay::say("Hello", "cow", ...)
}

##' @param language Language to translate into
##' @export
##' @importFrom traduire t_
##' @rdname hello
world <- function(language = "en", ...) {
  cowsay::say(t_("hello", language = language), "cow", ...)
}

##' @export
##' @rdname hello
monde <- function(...) {
  cowsay::say(t_("hello"), ...)
}

## This enables use of traduire::translate
.onLoad <- function(...) {
  path <- system.file("traduire.json", package = "hello", mustWork = TRUE)
  traduire::translator_register(path, "en")
}
