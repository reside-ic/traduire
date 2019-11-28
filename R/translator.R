##' Register a translator within the \code{traduire} package, and use
##' it directly. This will allow your code to access translations via
##' \code{translator_translate} (or more concisely \code{t_}) without
##' having to pass around a translation object.  If called from
##' package code (and assuming a single translator per package) then
##' the \code{name} argument can be omitted and will be automatically
##' converted into \code{package:<packagename}).
##'
##' @section Use in package code:
##'
##' The intention is that this would typically be called from
##'   \code{.onLoad}, something like:
##'
##' \preformatted{
##' .onLoad <- function(...) {
##'   path <- system.file("traduire.json", package = "hello", mustWork = TRUE)
##'   traduire::translator_register(path, "en")
##' }
##' }
##'
##' and then used from that package's code as
##'
##' \preformatted{
##' traduire::t_("key")
##' }
##'
##' The language option for this translator can be changed by
##'
##' \preformatted{
##' traduire::translator_set_language("es")
##' }
##'
##' Every package's translator object is isolated from every other
##'   package, and if the \code{traduire} functions are called from
##'   your package code, then the correct translator should be found
##'   automatically.
##'
##' @section Warning:
##'
##' Do not use \code{translator_unregister} on someone elses's
##'   translator, particularly not in package code, or things will
##'   break.  This may get tightened up at some point (RESIDE-79).
##'
##' @title Register a translator
##' @inheritParams i18n
##'
##' @param name Optional name for the translator.  If omitted, this
##'   will be determined automatically if called from package code
##'
##' @export
##' @rdname translator
##' @examples
##' path <- system.file("examples/simple.json", package = "traduire")
##' traduire::translator_register(path, name = "myexample")
##' traduire::t_("hello", language = "fr", name = "myexample")
##' "myexample" %in% traduire::translator_list()
##' traduire::translator_unregister("myexample")
translator_register <- function(resources, language = NULL, name = NULL) {
  name <- name_from_context(name)
  translators[[name]] <- i18n(resources, language)
}


##' @export
##' @rdname translator
translator_unregister <- function(name = NULL) {
  name <- name_from_context(name)
  rm(list = name, envir = translators)
}


##' @param ... Arguments passed to \code{\link{i18n}}'s \code{t}
##'   method, being \code{string}, \code{data}, \code{language} etc.
##'
##' @inheritParams translator_register
##' @export
##' @rdname translator
translator_translate <- function(..., name = NULL) {
  translator(name)$t(...)
}


##' @export
##' @rdname translator
t_ <- translator_translate


##' @export
##' @rdname translator
translator <- function(name = NULL) {
  name <- name_from_context(name)
  translator <- translators[[name]]
  if (is.null(translator)) {
    stop(sprintf("Did not find translator '%s'", name))
  }
  translator
}


##' @param language Language to use, passed through to
##'   \code{\link{i18n}}'s \code{set_language} method
##'
##' @export
##' @rdname translator
translator_set_language <- function(language, name = NULL) {
  translator(name)$set_language(language)
}


##' @rdname translator
##' @export
translator_list <- function() {
  names(translators)
}


name_from_context <- function(name = NULL) {
  if (is.null(name)) {
    depth <- 1L
    name <- utils::packageName(parent.frame(depth))
    while (!is.null(name) && name == "traduire") {
      depth <- depth + 1L
      name <- utils::packageName(parent.frame(depth))
    }
    if (is.null(name) || !nzchar(name)) {
      stop("Did not determine environment name")
    }
    name <- paste0("package:", name)
  }
  name
}


translators <- new.env(parent = emptyenv())
