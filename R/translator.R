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
##' If you need to get a translation for another package, you should
##'   use \code{package} argument, for example:
##'
##' \preformatted{
##' traduire::t_("key", package = "other")
##' }
##'
##' You can change the language in another package (e.g., using
##'   \code{traduire::change_language("en", package = "other")}) but
##'   should be careful to reset this using the returned reset
##'   function.
##'
##' It is not possible to unregister a translator in another package,
##'   or to overwrite one.
##'
##' Translators provided in other packages will be listed by
##'   \code{traduire::translator_list} with the prefix \code{package:}
##'   (e.g., \code{package:other}) however, you should not access them
##'   directly using \code{name = "package:other"}.
##'
##' @title Register a translator
##'
##' @param ... For \code{translator_register}, arguments passed to
##'   \code{\link{traduire_options}} to build the translator object.  All
##'   arguments are accepted.  For \code{translator_translate} and
##'   \code{t_}, arguments passed to the \code{$t} method of the
##'   translator object, being \code{string}, \code{data},
##'   \code{language} etc.
##'
##' @param name Optional name for the translator.  This should be used
##'   only when not using this interface from a package (e.g., from a
##'   shiny application).  If using from a package you can omit both
##'   \code{name} and \code{package}, and if interacting with
##'   translations from another package you should use the
##'   \code{package} argument (see below).
##'
##' @param package Optional name for the package to find a translator
##'   in.  This cannot be provided for \code{translator_register} and
##'   \code{translator_unregister} as these should either be
##'   registered by \code{name} or the package will be determined
##'   automatically.
##'
##' @export
##' @rdname translator
##' @examples
##' path <- system.file("examples/simple.json", package = "traduire")
##' traduire::translator_register(path, name = "myexample")
##' traduire::t_("hello", language = "fr", name = "myexample")
##' "myexample" %in% traduire::translator_list()
##' traduire::translator_unregister("myexample")
translator_register <- function(..., name = NULL) {
  name <- name_from_context(name, NULL, strict = TRUE)
  translators[[name]] <- i18n(...)
}


##' @export
##' @rdname translator
translator_unregister <- function(name = NULL) {
  name <- name_from_context(name, NULL, strict = TRUE)
  rm(list = name, envir = translators)
}


##' @export
##' @rdname translator
translator_translate <- function(..., name = NULL, package = NULL) {
  translator(name, package)$t(...)
}


##' @export
##' @rdname translator
t_ <- translator_translate


##' @export
##' @rdname translator
translator <- function(name = NULL, package = NULL) {
  name <- name_from_context(name, package, FALSE)
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
translator_set_language <- function(language, name = NULL, package = NULL) {
  translator(name, package)$set_language(language)
}


##' @rdname translator
##' @export
translator_list <- function() {
  names(translators)
}


## name will be provided only for non-package use (hence the
## check/stop below)
##
## if used from a package, then package should be empty - that
## argument should be used when refering to other packages.
##
## In order to prevent registration/unregistration of translators from
## other packages, if  package *is* provided and strict  is TRUE, then
## we verify that the correct name was given.
name_from_context <- function(name, package, strict) {
  prefix <- "package:"
  if (!is.null(package)) {
    validate_package_name(package, strict)
    name <- paste0(prefix, package)
  } else if (!is.null(name) && starts_with(name, prefix)) {
    stop(sprintf("Do not use '%s' prefix directly", prefix))
  }
  if (is.null(name)) {
    package <- package_from_context()
    name <- paste0(prefix, package)
  }
  name
}


package_from_context <- function(depth = 1L) {
  name <- utils::packageName(parent.frame(depth))
  while (!is.null(name) && name == "traduire") {
    depth <- depth + 1L
    name <- utils::packageName(parent.frame(depth))
  }
  if (is.null(name) || !nzchar(name)) {
    stop("Did not determine environment name")
  }
  name
}


validate_package_name <- function(given, strict) {
  if (strict) {
    called <- package_from_context()
    if (called != given) {
      stop(sprintf("Package mismatch - called with %s, called from %s",
                   called, given))
    }
  }
}


translators <- new.env(parent = emptyenv())
