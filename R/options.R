#' Create a set of options for traduire
#' 
#' Takes a named set of options and optionally an already created 
#' options object. If both are set, named options will overwrite 
#' any options in the object.
#' 
#' These options are passed to i18next.
#'
#' @param ... Named options
##' 
##' language The default language for the translation
##'
##' default_namespace The default namespace to use.  If not
##'   given, then \code{i18next} assumes the namespace
##'   \code{translation}
##'
##' debug Logical, indicating if i18next's debug output should
##'   be turned on.  This will result in lots of output via
##'   \code{message} about various i18next actions.
##'
##' resource_pattern A pattern to use for on-demand loading of
##'   translation resources.  Only works if \code{translations} is
##'   \code{NULL} at present.
##'
##' namespaces A vector of namespaces to load. Namespaces not
##'   listed here may not be loaded as expected (use \code{debug =
##'   TRUE} to work out what is going on).  The default (\code{NULL})
##'   will use i18next's logic, which is to use \code{translation} as
##'   the only loaded namespace.  This creates some issues if
##'   \code{default_namespace} is set here, as the default namespace
##'   will not be loaded.  A future version of this package will
##'   probably do better with the logic here.
##'
##' languages A vector of languages to \emph{preload}. You can
##'   always add additional languages using the \code{load_language}
##'   method.  Note that the adding a language here does not (yet)
##'   mean that failure to load the language is an error.
##'
##' fallback The fallback language to use.  The options here
##'   are to use a character string (a single fallback to use for all
##'   languages), a character vector (a series of languages to use in
##'   turn, listed from first to try to last to try) or a named list
##'   of language-fallback mappings, e.g., \code{list("de-CH": c("fr",
##'   "it"), "es": "fr")}.
##'
##' escape Logical, indicating if the translation output should
##'   be, by default, escaped (see the i18next interpolation
##'   documentation).  The i18next implementation is to prevent xss
##'   attacks, and so is disabled by default in traduire.
#' @param options traduire_options object
#'
#' @return
#' @export
#'
#' @examples
traduire_options <- function(..., options = NULL) {
  args <- list(...)
  if (length(args) > 0) {
    assert_named(args, unique = TRUE, name = "options")
  }
  if (!is.null(options) && !is_traduire_options(options)) {
    stop("Options must be of type 'traduire_options'")
  }
  
  null_or_validate <- function(validate_func) {
    function(x, name = deparse(substitute(x))) {
      if (!is.null(x)) {
        validate_func(x, name)
      }
    }
  }
  
  validate_funcs <- list(
    language = assert_scalar_character,
    default_namespace = null_or_validate(assert_scalar_character),
    debug = assert_scalar_logical,
    resource_pattern = null_or_validate(assert_scalar_character),
    namespaces = assert_character,
    languages = null_or_validate(assert_character),
    fallback = validate_fallback,
    escape = assert_scalar_logical
  )
  
  if (is.null(options)) {
    ## TODO: better defaults for language, but there's lots to consider
    ## with fallbacks still
    language <- args[["language"]] %||% "en"
    debug <- args[["debug"]] %||% FALSE
    namespaces <- args[["namespaces"]] %||% "translation"
    fallback <- args[["fallback"]] %||% "dev"
    escape <- args[["escape"]] %||% FALSE
    options <- list(
      language = language,
      default_namespace = args[["default_namespace"]],
      debug = debug,
      resource_pattern = args[["resource_pattern"]],
      namespaces = namespaces,
      languages = args[["languages"]],
      fallback = fallback,
      escape = escape)
    for (option in names(options)) {
      validate_func <- validate_funcs[[option]]
      validate_func(options[[option]], option)
    }
    options <- structure(options, class = "traduire_options")
  } else {
    ## Overwrite args which have been passed in
    for (arg in names(args)) {
      validate_func <- validate_funcs[[arg]]
      validate_func(args[[arg]], arg)
      options[[arg]] <- args[[arg]]
    }
  }
  options
}

is_traduire_options <- function(obj) {
  inherits(obj, "traduire_options")
}
