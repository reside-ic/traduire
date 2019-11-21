##' Create a new translator object
##' @title Create translator object
##'
##' @param translations Path to a json file containing translations
##'
##' @param language The default language for the translation
##'
##' @param default_namespace The default namespace to use.  If not
##'   given, then \code{i18next} assumes the namespace
##'   \code{translation}
##'
##' @export
##' @examples
##' path <- system.file("examples/simple.json", package = "traduire")
##' obj <- traduire::i18n(path)
##' obj$t("hello", language = "fr")
i18n <- function(translations, language = NULL, default_namespace = NULL) {
  ## TODO: better defaults here, but there's lots to consider with
  ## fallbacks still
  R6_i18n$new(translations, language %||% "en", default_namespace)
}


R6_i18n <- R6::R6Class(
  "i18n",

  cloneable = FALSE,
  private = list(
    context = NULL
  ),

  public = list(
    initialize = function(translations, language, default_namespace) {
      translations_js <- read_input(translations)
      private$context <- V8::v8()
      private$context$source(traduire_file("js/bundle.js"))
      private$context$call("init", translations_js, language, default_namespace)
    },

    t = function(string, data = NULL, language = NULL, count = NULL,
                 context = NULL) {
      options <- i18n_options(data, language, count, context)
      private$context$call("t", string, options)
    },

    replace = function(text, ...) {
      t <- function(string) {
        self$t(string, ...)
      }
      vapply(text, i18n_replace1, "", t, USE.NAMES = !is.null(names(text)))
    },

    exists = function(string, data = NULL, language = NULL, count = NULL,
                      context = NULL) {
      options <- i18n_options(data, language, count, context)
      private$context$call("exists", string, options)
    },

    set_language = function(language) {
      prev <- self$language()
      private$context$call("i18next.changeLanguage", language)
      invisible(function() self$set_language(prev))
    },

    language = function() {
      private$context$call("language")
    },

    languages = function() {
      private$context$call("languages")
    },

    default_namespace = function() {
      private$context$call("default_namespace")
    },

    set_default_namespace = function(namespace) {
      prev <- self$default_namespace()
      private$context$call("i18next.setDefaultNamespace", namespace)
      invisible(function() self$set_default_namespace(prev))
    }
  )
)


## NOTE: This silently overwrites any data stored in language, count,
## context if they are present in data and provided as an explicit
## argument.
i18n_options <- function(data, language, count, context) {
  data <- data %||% list()
  if (!is.null(language)) {
    data$lng <- language
  }
  if (!is.null(count)) {
    data$count <- count
  }
  if (!is.null(context)) {
    data$context <- context
  }
  if (length(data) == 0) {
    data <- list()
  }
  data
}


## This approach does not support getting additional arguments into
## different replacements (e.g., count, data, context, language) and
## it's not currently obvious that is desirable if this to act as a
## fairly logic-free replacement.
i18n_replace1 <- function(text, t) {
  res <- glue::glue(text,
                    .transformer = function(text, envir) t(text),
                    .open = "t_(", .close = ")")
  ## glue leaves a 'glue' class here, which we don't need or want,
  ## and it's not obviously documented what that class is actually
  ## *for*.
  unclass(res)
}
