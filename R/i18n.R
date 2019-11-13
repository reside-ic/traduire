##' Create a new translator object
##' @title Create translator object
##' @param translations Path to a json file containing translations
##' @param language The default language for the translation
##' @export
##' @examples
##' path <- system.file("examples/simple.json", package = "traduire")
##' obj <- traduire::i18n(path)
##' obj$t("hello", language = "fr")
i18n <- function(translations, language = "en") {
  R6_i18n$new(translations, language)
}


R6_i18n <- R6::R6Class(
  "i18n",

  cloneable = FALSE,
  private = list(
    context = NULL
  ),

  public = list(
    initialize = function(translations, language) {
      translations_js <- read_input(translations)
      private$context <- V8::v8()
      private$context$source(traduire_file("js/bundle.js"))
      private$context$call("init", translations_js, language)
    },

    t = function(string, data = NULL, language = NULL, count = NULL) {
      options <- i18n_options(data, language, count)
      private$context$call("t", string, options)
    },

    exists = function(string, data = NULL, language = NULL, count = NULL) {
      options <- i18n_options(data, language, count)
      private$context$call("exists", string, options)
    },

    set_language = function(language) {
      private$context$call("i18next.changeLanguage", language)
    },

    language = function() {
      private$context$call("language")
    },

    languages = function() {
      private$context$call("languages")
    }
  )
)


i18n_options <- function(data, language, count) {
  data <- data %||% list()
  data$lng <- language
  data$count <- count
  if (length(data) == 0) {
    data <- NA
  }
  data
}
