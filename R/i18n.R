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
      data <- data %||% list()
      data$lng <- language
      data$count <- count
      if (length(data) == 0) {
        data <- NA
      }
      private$context$call("t", string, data)
    }
  )
)
