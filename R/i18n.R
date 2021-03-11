##' Create a new translator object
##'
##' @section Warning:
##'
##' Note that the argument list here \emph{will} change.  The only
##'   part of this that we consider stable is that the first argument
##'   will represent a resource bundle.
##'
##' @title Create translator object
##'
##' @param resources Path to a json file containing translation
##'   resources. If given in this way, then on-demand translation
##'   loading (via \code{resource_pattern}) is disabled unless a
##'   currently unexposed i18next option is used.
##'   
##' @param ... Named options passed to \code{\link{traduire_options}}
##' @param options Options object passed to \code{\link{traduire_options}}
##'
##' @export
##' @examples
##' path <- system.file("examples/simple.json", package = "traduire")
##' obj <- traduire::i18n(path)
##' obj$t("hello", language = "fr")
i18n <- function(resources, ..., options = NULL) {
  options <- traduire_options(..., options = options)
  R6_i18n$new(resources, options)
}


R6_i18n <- R6::R6Class(
  "i18n",

  cloneable = FALSE,
  private = list(
    context = NULL
  ),

  public = list(
    initialize = function(resources, options) {
      resources_js <- read_input(resources)
      private$context <- V8::v8()
      private$context$source(traduire_file("js/bundle.js"))
      private$context$call("init", resources_js, 
                           scalar(options[["language"]]),
                           safe_js_null(options[["default_namespace"]]),
                           scalar(options[["debug"]]),
                           safe_js_null(options[["resource_pattern"]]),
                           options[["namespaces"]],
                           safe_js_null(options[["languages"]]),
                           options[["fallback"]],
                           scalar(options[["escape"]]),
                           auto_unbox = FALSE)
    },

    options = function() {
      private$context$call("options")
    },

    t = function(string, data = NULL, language = NULL, count = NULL,
                 context = NULL, escape = NULL) {
      options <- i18n_options(data, language, count, context, escape)
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
      options <- i18n_options(data, language, count, context, FALSE)
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
      private$context$call("defaultNamespace")
    },

    set_default_namespace = function(namespace) {
      prev <- self$default_namespace()
      private$context$call("i18next.setDefaultNamespace", namespace)
      invisible(function() self$set_default_namespace(prev))
    },

    has_resource_bundle = function(language, namespace) {
      private$context$call("i18next.hasResourceBundle", language, namespace)
    },

    add_resource_bundle = function(language, namespace, resources,
                                   deep = FALSE, overwrite = FALSE) {
      resources_js <- read_input(resources)
      private$context$call("addResourceBundle",
                           language, namespace, resources_js, deep, overwrite)
      invisible(self)
    },

    get_resource = function(language, namespace, key, sep = ".") {
      options <- list(keySeparator = scalar(sep))
      private$context$call("getResource", language, namespace, key, options)
    },

    load_namespaces = function(namespaces) {
      private$context$call("i18next.loadNamespaces", namespaces)
      invisible(self)
    },

    load_languages = function(languages) {
      private$context$call("i18next.loadLanguages", languages)
      invisible(self)
    }
  )
)


## NOTE: This silently overwrites any data stored in language, count,
## context if they are present in data and provided as an explicit
## argument.
i18n_options <- function(data, language, count, context, escape) {
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
  if (!is.null(escape)) {
    data$interpolation <- list(escapeValue = scalar(escape))
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


## The debug level here should be tuneable, and that will be easiest
## to do once the logging backend is done.
i18n_backend_read <- function(pattern, language, namespace) {
  if (is_missing(pattern)) {
    return(jsonlite::unbox("null"))
  }
  data <- list(language = language, namespace = namespace)
  path <- glue::glue(pattern, .envir = data)
  tryCatch(
    read_input(path),
    error = function(e) {
      if (language != "dev") {
        message(sprintf(
          "Tried to load language:%s, namespace:%s but failed (%s)",
          language, namespace, e$message))
      }
      return(jsonlite::unbox("null"))
    })
}


## Quite a bit here - if these errors get through to the js, you get
## inscruitable runtime error messages, so we're better off validating
## in R.
validate_fallback <- function(fallback, name) {
  if (is.null(fallback)) {
    return(V8::JS("null"))
  }

  check_fallback_entries <- function(x) {
    any(is.na(x) | !nzchar(x))
  }

  if (is_named(fallback)) {
    fallback <- as.list(fallback)
  }
  if (is.character(fallback)) {
    if (check_fallback_entries(fallback)) {
      stop("All fallback entries must be non-NA and non-empty")
    }
  } else if (is.list(fallback)) {
    if (!is_named(fallback)) {
      stop("If fallback is a list, it must be named")
    }
    if (any(duplicated(names(fallback)))) {
      stop("Duplicated names in fallback")
    }
    if (!all(nzchar(names(fallback)))) {
      stop("Zero-length names in fallback")
    }
    err <- !vlapply(fallback, is.character)
    if (any(err)) {
      stop(sprintf("All elements of fallback must be character (see %s)",
                   paste(squote(names(fallback)[err]), collapse = ", ")))
    }
    err <- vlapply(fallback, check_fallback_entries)

    if (any(err)) {
      stop(sprintf(
        "All elements of fallback must be non-NA and non-empty (see %s)",
        paste(squote(names(fallback)[err]), collapse = ", ")))
    }
  } else {
    stop("fallback must be a character vector or list of character vectors")
  }
  fallback
}
