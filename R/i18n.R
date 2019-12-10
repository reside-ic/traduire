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
##' @param language The default language for the translation
##'
##' @param default_namespace The default namespace to use.  If not
##'   given, then \code{i18next} assumes the namespace
##'   \code{translation}
##'
##' @param debug Logical, indicating if i18next's debug output should
##'   be turned on.  This will result in lots of output via
##'   \code{message} about various i18next actions.
##'
##' @param resource_pattern A pattern to use for on-demand loading of
##'   translation resources.  Only works if \code{translations} is
##'   \code{NULL} at present.
##'
##' @param namespaces A vector of namespaces to load. Namespaces not
##'   listed here may not be loaded as expected (use \code{debug =
##'   TRUE} to work out what is going on).  The default (\code{NULL})
##'   will use i18next's logic, which is to use \code{translation} as
##'   the only loaded namespace.  This creates some issues if
##'   \code{default_namespace} is set here, as the default namespace
##'   will not be loaded.  A future version of this package will
##'   probably do better with the logic here.
##'
##' @param languages A vector of languages to \emph{preload}. You can
##'   always add additional languages using the \code{load_language}
##'   method.  Note that the adding a language here does not (yet)
##'   mean that failure to load the language is an error.
##'
##' @param fallback The fallback language to use
##'
##' @export
##' @examples
##' path <- system.file("examples/simple.json", package = "traduire")
##' obj <- traduire::i18n(path)
##' obj$t("hello", language = "fr")
i18n <- function(resources, language = NULL, default_namespace = NULL,
                 debug = FALSE, resource_pattern = NULL,
                 namespaces = NULL, languages = NULL,
                 fallback = "dev") {
  ## TODO: better defaults for language, but there's lots to consider
  ## with fallbacks still
  R6_i18n$new(resources, language %||% "en", default_namespace,
              debug, resource_pattern, namespaces, languages, fallback)
}


R6_i18n <- R6::R6Class(
  "i18n",

  cloneable = FALSE,
  private = list(
    context = NULL
  ),

  public = list(
    initialize = function(resources, language, default_namespace,
                          debug, resource_pattern, namespaces, languages,
                          fallback) {
      resources_js <- read_input(resources)
      private$context <- V8::v8()
      private$context$source(traduire_file("js/bundle.js"))
      private$context$call("init", resources_js, scalar(language),
                           safe_js_null(default_namespace),
                           scalar(debug),
                           safe_js_null(resource_pattern),
                           namespaces %||% "translation",
                           safe_js_null(languages),
                           validate_fallback(fallback),
                           auto_unbox = FALSE)
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
validate_fallback <- function(fallback) {
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
