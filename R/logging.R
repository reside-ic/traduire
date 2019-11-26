##' Create a function suitable for logging from traduire.  This
##' approach splits the logging process into three steps: (1)
##' transform, (2) format, (3) emit.  You can provide functions that
##' carry out any of these steps, falling back on reasonable defaults.
##' The default logger lightly processes the i18next output, formats
##' it as a string suitable for display to screen, and then uses
##' \code{message} to send it to the console.
##'
##' The \code{transform} stage will recieve data like \code{level =
##' "warn", key = "i18next::translator: missingKey", [...]} where the
##' last value is an array of json values.  There is no reference as
##' to what the keys or data might be, save for reading the
##' \code{i18next} source code.  The default transformation attempts
##' to unmarshal the json data in a sensible way, and will return
##' either a named or an vector for the \code{data}.  We will try and
##' improve this function, and hopefully this default \code{transform}
##' stage will be generally useable.
##'
##' The \code{format} function will recieve the same \code{level} and
##' \code{key{} values as \code{transform}, but whatever
##' \code{transform} transforms the \code{data} into as its
##' \code{data} argument, You can use transform to use alternative
##' string interpolations that better suit your logging needs (e.g.,
##' adding a timestamp, or other contextual information about the
##' state of the application if you make that available through use of
##' an appropriate closure.
##'
##' The \code{emit} function is the simplest and probably the most
##' sensible to provide a custom value for. You can log to however
##' many sources you want (e.g., both send to screen and to a
##' database).  The return value is ignored.
##'
##' If you throw an error in any of these functions, it will not be
##' caught.
##'
##' @title Create a logging function
##'
##' @param transform A transformation function - takes arguments
##'   \code{level}, \code{key}, \code{data} and returns \code{data},
##'   suitably transformed, but in any format.  This will be passed as
##'   \code{data} to \code{format}.
##'
##' @param format A formatting function - takes arguments
##'   \code{level}, \code{key} and \code{data}, and returns an object
##'   suitable for the \code{data} element of the \code{emit}
##'   function.
##'
##' @param emit An emmitting function, used for its side effect of
##'   printing (or otherwise shipping) the log value.  You might use
##'   this to write to a file, log to a database, or log to the screen
##'   with different colours (perhaps using the \code{crayon} package,
##'   for example).  Note that the default function will throw an
##'   error on i18next errors, though this only occurs for
##'   interpolation errors from the look of the i18next source.
##'
##' @export
##' @examples
##' # Create a closure that appends to a file
##' file_appender <- function(path) {
##'   function(level, key, data) {
##'     con <- file(path, "a")
##'     on.exit(close(con))
##'     writeLines(data, con)
##'   }
##' }
##'
##' # Then create a logger from this
##' path <- tempfile()
##' traduire::traduire_logger(emit = file_appender(path))
##'
##' # We can then pass that through to the constructor
##' resources <- system.file("examples/simple.json", package = "traduire")
##' obj <- traduire::i18n(resources, logger = logger)
##' obj$t("nonexistant")
##'
##' # Our missed key is present in the file
##' readLines(path)
traduire_logger <- function(transform = NULL, format = NULL, emit = NULL) {
  ## TODO: validation needed here:
  transform <- transform %||% traduire_logger_transform
  format <- format %||% traduire_logger_format
  emit <- emit %||% traduire_logger_emit

  function(level, key, data) {
    data <- transform(level, key, data)
    data <- format(level, key, data)
    emit(level, key, data)
  }
}


## This will be used where logging is disabled, being the fastest way
## of doing nothing.
traduire_logger_silent <- function() {
  traduire_logger(
    transform = function(...) NULL,
    format = function(...) NULL,
    emit = function(...) NULL)
}


## This next bit (the environment, the register and the call function)
## are used to scope a logger to the private environment of an i18n
## object, and coordinate calling them from javascript.  This is
## needed because V8 uses the global environment, not the lexical
## environment, as the environment for the R callbacks.
loggers <- new.env(parent = emptyenv())


logger_register <- function(private, logger) {
  if (is.null(logger)) {
    logger <- traduire_logger()
  } else if (identical(logger, FALSE)) {
    logger <- traduire_logger_silent()
  }
  stopifnot(is.function(logger))
  nm <- env_to_name(private)
  loggers[[nm]] <- logger
  reg.finalizer(private, function(e) rm(list = nm, envir = loggers))
  nm
}


traduire_logger_call <- function(nm, level, key, data) {
  loggers[[nm]](level, key, data)
}


## These are the default logger components
traduire_logger_transform <- function(level, key, data) {
  from_json <- function(x) jsonlite::fromJSON(x, simplifyDataFrame = FALSE)
  if (key == "i18next: initialized") {
    data <- "<configuration options hidden>"
  } else if (key == "i18next::translator: missingKey") {
    data <- from_json(data)
    names(data) <- c("language", "namespace", "key", "returning")
  } else if (key == "i18next: languageChanged") {
    data <- from_json(data)
  }
  data
}


traduire_logger_format <- function(level, key, data) {
  if (length(data) == 0) {
    fmt <- "[%s] %s"
  } else {
    fmt <- "[%s] %s - %s"
  }
  if (!is.null(names(data))) {
    data <- sprintf("%s: %s", names(data), list_to_character(data))
  }
  sprintf(fmt, level, key, paste(data, collapse = ", "))
}


traduire_logger_emit <- function(level, key, data) {
  if (level == "error") {
    stop(data)
  } else {
    message(data)
  }
}
