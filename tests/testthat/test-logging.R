context("logging")


test_that("default logger", {
  logger <- traduire_logger()
  expect_is(logger, "function")

  expect_message(
    logger("info", "key", '["value"]'),
    "[info] key - value", fixed = TRUE)
  expect_message(
    logger("warn", "key", '["value"]'),
    "[warn] key - value", fixed = TRUE)
  expect_error(
    logger("error", "key", '["value"]'),
    "[error] key - value", fixed = TRUE)
})


test_that("default format with data arguments", {
  expect_equal(
    traduire_logger_format("level", "key", "data"),
    "[level] key - data")
  expect_equal(
    traduire_logger_format("level", "key", NULL),
    "[level] key")
  expect_equal(
    traduire_logger_format("level", "key", c("a", "b", "c")),
    "[level] key - a, b, c")
  expect_equal(
    traduire_logger_format("level", "key", c(x = "a", y = "b", z = "c")),
    "[level] key - x: a, y: b, z: c")
})


test_that("default emit", {
  msg <- "a message"
  expect_message(
    traduire_logger_emit("info", "key", msg),
    msg)
  expect_message(
    traduire_logger_emit("warn", "key", msg),
    msg)
  expect_error(
    traduire_logger_emit("error", "key", msg),
    msg)
})


test_that("default transform", {
  expect_equal(
    traduire_logger_transform("info", "i18next: initialized", "[some json]"),
    "<configuration options hidden>")
  expect_equal(
    traduire_logger_transform("info", "i18next::translator: missingKey",
                              '["a", "b", "c", "d"]'),
    list(language = "a", namespace = "b", key = "c", returning = "d"))
  expect_equal(
    traduire_logger_transform("info", "i18next: languageChanged", '["en"]'),
    "en")
})


test_that("create scoped translators", {
  e <- new.env(parent = emptyenv())
  logger <- traduire_logger()
  nm <- logger_register(e, logger)
  expect_match(nm, "^env")
  expect_true(exists(nm, loggers, inherits = FALSE))
  expect_identical(loggers[[nm]], logger)
  expect_message(
    traduire_logger_call(nm, "level", "key", '["value"]'),
    "[level] key - value", fixed = TRUE)
  ## Once the environment disappears, the logger is cleaned up
  rm(e)
  gc()
  expect_false(exists(nm, loggers, inherits = FALSE))
})


test_that("validate logger - default", {
  logger <- logger_validate(NULL)
  expect_message(
    logger("level", "key", '["value"]'),
    "[level] key - value", fixed = TRUE)
})


test_that("validate logger - disabled", {
  logger <- logger_validate(FALSE)
  expect_silent(
    logger("level", "key", '["value"]'))
})


test_that("validate logger - custom", {
  logger <- function(level, key, data) message("the log")
  expect_identical(logger_validate(logger), logger)
})


test_that("integrate logging into object", {
  file_appender <- function(path) {
    function(level, key, data) {
      con <- file(path, "a")
      on.exit(close(con))
      writeLines(data, con)
    }
  }

  path <- tempfile()
  logger <- traduire_logger(emit = file_appender(path))
  resources <- traduire_file("examples/simple.json")
  obj <- traduire::i18n(resources, debug = TRUE, logger = logger)
  obj$t("nonexistant")
  lines <- readLines(path)

  expect_match(
    lines[[length(lines)]],
    "i18next::translator: missingKey - language: en, namespace: translation",
    fixed = TRUE)
})
