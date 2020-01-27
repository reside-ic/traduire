context("lint")

test_that("existing and missing key", {
  src <- c(
    'a <- t_("hello")',
    'b <- t_("missing")')
  p <- tempfile(fileext = ".R")
  writeLines(src, p)
  obj <- i18n(traduire_file("examples/simple.json"))
  res <- lint_translations(p, obj)
  expect_is(res, "lint_translations")
  expect_equal(res[[1]]$path, p)
  expect_equal(res[[1]]$text, src)
  expect_is(res[[1]]$data, "data.frame")
  expect_is(res[[1]]$usage, "list")
  expect_equal(length(res[[1]]$usage), 2)

  expect_null(res[[1]]$usage[[1]]$key$namespace)
  expect_equal(res[[1]]$usage[[1]]$key$key, "hello")
  expect_equal(res[[1]]$usage[[1]]$key$value, "hello world")
  expect_equal(res[[1]]$usage[[1]]$key$namespace_computed, "translation")
  expect_true(res[[1]]$usage[[1]]$key$exists)
  expect_true(res[[1]]$usage[[1]]$interpolation$valid)

  expect_null(res[[1]]$usage[[2]]$key$namespace)
  expect_equal(res[[1]]$usage[[2]]$key$key, "missing")
  expect_null(res[[1]]$usage[[2]]$key$value)
  expect_equal(res[[1]]$usage[[2]]$key$namespace_computed, "translation")
  expect_false(res[[1]]$usage[[2]]$key$exists)
  expect_true(res[[1]]$usage[[1]]$interpolation$valid)

  s <- res[[1]]$info$summary()
  expect_is(s, "matrix")
  expect_equal(nrow(s), 2)
  expect_equal(ncol(s), length(lint_tags_names()))
  expect_equal(s[, "EXPR"], c(TRUE, TRUE))
  expect_equal(s[, "VALID"], c(TRUE, FALSE))
  expect_equal(s[, "MISSING_KEY"], c(FALSE, TRUE))
  expect_false(any(s[, !(colnames(s) %in% c("EXPR", "VALID", "MISSING_KEY"))]))
})


test_that("read interpolation data", {
  src <- c(
    'a <- t_("interpolate", list(what = "thing", how = "done"))',
    'b <- t_("interpolate", list(what = "thing", err = "done"))')
  p <- tempfile(fileext = ".R")
  writeLines(src, p)
  obj <- i18n(traduire_file("examples/simple.json"))
  res <- lint_translations(p, obj)

  expect_is(res, "lint_translations")
  expect_equal(res[[1]]$path, p)
  expect_equal(res[[1]]$text, src)
  expect_is(res[[1]]$data, "data.frame")
  expect_is(res[[1]]$usage, "list")
  expect_equal(length(res[[1]]$usage), 2)

  expect_null(res[[1]]$usage[[1]]$key$namespace)
  expect_equal(res[[1]]$usage[[1]]$key$key, "interpolate")
  expect_equal(res[[1]]$usage[[1]]$key$value, "{{what}} is {{how}}")
  expect_equal(res[[1]]$usage[[1]]$key$namespace_computed, "translation")
  expect_true(res[[1]]$usage[[1]]$key$exists)
  expect_equal(res[[1]]$usage[[1]]$interpolation$data,
               list(what = list(name = 16L, value = 18:19),
                    how = list(name = 21L, value = 23:24)))
  expect_equal(res[[1]]$usage[[1]]$interpolation$missing, character(0))
  expect_equal(res[[1]]$usage[[1]]$interpolation$unused, character(0))
  expect_true(res[[1]]$usage[[1]]$interpolation$valid)

  expect_null(res[[1]]$usage[[2]]$key$namespace)
  expect_equal(res[[1]]$usage[[2]]$key$key, "interpolate")
  expect_equal(res[[1]]$usage[[2]]$key$value, "{{what}} is {{how}}")
  expect_equal(res[[1]]$usage[[2]]$key$namespace_computed, "translation")
  expect_true(res[[1]]$usage[[2]]$key$exists)
  expect_equal(res[[1]]$usage[[2]]$interpolation$data,
               list(what = list(name = 42L, value = 44:45),
                    err = list(name = 47L, value = 49:50)))
  expect_equal(res[[1]]$usage[[2]]$interpolation$missing, "how")
  expect_equal(res[[1]]$usage[[2]]$interpolation$unused, "err")
  expect_false(res[[1]]$usage[[2]]$interpolation$valid)
})


test_that("Detect namespaced keys", {
  src <- c(
    'a <- t_("hello")',
    'b <- t_("common:hello")',
    'c <- t_("username")',
    'd <- t_("login:username")')
  p <- tempfile(fileext = ".R")
  writeLines(src, p)
  obj <- i18n(traduire_file("examples/namespaces.json"),
              default_namespace = "common")
  res <- lint_translations(p, obj)

  expect_true(res[[1]]$usage[[1]]$key$exists)
  expect_null(res[[1]]$usage[[1]]$key$namespace)
  expect_equal(res[[1]]$usage[[1]]$key$namespace_computed, "common")

  expect_true(res[[1]]$usage[[2]]$key$exists)
  expect_equal(res[[1]]$usage[[2]]$key$namespace, "common")
  expect_equal(res[[1]]$usage[[2]]$key$namespace_computed, "common")

  expect_false(res[[1]]$usage[[3]]$key$exists)
  expect_null(res[[1]]$usage[[3]]$key$namespace)
  expect_equal(res[[1]]$usage[[3]]$key$namespace_computed, "common")

  expect_true(res[[1]]$usage[[4]]$key$exists)
  expect_equal(res[[1]]$usage[[4]]$key$namespace, "login")
  expect_equal(res[[1]]$usage[[4]]$key$namespace_computed, "login")
})


test_that("detect escaped interpolation fields", {
  translations <- list(
    "en" = list(
      "translation" = list(
        "escape" = "the {{code}}, escaped",
        "noescape" = "the raw {{- code}} with no escape")))
  json <- tempfile()
  writeLines(
    jsonlite::toJSON(translations, auto_unbox = TRUE),
    json)
  obj <- i18n(json, escape = TRUE)

  src <- c(
    'a <- t_("escape", list(code = "mycode"))',
    'b <- t_("noescape", list(code = "mycode"))')
  p <- tempfile(fileext = ".R")
  writeLines(src, p)

  ## TODO: we should expose the file-level linting more here to simplify this
  res <- lint_translations(p, obj)
  expect_equal(res[[1]]$usage[[1]]$interpolation$fields$text, "code")
  expect_equal(res[[1]]$usage[[1]]$interpolation$fields$from, 5)
  expect_equal(res[[1]]$usage[[1]]$interpolation$fields$to, 12)

  expect_equal(res[[1]]$usage[[2]]$interpolation$fields$text, "code")
  expect_equal(res[[1]]$usage[[2]]$interpolation$fields$from, 9)
  expect_equal(res[[1]]$usage[[2]]$interpolation$fields$to, 18)
})


test_that("behaviour when symbol used for key", {
  src <- c(
    'k <- "hello"',
    'a <- t_("hello")',
    'b <- t_(k)')
  p <- tempfile(fileext = ".R")
  writeLines(src, p)
  obj <- i18n(traduire_file("examples/simple.json"))
  res <- lint_translations(p, obj)

  expect_true(res[[1]]$usage[[1]]$key$exists)

  expect_identical(res[[1]]$usage[[2]]$key$exists, NA)
  expect_identical(res[[1]]$usage[[2]]$key$key, NA_character_)
  expect_identical(res[[1]]$usage[[2]]$key$value, NA_character_)
  expect_identical(res[[1]]$usage[[2]]$key$namespace_computed, NA_character_)

  expect_equal(res[[1]]$info$spans[[7]]$tag, "UNKNOWN_KEY")
  expect_equal(res[[1]]$info$spans[[7]]$msg,
               "Translation key could not be determined without string")
})


test_that("behaviour when using symbol for interpolation data", {
  src <- c(
    'data <- list(what = "thing", how = "done")',
    'a <- t_("interpolate", list(what = "thing", how = "done"))',
    'b <- t_("interpolate", data)')
  p <- tempfile(fileext = ".R")
  writeLines(src, p)
  obj <- i18n(traduire_file("examples/simple.json"))
  res <- lint_translations(p, obj)

  expect_true(res[[1]]$usage[[1]]$interpolation$valid)
  expect_true(res[[1]]$usage[[1]]$interpolation$found)

  expect_identical(res[[1]]$usage[[2]]$interpolation$valid, NA)
  expect_false(res[[1]]$usage[[2]]$interpolation$found)
  expect_identical(res[[1]]$usage[[2]]$interpolation$fields$text,
                   c("what", "how"))

  expect_equal(res[[1]]$info$spans[[7]]$tag, "UNKNOWN_DATA")
  expect_equal(res[[1]]$info$spans[[7]]$msg,
               "Interpolation data could not be determined")
})


test_that("directory wrangling", {
  src1 <- c(
    'a <- t_("hello")',
    'b <- t_("missing")')
  src2 <- c('message(a)')
  p <- tempfile()
  dir.create(file.path(p, "R"), FALSE, TRUE)
  writeLines(src1, file.path(p, "R", "a.R"))
  writeLines(src2, file.path(p, "R", "b.R"))

  obj <- i18n(traduire_file("examples/simple.json"))
  res <- lint_translations("R", obj, root = p)

  expect_equal(res[[1]]$path, "R/a.R")
  expect_equal(res[[2]]$path, "R/b.R")
  expect_equal(length(res[[1]]$usage), 2)
  expect_equal(length(res[[2]]$usage), 0)
})


test_that("lint tags", {
  expect_error(
    lint_tags(list(EXPR = html_span("whatever"))),
    "Invalid tag names")
})
