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


test_that("parse data match call", {
  ## TODO: this is probably a bit implementation dependent. We should
  ## compute the positions of these characters rather than encoding
  ## them here.  Marked as skip_on_cran to indicate that it's too
  ## implementation dependent.
  skip_on_cran()
  data <- parse_data(parse(text = "f('a', b = 2, 'c')", keep.source = TRUE))
  i <- which(data$text == "f")[[1]]
  expect_equal(parse_data_match_call(i, data, function(a, b, c) {}),
               list(a = list(name = NULL, value = 5:6),
                    b = list(name = 8, value = 10:11),
                    c = list(name = NULL, value = 13:14)))
  expect_equal(parse_data_match_call(i, data, function(x, y, b) {}),
               list(x = list(name = NULL, value = 5:6),
                    y = list(name = NULL, value = 13:14),
                    b = list(name = 8, value = 10:11)))
})