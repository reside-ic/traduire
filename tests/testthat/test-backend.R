context("backend")

test_that("basic backend loading works", {
  pattern <- file.path(
    traduire_file("examples/structured"),
    "{language}-{namespace}.json")

  obj <- i18n(NULL, debug = FALSE, resource_pattern = pattern)
  expect_equal(obj$t("common:hello"), "hello")
  expect_equal(obj$t("common:hello", language = "fr"), "hello")
  obj$load_namespaces("common")
  expect_equal(obj$t("common:hello", language = "en"), "hello world")
  expect_equal(obj$t("common:hello", language = "fr"), "hello")
  obj$load_languages("fr")
  expect_equal(obj$t("common:hello", language = "en"), "hello world")
  expect_equal(obj$t("common:hello", language = "fr"), "salut le monde")
})
