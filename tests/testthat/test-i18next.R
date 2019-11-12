context("i18next")

test_that("simple usage works", {
  obj <- i18n(i18n_file("examples/simple.json"))
  expect_equal(obj$t("hello"), "hello world")
  expect_equal(obj$t("hello", language = "en"), "hello world")
  expect_equal(obj$t("hello", language = "fr"), "bonjour le monde")
})
