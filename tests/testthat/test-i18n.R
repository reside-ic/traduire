context("i18n")

test_that("simple usage works", {
  obj <- i18n(traduire_file("examples/simple.json"))
  expect_equal(obj$t("hello"), "hello world")
  expect_equal(obj$t("hello", language = "en"), "hello world")
  expect_equal(obj$t("hello", language = "fr"), "bonjour le monde")
})


test_that("interpolation", {
  obj <- i18n(traduire_file("examples/simple.json"))
  expect_equal(
    obj$t("interpolate", list(what = "i18next", how = "easy"),
          language = "en"),
    "i18next is easy")
  expect_equal(
    obj$t("interpolate", list(what = "i18next", how = "facile"),
          language = "fr"),
    "i18next est facile")
})


test_that("plurals", {
  obj <- i18n(traduire_file("examples/simple.json"))
  expect_equal(obj$t("pluralex1", count = 1), "nose")
  expect_equal(obj$t("pluralex1", count = 2), "noses")
  expect_equal(obj$t("pluralex2", count = 1), "sheep")
  expect_equal(obj$t("pluralex2", count = 2), "sheep")

  expect_equal(obj$t("pluralex1", count = 1, language = "fr"), "nez")
  expect_equal(obj$t("pluralex1", count = 2, language = "fr"), "nez")
  expect_equal(obj$t("pluralex2", count = 1, language = "fr"), "mouton")
  expect_equal(obj$t("pluralex2", count = 2, language = "fr"), "moutons")
})


test_that("default languages", {
  obj <- i18n(traduire_file("examples/simple.json"))
  expect_equal(obj$language(), "en")
  expect_equal(obj$languages(), c("en", "dev"))

  obj$set_language("fr")

  expect_equal(obj$language(), "fr")
  expect_equal(obj$languages(), c("fr", "dev"))
})
