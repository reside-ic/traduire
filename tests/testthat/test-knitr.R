context("knitr")

test_that("can translate a code chunk", {
  tr <- traduire::i18n("ex.json", language = "en")
  code <- 'plot(1:10, xlab = t_("time"), ylab = t_("things"))'
  expect_equal(
    rewrite_code(code, tr),
    'plot(1:10, xlab = "Time", ylab = "Things")')
  tr$set_language("fr")
  expect_equal(
    rewrite_code(code, tr),
    'plot(1:10, xlab = "Temps", ylab = "Choses")')
})


test_that("build a translator object from rmarkdown metadata", {
  expect_error(
    traduire_rmarkdown_translator(list(), list()),
    "Did not find 'traduire' section in metadata")
  expect_error(
    traduire_rmarkdown_translator(list(), list(traduire = NULL)),
    "The 'traduire' section in metadata must have a 'resources' element")
  metadata <- list(traduire = list(resources = "ex.json"))
  tr <- traduire_rmarkdown_translator(list(), metadata)
  expect_s3_class(tr, "i18n")
  expect_equal(tr$t("time"), "Time")
})


test_that("Can set language via parameters", {
  metadata <- list(traduire = list(resources = "ex.json"))
  tr <- traduire_rmarkdown_translator(list(language = "fr"), metadata)
  expect_equal(tr$language(), "fr")
})


test_that("Fall back to set language via metadata", {
  metadata <- list(traduire = list(resources = "ex.json", language = "es"))
  expect_equal(
    traduire_rmarkdown_translator(list(language = "fr"), metadata)$language(),
    "fr")
  expect_equal(
    traduire_rmarkdown_translator(list(), metadata)$language(),
    "es")
})


test_that("Pass additional traduire options", {
  metadata <- list(traduire = list(resources = "ex.json",
                                   options = list(language = "es")))
  expect_equal(
    traduire_rmarkdown_translator(list(), metadata)$language(),
    "es")
})


test_that("generate sensible css", {
  expected <- c("<style>",
                "#translate {",
                "  display: none;", "}",
                "#translate[language=fr] {",
                "display: block;", "}",
                "</style>")
  expect_equal(knitr_prose_css("fr"), expected)
})
