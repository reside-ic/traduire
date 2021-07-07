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
