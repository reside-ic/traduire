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
  expect_null(res[[1]]$usage[[1]]$interpolation)

  expect_null(res[[1]]$usage[[2]]$key$namespace)
  expect_equal(res[[1]]$usage[[2]]$key$key, "missing")
  expect_null(res[[1]]$usage[[2]]$key$value)
  expect_equal(res[[1]]$usage[[2]]$key$namespace_computed, "translation")
  expect_false(res[[1]]$usage[[2]]$key$exists)
  expect_null(res[[1]]$usage[[2]]$interpolation)
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
               list(a = 5:6, b = 10:11, c = 13:14))
  expect_equal(parse_data_match_call(i, data, function(x, y, b) {}),
               list(x = 5:6, y = 13:14, b = 10:11))
})
