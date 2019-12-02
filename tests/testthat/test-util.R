context("util")

test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})


test_that("sensible errors on missing files", {
  input <- tempfile(fileext = ".json")
  expect_error(read_input(input),
               "'input' looks like a filename but does not exist")
})


test_that("read_input passes json through", {
  dat <- '{"a" : "b"}'
  expect_equal(read_input(dat), jsonlite::unbox(dat))
})


test_that("data_frame does not let factors through", {
  a <- letters
  b <- runif(26)
  expect_is(data_frame(first = a, second = b)$first, "character")
  expect_identical(
    data_frame(a, b),
    data.frame(a, b, stringsAsFactors = FALSE))
})
