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


test_that("expand_paths expands paths", {
  expect_setequal(
    expand_paths(traduire_file("hello/R")),
    c(traduire_file("hello/R/api.R"),
      traduire_file("hello/R/hello.R")))
})


test_that("browse_html", {
  code <- "hello world"
  mock_browser <- mockery::mock()
  mockery::stub(
    browse_html,
    "utils::browseURL",
    mock_browser)
  res <- withVisible(browse_html(code))
  expect_false(res$visible)
  expect_true(file.exists(res$value))
  expect_equal(readLines(res$value), code)
  mockery::expect_called(mock_browser, 1)
  mockery::expect_call(mock_browser, 1, utils::browseURL(file))
})
