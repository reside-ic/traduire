context("identify")


test_that("simple case", {
  txt <- c('f <- function(a, b) {',
           '  message("Adding a and b")',
           '  a + b',
           '}')
  path <- tempfile()
  writeLines(txt, path)
  obj <- R6_file$new(path)
  expect_equal(
    obj$info(),
    list(path = path, n_lines = 4, n_strings = 1))

  res <- obj$render(function(x) sprintf("{{%s}}", x))
  cmp <- txt
  cmp[[2]] <- '  message({{"Adding a and b"}})'
  expect_equal(res, list(list(lines = 1:4, text = cmp)))
})


test_that("Can identify based on regular expressions", {
  txt <- c('f <- function() {',
    '  list(a = "short",',
    '       b = "longer_but_no_spaces",',
    '       c = "sh rt",',
    '       d = "longer and with spaces")',
    '}')
  path <- tempfile()
  writeLines(txt, path)
  obj <- R6_file$new(path)
  expect_equal(
    obj$info(),
    list(path = path, n_lines = 6, n_strings = 4))

  obj$identify(pattern = " ")
  expect_equal(
    obj$strings(),
    data_frame(line = 4:5,
               text = c('"sh rt"', '"longer and with spaces"')))
  expect_equal(
    obj$identify(pattern = "[^ ]{10,}")$info()$n_strings, 1)
  expect_equal(
    obj$strings(),
    data_frame(line = 3,
               text = '"longer_but_no_spaces"'))
})



test_that("can ignore strings within some functions", {
  txt <- c('local({',
           '  h(if (some_condition) {',
           '      f("this string should be ignored")',
           '    } else {',
           '      g("translate me")',
           '    })',
           '})')
  path <- tempfile()
  writeLines(txt, path)
  obj <- R6_file$new(path, ignore = "f")
  expect_equal(obj$strings(),
               data_frame(line = 5, text = '"translate me"'))
  obj$identify(ignore = "h")
  expect_equal(obj$strings(),
               data_frame(line = integer(0), text = character(0)))
})
