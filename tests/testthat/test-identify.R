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



test_that("multiple strings on one line", {
  txt <- c('for (i in 1:10) {',
           '  message("a", "b", "c")',
           '}')
  path <- tempfile()
  writeLines(txt, path)
  obj <- R6_file$new(path)

  expect_equal(obj$strings()$line, rep(2, 3))
  expect_equal(obj$strings()$text, c('"a"', '"b"', '"c"'))
  f <- function(x) sprintf("{{%s}}", x)
  expect_equal(
    obj$render(f),
    list(list(lines = 1:3,
              text = c('for (i in 1:10) {',
                       '  message({{"a"}}, {{"b"}}, {{"c"}})',
                       '}'))))
  ids <- obj$strings()$id
  expect_equal(
    obj$render(f, id = ids[[1]]),
    list(list(lines = 1:3,
              text = c(txt[1], '  message({{"a"}}, "b", "c")', txt[3]))))
  expect_equal(
    obj$render(f, id = ids[[2]]),
    list(list(lines = 1:3,
              text = c(txt[1], '  message("a", {{"b"}}, "c")', txt[3]))))
  expect_equal(
    obj$render(f, id = ids[[3]]),
    list(list(lines = 1:3,
              text = c(txt[1], '  message("a", "b", {{"c"}})', txt[3]))))
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
  ids <- obj$strings()$id

  obj$identify(pattern = " ")
  expect_equal(
    obj$strings(),
    data_frame(line = 4:5,
               id = ids[3:4],
               text = c('"sh rt"', '"longer and with spaces"')))
  expect_equal(
    obj$identify(pattern = "[^ ]{10,}")$info()$n_strings, 1)
  expect_equal(
    obj$strings(),
    data_frame(line = 3,
               id = ids[2],
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
  expect_equal(obj$strings()$line, 5)
  expect_equal(obj$strings()$text, '"translate me"')
  obj$identify(ignore = "h")
  expect_equal(obj$strings(),
               data_frame(line = integer(0),
                          id = integer(0),
                          text = character(0)))
})
