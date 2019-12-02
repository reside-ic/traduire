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
