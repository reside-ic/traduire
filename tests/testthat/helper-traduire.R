rand_str <- function(n = 10) {
  paste(sample(letters, n, replace = TRUE), collapse = "")
}


with_options <- function(new, expr) {
  old <- options(new)
  on.exit(options(old))
  force(expr)
}

expect_no_error <- function(object) {
  expect_error(object, NA)
}
