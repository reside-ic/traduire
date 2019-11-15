rand_str <- function(n = 10) {
  paste(sample(letters, n, replace = TRUE), collapse = "")
}
