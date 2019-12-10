context("validate")


test_that("validate_fallback: happy path", {
  expect_equal(validate_fallback(NULL), V8::JS("null"))
  expect_equal(validate_fallback("a"), "a")
  expect_equal(validate_fallback(c("a", "b")), c("a", "b"))
  expect_equal(validate_fallback(c(x = "a", y = "b")), list(x = "a", y = "b"))
  expect_equal(validate_fallback(list(x = "a", y = "b")),
               list(x = "a", y = "b"))
  expect_equal(validate_fallback(list(x = c("a", "b"), y = "b")),
               list(x = c("a", "b"), y = "b"))
})


test_that("validate_fallback: error cases", {
  expect_error(
    validate_fallback(1),
    "fallback must be a character vector or list of character vectors")

  expect_error(
    validate_fallback(c("a", NA)),
    "All fallback entries must be non-NA and non-empty")
  expect_error(
    validate_fallback(c("a", "")),
    "All fallback entries must be non-NA and non-empty")
  expect_error(
    validate_fallback(c(NA, "")),
    "All fallback entries must be non-NA and non-empty")

  expect_error(
    validate_fallback(list("a", "b")),
    "If fallback is a list, it must be named")

  expect_error(
    validate_fallback(list(x = "a", x = "b")),
    "Duplicated names in fallback")
  expect_error(
    validate_fallback(list(x = "a", "b")),
    "Zero-length names in fallback")

  expect_error(
    validate_fallback(list(x = 1, y = 2, z = "en")),
    "All elements of fallback must be character (see 'x', 'y')", fixed = TRUE)

  expect_error(
    validate_fallback(list(x = "", y = NA_character_, z = "en")),
    "All elements of fallback must be non-NA and non-empty (see 'x', 'y')",
    fixed = TRUE)
})
