context("lint report")


test_that("html span", {
  s <- html_span("myclass")
  expect_equal(s(TRUE, NA), '<span class="myclass">')
  expect_equal(s(FALSE, NA), '</span>')
  expect_equal(s(TRUE, "message"),
               '<span class="myclass" data-tooltip="message">')
  expect_equal(s(FALSE, "message"), '</span>')
})
