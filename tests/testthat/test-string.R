context("string")


test_that("glue_extract", {
  expect_equal(
    glue_extract("a", "{", "}"),
    data_frame(text = character(), from = numeric(), to = numeric()))

  expect_equal(
    glue_extract("my {glue} message", "{", "}"),
    data_frame(text = "glue", from = 4, to = 9))

  expect_equal(
    glue_extract("{my} {glue} {message}", "{", "}"),
    data_frame(text = c("my", "glue", "message"),
               from = c(1, 6, 13),
               to = c(4, 11, 21)))

  expect_equal(
    glue_extract("{my} {{glue}} {message}", "{{", "}}"),
    data_frame(text = "glue", from = 6, to = 13))
})


test_that("str_find", {
  expect_equal(str_find("silly", "my silly silly string", 0), c(4, 8))
  expect_equal(str_find("silly", "my silly silly string", 8), c(10, 14))
  expect_error(str_find("silly", "my silly silly string", 14),
               "Failed match!")
})


test_that("str_insert", {
  expect_equal(str_insert("my string", 3, " amazing"), "my amazing string")
})


test_that("html escape", {
  ##         123356
  text <- c("a <- 1",
            "b = 2")
  col <- c(1, 3, 6, 1, 3, 5)
  line <- rep(1:2, each = 3)
  open <- rep(TRUE, 6)

  expect_equal(html_escape(text, line, col, open),
               list(text = c("a &lt;- 1", "b = 2"),
                    col = c(1, 3, 9, 1, 3, 5)))
  expect_equal(html_escape(text, line, col, rep(c(TRUE, FALSE), c(1, 5))),
               list(text = c("a &lt;- 1", "b = 2"),
                    col = c(1, 6, 9, 1, 3, 5)))
})
