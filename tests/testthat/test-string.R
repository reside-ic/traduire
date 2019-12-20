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


test_that("render", {
  text <- "a <- 1"
  exprs <- parse(text = text, keep.source = TRUE)
  data <- parse_data(exprs)
  m <- Markup$new(text, data)
  i <- data$index[data$token == "LEFT_ASSIGN"]
  m$add("ASSIGN", i)
  tags <- list(ASSIGN = html_span("assign"))
  res <- m$render(tags, filter = FALSE, escape = TRUE)
  expect_equal(res$lines, 1)
  expect_equal(res$text, "a <span class=\"assign\">&lt;-</span> 1")
})


test_that("render - filter group", {
  text <- c("a <- 1", "b = 2")
  exprs <- parse(text = paste(text, collapse = "\n"), keep.source = TRUE)
  data <- parse_data(exprs)
  m <- Markup$new(text, data)

  m$start()
  m$add("ASSIGN", data$index[data$token == "LEFT_ASSIGN"])
  m$start()
  m$add("ASSIGN", data$index[data$token == "EQ_ASSIGN"])

  tags <- list(ASSIGN = html_span("assign"))

  res1 <- m$render(tags, group = 1, filter = TRUE)
  expect_equal(res1$lines, 1)
  expect_equal(res1$text, "a <span class=\"assign\"><-</span> 1")

  res2 <- m$render(tags, filter = FALSE, group = 2)
  expect_equal(res2$lines, 1:2)
  expect_equal(res2$text, c("a <- 1", "b <span class=\"assign\">=</span> 2"))
})
