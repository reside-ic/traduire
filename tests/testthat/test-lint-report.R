context("lint report")


test_that("html span", {
  s <- html_span("myclass")
  expect_equal(s(TRUE, NA), '<span class="myclass">')
  expect_equal(s(FALSE, NA), '</span>')
  expect_equal(s(TRUE, "message"),
               '<span class="myclass" data-tooltip="message">')
  expect_equal(s(FALSE, "message"), '</span>')
})


test_that("report for file", {
  src <- c(
    'a <- t_("hello")',
    'b <- t_("missing")')
  p <- tempfile()
  dir.create(p)
  writeLines(src, file.path(p, "a.R"))
  obj <- i18n(traduire_file("examples/simple.json"))
  x <- lint_translations("a.R", obj, root = p)
  res <- lint_translations_html_report_file(x[[1]])

  ## TODO: this needs work to add an id in order that it is embeddable
  expect_setequal(names(res), c("tab", "content"))
  expect_equal(
    res$tab,
    '<button class="tablinks" onclick="openTab(event, \'a.R\')">a.R</button>')

  re <- '^<div id="a.R" class="tabcontent">\n<pre>(.+)</pre>\n</div>\\s*$'
  expect_match(res$content, re)
  code <- sub(re, "\\1", res$content)
  spans <- strsplit(trimws(code), "\n", fixed = TRUE)[[1]]
  expect_equal(
    spans[[1]],
    '<span class="line">a &lt;- <span class="valid"><span class="expr">t_("hello")</span></span></span>')
  expect_equal(
    spans[[2]],
    '<span class="line">b &lt;- <span class="expr">t_(<span class="error-missing" data-tooltip="Translation key \'translation:missing\' not found">"missing"</span>)</span></span>')
})
