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
    '<span class="line"></span>a &lt;- <span class="valid"><span class="expr">t_("hello")</span></span>')
  expect_equal(
    spans[[2]],
    '<span class="line"></span>b &lt;- <span class="expr">t_(<span class="error-missing" data-tooltip="Translation key \'translation:missing\' not found">"missing"</span>)</span>')
})


test_that("complete report", {
  src <- c(
    'a <- t_("hello")',
    'b <- t_("missing")')
  p <- tempfile()
  dir.create(p)
  writeLines(src, file.path(p, "a.R"))
  obj <- i18n(traduire_file("examples/simple.json"))
  x <- lint_translations("a.R", obj, root = p, title = "mytitle")
  html <- lint_translations_html_report(x)
  expect_match(html, "^<html")

  mock_browser <- mockery::mock()
  path <- withr::with_options(
    list(browser = mock_browser),
    lint_translations_report(x))
  expect_true(file.exists(path))
  mockery::expect_called(mock_browser, 1)
  expect_equal(mockery::mock_args(mock_browser),
               list(list(path)))
})


test_that("lint_translations_package", {
  skip_if_not_installed("mockery")
  root <- traduire_file("hello")
  obj <- i18n(file.path(root, "inst/traduire.json"), "en")
  mock_translator <- mockery::mock(obj)
  mock_load <- mockery::mock()
  mockery::stub(lint_translations_package, "translator", mock_translator)
  mockery::stub(lint_translations_package, "loadNamespace", mock_load)
  res <- lint_translations_package(root)
  cmp <- lint_translations("R", obj, root = root, title = "{hello}")
  expect_equal(
    lint_translations_html_report(res),
    lint_translations_html_report(cmp))
})


test_that("lint_translations_package corner cases", {
  path <- tempfile()
  expect_error(
    lint_translations_package(path),
    "Did not find DESCRIPTION at '.+'")
  dir.create(path)
  writeLines("Foo: bar", file.path(path, "DESCRIPTION"))
  expect_error(
    lint_translations_package(path),
    "Invalid DESCRIPTION")
})
