test_that("can translate a code chunk", {
  tr <- traduire::i18n("ex.json", language = "en")
  code <- 'plot(1:10, xlab = t_("time"), ylab = t_("things"))'
  expect_equal(
    rewrite_code(code, tr),
    'plot(1:10, xlab = "Time", ylab = "Things")')
  tr$set_language("fr")
  expect_equal(
    rewrite_code(code, tr),
    'plot(1:10, xlab = "Temps", ylab = "Choses")')
})


test_that("build a translator object from rmarkdown metadata", {
  expect_error(
    markdown_translator(list()),
    "Did not find 'traduire' section in metadata")
  expect_error(
    markdown_translator(list(traduire = NULL)),
    "The 'traduire' section in metadata must have a 'resources' element")
  metadata <- list(traduire = list(resources = "ex.json"))
  tr <- markdown_translator(metadata)
  expect_s3_class(tr, "i18n")
  expect_equal(tr$t("time"), "Time")
})


test_that("Pass additional traduire options", {
  metadata <- list(traduire = list(resources = "ex.json",
                                   options = list(language = "es")))
  expect_equal(
    markdown_translator(metadata)$language(),
    "es")
})


test_that("simple example", {
  tmp <- tempfile()
  on.exit(unlink(tmp, recursive = TRUE))
  dir.create(tmp, FALSE, TRUE)
  file.copy("ex.Rmd", file.path(tmp, "example.Rmd"))
  file.copy("ex.json", tmp)

  res <- withr::with_dir(
    tmp,
    markdown_render_multilingual("example.Rmd", quiet = TRUE))

  expect_equal(names(res), c("en", "fr", "es", "multilingual"))
  expect_true(all(lengths(res) == 1))
  expected <- c(en =  "example-en.html",
                fr =  "example-fr.html",
                es =  "example-es.html",
                multilingual =  "example.html")
  expect_equal(vcapply(res, basename), expected)

  txt_fr <- readLines(file.path(tmp, "example-fr.html"))
  expect_length(grep("Things", txt_fr), 0)
  expect_length(grep("Choses", txt_fr), 1)

  txt_en <- readLines(file.path(tmp, "example-en.html"))
  expect_length(grep("Things", txt_en), 1)
  expect_length(grep("Choses", txt_en), 0)
})
