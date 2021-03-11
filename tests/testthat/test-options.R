context("options")

test_that("options can be built from defaults", {
  opts <- traduire_options()
  expect_equivalent(opts,
                    list(language = "en",
                         default_namespace = NULL,
                         debug = FALSE,
                         resource_pattern = NULL,
                         namespaces = "translation",
                         languages = NULL,
                         fallback = "dev",
                         escape = FALSE))
})

test_that("options can be individually set", {
  opts <- traduire_options(language = "fr",
                           default_namespace = "tr",
                           debug = TRUE,
                           resource_pattern = "pattern",
                           namespaces = c("tr", "tr2"),
                           languages = c("en", "fr"),
                           fallback = "test",
                           escape = TRUE)
  expect_equivalent(opts,
                    list(language = "fr",
                         default_namespace = "tr",
                         debug = TRUE,
                         resource_pattern = "pattern",
                         namespaces = c("tr", "tr2"),
                         languages = c("en", "fr"),
                         fallback = "test",
                         escape = TRUE))
})

test_that("options are validated correctly", {
  expect_error(traduire_options("opt"), "'options' must be named")
  expect_error(traduire_options(options = list("opt1" = "test")),
               "Options must be of type 'traduire_options'")
  
  expect_error(traduire_options(language = list("1", "2")),
               "'language' must be a scalar")
  expect_error(traduire_options(default_namespace = list("1", "2")),
               "'default_namespace' must be a scalar")
  expect_error(traduire_options(debug = "true"),
               "'debug' must be logical")
  expect_error(traduire_options(resource_pattern = 1),
               "'resource_pattern' must be character")
  expect_error(traduire_options(namespaces = 1),
               "'namespaces' must be character")
  expect_error(traduire_options(languages = 1),
               "'languages' must be character")
  expect_error(
    traduire_options(fallback = 1),
    "fallback must be a character vector or list of character vectors")
  expect_error(traduire_options(escape = 1),
               "'escape' must be logical")
  
})

test_that("options object can be used", {
  input_opts <- traduire_options()
  expect_identical(traduire_options(options = input_opts), input_opts)
  
  opts <- traduire_options(escape = TRUE, options = input_opts)
  expect_equivalent(opts, list(language = "en",
                               default_namespace = NULL,
                               debug = FALSE,
                               resource_pattern = NULL,
                               namespaces = "translation",
                               languages = NULL,
                               fallback = "dev",
                               escape = TRUE))
  
  ## Overriden options are validated
  expect_error(traduire_options(escape = 1, options = input_opts),
               "'escape' must be logical")
  expect_error(traduire_options(debug = NULL, options = input_opts),
               "'debug' must be a scalar")
})
