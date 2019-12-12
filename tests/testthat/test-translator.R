context("translator")


test_that("translator register, use, unregister", {
  path <- traduire_file("examples/simple.json")
  name <- rand_str()
  res <- withVisible(translator_register(path, name = name))
  expect_false(res$visible)
  expect_is(res$value, "i18n")
  expect_true(name %in% translator_list())

  expect_identical(translator(name), res$value)
  expect_equal(translator_translate("hello", language = "fr", name = name),
               "bonjour le monde")
  expect_equal(t_("hello", language = "fr", name = name),
               "bonjour le monde")
  expect_true(exists(name, translators))
  translator_unregister(name)
  expect_false(exists(name, translators))
  expect_false(name %in% translator_list())
})


test_that("translator set language", {
  path <- traduire_file("examples/simple.json")
  name <- rand_str()
  translator_register(path, name = name)
  translator_set_language("fr", name)
  expect_equal(translator(name)$language(), "fr")
})


test_that("error if translator not found", {
  name <- rand_str()
  expect_error(translator(name),
               sprintf("Did not find translator '%s'", name))
})


test_that("package from context", {
  skip_if_not_installed("mockery")
  mock_package_name <- mockery::mock("traduire", "traduire", "foo")
  mockery::stub(
    package_from_context,
    "utils::packageName",
    mock_package_name)
  expect_equal(package_from_context(), "foo")
})


test_that("package from context", {
  skip_if_not_installed("mockery")
  mock_package_name <- mockery::mock("traduire", "traduire", "")
  mockery::stub(
    package_from_context,
    "utils::packageName",
    mock_package_name)
  expect_error(package_from_context(), "Did not determine environment name")
})


test_that("validate_package_name requires correct package in strict mode", {
  mock_package_from_context <- mockery::mock("bar", cycle = TRUE)
  mockery::stub(
    validate_package_name,
    "package_from_context",
    mock_package_from_context)
  expect_error(
    validate_package_name("foo", TRUE),
    "Package mismatch - called with bar, called from foo")
  expect_silent(validate_package_name("foo", FALSE))
})


test_that("name_from_context rejects a package: prefix", {
  expect_error(name_from_context("package:whatever", NULL, FALSE),
               "Do not use 'package:' prefix directly")
})


test_that("name_from_context finds name in package", {
  mock_package_from_context <- mockery::mock("pkg", cycle = TRUE)
  mockery::stub(
    name_from_context,
    "package_from_context",
    mock_package_from_context)
  expect_equal(name_from_context("given", NULL, FALSE), "given")
  expect_equal(name_from_context(NULL, NULL, FALSE), "package:pkg")
  expect_equal(name_from_context(NULL, "pkg", FALSE), "package:pkg")
})


test_that("translator get package", {
  id <- "package:impossible_package"
  path <- traduire_file("examples/simple.json")
  translators[[id]] <- i18n(path)
  on.exit(rm(list = id, translators))

  res <- translator(package = "impossible_package")
  expect_identical(res, translators[[id]])
})
