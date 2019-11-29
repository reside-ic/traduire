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


test_that("name from context", {
  skip_if_not_installed("mockery")
  mock_package_name <- mockery::mock("traduire", "traduire", "foo")
  mockery::stub(
    name_from_context,
    "utils::packageName",
    mock_package_name)
  expect_equal(name_from_context(), "package:foo")
})


test_that("name from context", {
  skip_if_not_installed("mockery")
  mock_package_name <- mockery::mock("traduire", "traduire", "")
  mockery::stub(
    name_from_context,
    "utils::packageName",
    mock_package_name)
  expect_error(name_from_context(), "Did not determine environment name")
})
