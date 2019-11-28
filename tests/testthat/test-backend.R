context("backend")

test_that("basic backend loading works", {
  pattern <- file.path(
    traduire_file("examples/structured"),
    "{language}-{namespace}.json")

  obj <- i18n(NULL, debug = FALSE, resource_pattern = pattern)
  expect_equal(obj$t("common:hello"), "hello")
  expect_equal(obj$t("common:hello", language = "fr"), "hello")
  obj$load_namespaces("common")
  expect_equal(obj$t("common:hello", language = "en"), "hello world")
  expect_equal(obj$t("common:hello", language = "fr"), "hello")
  obj$load_languages("fr")
  expect_equal(obj$t("common:hello", language = "en"), "hello world")
  expect_equal(obj$t("common:hello", language = "fr"), "salut le monde")
})


test_that("predeclare namespaces", {
  pattern <- file.path(
    traduire_file("examples/structured"),
    "{language}-{namespace}.json")
  obj <- i18n(NULL, debug = FALSE, resource_pattern = pattern,
              default_namespace = "common", namespaces = c("common", "login"))

  expect_equal(obj$t("hello"), "hello world")
  expect_equal(obj$t("common:hello"), "hello world")
  expect_equal(obj$t("login:username"), "Username")

  expect_equal(obj$t("hello", language = "fr"), "hello")
  expect_equal(obj$t("common:hello", language = "fr"), "hello")
  expect_equal(obj$t("login:username", language = "fr"), "username")

  obj$load_languages("fr")

  expect_equal(obj$t("hello", language = "fr"), "salut le monde")
  expect_equal(obj$t("common:hello", language = "fr"), "salut le monde")
  expect_equal(obj$t("login:username", language = "fr"), "Nom d'utilisateur")
})


test_that("predeclare namespaces & languages", {
  pattern <- file.path(
    traduire_file("examples/structured"),
    "{language}-{namespace}.json")
  obj <- i18n(NULL, debug = FALSE, resource_pattern = pattern,
              default_namespace = "common", namespaces = c("common", "login"),
              languages = c("en", "fr"))

  expect_equal(obj$t("hello"), "hello world")
  expect_equal(obj$t("common:hello"), "hello world")
  expect_equal(obj$t("login:username"), "Username")

  expect_equal(obj$t("hello", language = "fr"), "salut le monde")
  expect_equal(obj$t("common:hello", language = "fr"), "salut le monde")
  expect_equal(obj$t("login:username", language = "fr"), "Nom d'utilisateur")
})
