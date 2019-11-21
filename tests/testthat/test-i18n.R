context("i18n")

test_that("simple usage works", {
  obj <- i18n(traduire_file("examples/simple.json"))
  expect_equal(obj$t("hello"), "hello world")
  expect_equal(obj$t("hello", language = "en"), "hello world")
  expect_equal(obj$t("hello", language = "fr"), "bonjour le monde")
})


test_that("interpolation", {
  obj <- i18n(traduire_file("examples/simple.json"))
  expect_equal(
    obj$t("interpolate", list(what = "i18next", how = "easy"),
          language = "en"),
    "i18next is easy")
  expect_equal(
    obj$t("interpolate", list(what = "i18next", how = "facile"),
          language = "fr"),
    "i18next est facile")
})


test_that("plurals", {
  obj <- i18n(traduire_file("examples/validation.json"))
  expect_equal(
    obj$t("nocols", list(missing = "A"), count = 1),
    "Data missing column A")
  expect_equal(
    obj$t("nocols", list(missing = "A, B"), count = 2),
    "Data missing columns A, B")

  expect_equal(
    obj$t("nocols", list(missing = "A"), count = 1, language = "fr"),
    "Les données sont manquantes colonne A")
  expect_equal(
    obj$t("nocols", list(missing = "A, B"), count = 2, language = "fr"),
    "Les données sont manquantes colonnes A, B")
})


test_that("default languages", {
  obj <- i18n(traduire_file("examples/simple.json"))
  expect_equal(obj$language(), "en")
  expect_equal(obj$languages(), c("en", "dev"))

  reset <- obj$set_language("fr")
  expect_is(reset, "function")

  expect_equal(obj$language(), "fr")
  expect_equal(obj$languages(), c("fr", "dev"))

  reset()
  expect_equal(obj$language(), "en")
})


test_that("exists", {
  obj <- i18n(traduire_file("examples/simple.json"))
  expect_true(obj$exists("hello"))
  expect_true(obj$exists("hello", language = "fr"))
  expect_false(obj$exists("hello", language = "es"))
  expect_false(obj$exists("goodbye"))
})


test_that("context", {
  resources <- jsonlite::toJSON(
    list(en = list(
           translation =
             list(
               house = "A house",
               house_large = "A mansion",
               house_small = "A cottage",
               house_small_plural = "{{count}} cottages"))),
    auto_unbox = TRUE)
  obj <- i18n(resources)
  expect_equal(obj$t("house"), "A house")
  expect_equal(obj$t("house", context = "large"), "A mansion")
  expect_equal(obj$t("house", context = "small"), "A cottage")
  expect_equal(obj$t("house", context = "small", count = 1), "A cottage")
  expect_equal(obj$t("house", context = "small", count = 3), "3 cottages")
})


test_that("replace", {
  obj <- i18n(traduire_file("examples/simple.json"))
  str <- '{"greeting": "t_(hello)"}'
  expect_equal(obj$replace(str), '{"greeting": "hello world"}')
  obj$set_language("fr")
  expect_equal(obj$replace(str), '{"greeting": "bonjour le monde"}')
  expect_equal(obj$replace(str, language = "en"),
               '{"greeting": "hello world"}')
})


test_that("replace works on multiline strings", {
  obj <- i18n(traduire_file("examples/simple.json"))
  obj$set_language("fr")
  str <- c("{",
           '  "x": "t_(hello)",',
           '  "y": "t_(query)"',
           "}")
  expected <- c("{",
                '  "x": "bonjour le monde",',
                '  "y": "ça va ?"',
                "}")
  expect_equal(obj$replace(str), expected)
})


test_that("replace retains names on multiline strings", {
  obj <- i18n(traduire_file("examples/simple.json"))
  x <- c(a = "t_(hello)", b = "t_(query)")
  y <- c(a = "hello world", b = "how are you?")
  expect_equal(obj$replace(x), y)
  expect_equal(obj$replace(unname(x)), unname(y))
})


test_that("can access keys in different namespaces", {
  ## TODO: t("hello") is not great here if the default namespace is
  ## not set.
  obj <- i18n(traduire_file("examples/namespaces.json"),
              default_namespace = "common")
  expect_equal(obj$t("hello"), "hello world")
  expect_equal(obj$t("common:hello"), "hello world")
  expect_equal(obj$t("login:username"), "Username")
})


test_that("can set default namespace, and reset it later", {
  obj <- i18n(traduire_file("examples/namespaces.json"),
              default_namespace = "common")
  expect_equal(obj$default_namespace(), "common")
  res <- withVisible(
    obj$set_default_namespace("login"))
  expect_is(res$value, "function")
  expect_false(res$visible)
  expect_equal(obj$default_namespace(), "login")
  res$value()
  expect_equal(obj$default_namespace(), "common")
})
