##' Configure an "rmarkdown" document to be translatable. This
##' configures a set of hooks and styling which will allow both code
##' chunks and prose to be translated/internationalised. It must be
##' called from within an Rmarkdown document
##'
##' To use, first configure traduire by adding appropriate elements to
##' your yaml frontmatter. For example
##'
##' \preformatted{
##' ---
##' params:
##'   language: fr
##' traduire:
##'   resources: "resources.json"
##' ---
##' }
##'
##' (among any other bits of frontmatter) would configure traduire to
##' use the resources file \code{resources.json} and set the default
##' language to be \code{fr}, which will be overrideable by passing in a
##' parameter to rmarkdown via its \code{params} argument.
##'
##'
##' Then, early in the document (before anything that needs
##' internationalisation) create a code block like:
##'
##' \preformatted{
##' ```{r, echo = FALSE, results = "asis"}
##' traduire::traduire_rmarkdown_enable()
##' ```
##' }
##'
##' It is important to include the `results = "asis"` part as this
##' will emit some CSS which enables translation of prose.
##'
##' After this, you can internationalise any code block, for example:
##'
##' \preformatted{
##' ```{r}
##' plot(runif(10), xlab = t_("time"), ylab = t_("things"))
##' ```
##' }
##'
##' With this code block, `traduire` will replace the keys \code{time}
##' and \code{things} with values from your resource file for the
##' corresponding file. Importantly, this will happen *before* the
##' code chunk is evaluated so that both the echo'd code and the
##' output will be internationalised.
##'
##' Prose blocks do require more direct interaction, and one might write:
##'
##' \preformatted{
##' ::: {#translate language="en"}
##' Plot of random points
##' :::
##'
##' ::: {#translate language="fr"}
##' Tracé de points aléatoires
##' :::
##' }
##'
##' which will render based on the language setting.
##'
##' @title Rmarkdown support
##'
##' @return Nothing, called for its side effect within an Rmarkdown
##'   document.
##' @export
traduire_rmarkdown_enable <- function() {
  envir <- parent.frame()
  tr <- traduire_rmarkdown_translator(envir$params, rmarkdown::metadata)
  traduire_knitr_enable(tr)
  ## TODO: Only do this for an html output; for latex etc we want to
  ## do something slightly different, but not sure what. We should be
  ## able to detect this.
  writeLines(knitr_prose_css(envir$params$language))
}


traduire_rmarkdown_translator <- function(params, metadata) {
  if (!("traduire" %in% names(metadata))) {
    stop("Did not find 'traduire' section in metadata")
  }
  if (is.null(metadata$traduire$resources)) {
    stop("The 'traduire' section in metadata must have a 'resources' element")
  }
  if (is.null(metadata$traduire$options)) {
    options <- NULL
  } else {
    options <- do.call(traduire_options, metadata$traduire$options)
  }

  tr <- i18n(metadata$traduire$resources, options = options)

  language <- params$language %||% metadata$traduire$language
  if (!is.null(language)) {
    tr$set_language(language)
  }

  tr
}


## TODO: work on inline chunks too?
traduire_knitr_enable <- function(tr, language = NULL) {
  knitr::opts_hooks$set(translate = knitr_rewrite_code(tr, language))
  knitr::opts_chunk$set(translate = TRUE)
}


knitr_rewrite_code <- function(tr, language) {
  force(tr)
  force(language)
  function(options) {
    if (!is.null(language)) {
      reset_language <- tr$set_language(language)
      on.exit(reset_language())
    }
    options$code <- rewrite_code(options$code, tr)
    options
  }
}


knitr_prose_css <- function(language) {
  if (is.null(language)) {
    character(0)
  } else {
    c("<style>",
      "#translate {",
      "  display: none;",
      "}",
      sprintf("#translate[language=%s] {", language),
      "display: block;",
    "}",
    "</style>")
  }
}


## There are two obvious ways of doing this:
##
## 1. If we have the code as an expression object we can recurse
##    through and find all calls to `t_` and evaluate them. However,
##    this is not ideal because then we break the contract with knitr
##    about retaining formatting.  We also can't nicely cope with
##    comments. The translations also can't cope with access to the
##    environment, so all variable lookups will be hard. Will see if
##    we can relax that later...
##
## 2. If we use the parse tree, as we do with the linting we can edit
##    the source code directly, as a string. We'll try and do
##    that. Getting comments working is nontrivial but we can cope
##    with that later.
rewrite_code <- function(code, tr, envir = globalenv()) {
  exprs <- parse(text = code, keep.source = TRUE)

  data <- utils::getParseData(exprs)
  data$depth <- cumsum((data$token == "'('") - (data$token == "')'"))
  data$index <- seq_len(nrow(data))
  idx <- data$token == "SYMBOL_FUNCTION_CALL" & data$text == "t_"
  dat <- lapply(which(idx), parse_data_find_call, data)

  e <- new.env(parent = envir)
  e$t_ <- tr$t

  ## Then some string substitutions back into the code; must be done
  ## in reverse though.
  for (i in rev(seq_along(dat))) {
    x <- dat[[i]]
    input <- str_extract_chunk(code, x$start, x$end)
    value <- sprintf('"%s"', eval(parse(text = input), envir = e))
    code <- str_swap_chunk(code, x$start, x$end, value)
  }

  code
}
