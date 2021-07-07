traduire_rmarkdown_enable <- function(default = TRUE) {
  envir <- parent.frame()
  tr <- traduire_rmarkdown_translator(envir$params, rmarkdown::metadata)
  traduire_knitr_enable(tr, default = default)
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


traduire_knitr_enable <- function(tr, language = NULL, default = TRUE) {
  ## We can access rmarkdown::metadata for this, which might be the
  ## best way of configuring it, really.
  knitr::opts_hooks$set(translate = knitr_rewrite_code(tr, language))
  if (default) {
    knitr::opts_chunk$set(translate = TRUE)
  }
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
