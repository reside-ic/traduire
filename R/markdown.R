##' A multilingual version of [rmarkdown::render], which takes an
##' appropriately internationalised rmarkdown file (see Details) and
##' creates output in multiple languages. This differs significantly
##' from [`rmarkdown::render`] in that the knitr document will be
##' compiled several times (once for each language), and there are
##' multiple ways that this can be compiled together (or not)
##' afterwards.
##'
##' There is no single simple way of internationalising markdown,
##' LaTeX or html, though lots of tools exist to help. The approach we
##' take here is simply to localose the input once per language so
##' that `file.Rmd` becomes `file-en.Rmd`, then compile that to (say)
##' `file-en.html`. By having an entirely single-language .Rmd
##' intermediate we can support hopefully all features of
##' rmarkdown. The `markdown_localise` function directly exposes this
##' functionality.  This does mean that if you need to compile Rmds
##' for many different languages, `render` will be called many times
##' and even a relatively quick Rmd will become painful.  You may need
##' to investigate manual caching (e.g., saving and loading rds files)
##' to get around this.
##'
##' @section Creating an internationalised Rmd:
##'
##' Add a configuration section to your frontmatter, something like:
##'
##' ```
##' ---
##' title: t_("title")
##' author: A. N. Author
##' traduire:
##'   resources: "ex.json"
##'   languages: ["en", "fr", "es"]
##' ---
##' ```
##'
##' Here, the title will be localised (looking up the `title`
##'   key). The [`traduire::i18n`] object will be initialised by
##'   loading the resources file `ex.json`, and the document will be,
##'   by default, created in English (`en`), French (`fr`) and
##'   (`es`). In order for that to work, we'll still need to provide
##'   translations for every language!
##'
##' You can omit the `languages` section (or override it) by passing
##'   in a `languages` vector.  This is useful in development for
##'   running a single language.
##'
##' You can omit this entire block if you provide the `tr` and
##'   `languages` arguments to the function.
##'
##' @section Prose:
##'
##' Prose sections (i.e., everything that is outside of the code
##'   chunks) can be internationalised by using a syntax following
##'   Pandoc's "fenced divs".  We can write:
##'
##' ```
##' ::: {lang=en}
##' Plot of random points
##' :::
##'
##' ::: {lang=fr}
##' Tracé de points aléatoires
##' :::
##' ```
##'
##' These can span as many lines as wanted.  When running
##'   `markdown_render_multilingual` for a language, only matching
##'   language codes will be included.
##'
##' @section Code:
##'
##' For internationalising code sections, use the `t_` function
##'   (without namespace prefix)
##'
##' ````
##' ```{r}
##' set.seed(1)
##' plot(runif(10), xlab = t_("time"), ylab = t_("things"))
##' ```
##' ````
##'
##' You do not need to initialise a translation object (via
##'   `traduire::i18n`) as this will be created and configured before
##'   rendering the document. The translations will be applied
##'   *before* evaluation, so that in English (`en`) this might become
##'
##' ````
##' ```{r}
##' set.seed(1)
##' plot(runif(10), xlab = "Time", ylab = "Things")
##' ```
##' ````
##'
##' and then rendered as usual. This ensures that all strings in the
##'   created plots and code chunks are translated appropriately.
##'
##' @title Multilingual markdown
##'
##' @param input The input file to be rendered. This should be an Rmd
##'   document at present, but we may expand to support more options
##'   later.
##'
##' @param ... Additional arguments to pass through to
##'   [rmarkdown::render]
##'
##' @param languages Optionally a vector of languages to render. By
##'   default, all languages in the `traduire:languages` frontmatter
##'   section will be rendered (see Details)
##'
##' @param tr Optionally a translator object. If given, then it
##'   overrides any definition in the metadata.
##'
##' @param combine Logical, indicating if a combined html document
##'   should be created, if possible (this might change).
##'
##' @export
##' @return A list with information about the created files
markdown_render_multilingual <- function(input, ..., languages = NULL,
                                         tr = NULL, combine = NULL) {
  src <- markdown_localise(input, languages, tr)
  ## There are things here that we need to watch out for:
  ##
  ## run_pandoc: if FALSE, we can't stitch things together the same way
  ## output_dir: if given things might be in an unexpected place
  ## output_format: if 'all' or anything other than 'html_document'
  ##   then we can't stitch together
  ## output_yaml: if given (and present) then we'll not detect additional
  ##   changes
  ##
  ## So, not ideal, and I expect a source of considerable corner
  ## cases in future.
  ##
  ## TODO: consider restoring the seed here, at least as an option,
  ## so that we do get more similar renderings for each language.
  ret <- lapply(src, rmarkdown::render, ...)

  ## Check here that we have a single html:
  ret_html <- lapply(ret, grep, pattern = "\\.html$", value = TRUE)
  can_combine <- length(ret_html) > 1 && all(lengths(ret_html) == 1L)
  combine <- combine %||% can_combine
  if (combine) {
    if (!can_combine) {
      stop("Unable to combine")
    }
    filename <- common_filename(ret_html)
    message(sprintf("Combining html output into single file (%s)",
                    basename(filename)))
    ret$multilingual <- markdown_combine(ret_html, filename)
  }

  ret
}


markdown_knit_button <- function(input, ...) {
  markdown_render_multilingual(input, ..., envir = globalenv(),
                               languages = "default", combine = FALSE)
}


##' @export
##' @rdname markdown_render_multilingual
markdown_localise <- function(input, languages = NULL, tr = NULL) {
  dat <- parse_cmark(readLines(input))

  metadata <- rmarkdown::yaml_front_matter(input)
  if (is.null(tr)) {
    tr <- markdown_translator(metadata)
  }

  if (is.null(languages)) {
    ## Infer languages; we should be able to do that from the
    ## resources, but it doesn't make it out to the object
    ## (reside-246)
    languages <- metadata$traduire$languages
  } else if (identical(languages, "default")) {
    languages <- stats::head(metadata$traduire$languages, 1)
  }

  if (length(languages) == 0L) {
    stop("At least one language must be given")
  }

  base <- basename(input)
  dest <- dirname(input)

  ## TODO: should this pattern be customisable? It's actually hard to
  ## specify!  There might a case where we want (say)
  ## `<lang>/<base>.html` as well or `<base>/<lang>.html`, and this
  ## will interact with the combined version for sure.
  fmt <- sprintf("%s-%%s.%s",
                 tools::file_path_sans_ext(base), tools::file_ext(base))
  dir.create(dest, FALSE, TRUE)

  localise1 <- function(lang) {
    tr$set_language(lang)
    filename <- sprintf(fmt, lang)
    if (!is.null(dest)) {
      filename <- file.path(dest, filename)
    }
    writeLines(markdown_localise_data(dat, tr), filename)
    filename
  }

  ret <- vcapply(languages, localise1)
  names(ret) <- languages
  ret
}


parse_cmark <- function(txt) {
  xml <- commonmark::markdown_xml(txt, sourcepos = TRUE)
  dat <- xml2::read_xml(xml)
  nodes <- xml2::xml_children(dat)

  ret <- vector("list", length(nodes))

  for (i in seq_along(nodes)) {
    x <- nodes[[i]]
    name <- xml2::xml_name(x)
    pos <- parse_sourcepos(xml2::xml_attr(x, "sourcepos"))

    type <- switch(name,
                   code_block = "code",
                   heading = "head",
                   "text")

    ## We assert here that this is entire lines
    stopifnot(pos$col1 == 1,
              pos$col2 == nchar(txt[[pos$line2]]))

    this <- list(type = type,
                 from = pos$line1,
                 to = pos$line2,
                 value = txt[pos$line1:pos$line2])

    ## Save the location of "interesting" bits of the node so that if
    ## we do add whitespace on later we can still work with the
    ## contents without any re-parsing.
    if (type == "code") {
      this$body <- seq_along(this$value)[-c(1L, length(this$value))]
    } else if (type == "head") {
      this$body <- seq_len(max(grep("^---\\s*$", this$value)) - 1L)
    }

    ## We don't want to drop whitespace from previous nodes, and we do
    ## want to merge consecutive text blocks (this only makes sense
    ## after the first iteration)
    if (i > 1) {
      prev <- ret[[i - 1]]
      extra <- this$from - prev$to - 1
      if (extra > 0) {
        prev$value <- c(prev$value, rep("", extra))
        prev$to <- prev$to + extra
        ret[[i - 1]] <- prev
      }

      if (type == "text" && prev$type == "text") {
        this$value <- c(prev$value, this$value)
        this$from <- prev$from
        ret[i - 1] <- list(NULL)
      }
    }

    ret[[i]] <- this
  }

  ## NOTE: this does not account for trailing whitespace at the end of
  ## the file, I think that's ok, because that's never wanted.
  ret[!vapply(ret, is.null, logical(1))]
}


parse_sourcepos <- function(p) {
  re <- "^([0-9]+):([0-9]+)-([0-9]+):([0-9]+)$"
  stopifnot(grepl(re, p))
  list(line1 = as.integer(sub(re, "\\1", p)),
       col1 = as.integer(sub(re, "\\2", p)),
       line2 = as.integer(sub(re, "\\3", p)),
       col2 = as.integer(sub(re, "\\4", p)))
}


## Naming below here might change as I do not love it.
markdown_localise_data <- function(dat, tr) {
  ret <- lapply(dat, markdown_localise_node, tr)
  unlist(ret)
}


markdown_localise_node <- function(x, tr) {
  switch(x$type,
         code = markdown_localise_code(x, tr),
         text = markdown_localise_text(x, tr),
         head = markdown_localise_head(x, tr),
         stop("impossible"))
}


markdown_localise_text <- function(x, tr) {
  re_open <- "^:{3,}\\s*([^[:space:]].*?)\\s*$"
  re_close <- "^:{3,}\\s*$"
  re_opts <- "\\{.*\\blang\\s*=\\s*([^ ,]+).*\\}"

  text <- x$value

  i_open <- grepl(re_open, text)
  i_close <- grepl(re_close, text)

  if (sum(i_open) != sum(i_close)) {
    stop("panic")
  }

  if (sum(i_open) == 0) {
    return(text)
  }

  opts <- sub(re_open, "\\1", text[i_open])
  is_lang <- grepl(re_opts, opts, perl = TRUE)
  if (!any(is_lang)) {
    return(text)
  }

  i <- rep(0, length(text))
  i[i_open] <- 1L
  i[i_close] <- -1L
  depth <- cumsum(i)

  find_div <- function(start) {
    valid <- depth[start:length(depth)] >= depth[[start]]
    end <- if (all(valid)) length(depth) else which(!valid)[[1]] + start - 1L
    seq(start, end)
  }

  include <- rep(TRUE, length(text))
  for (i in seq_len(sum(i_open))) {
    keep <- sub(re_opts, "\\1", opts[[i]]) == tr$language()
    j <- find_div(which(i_open)[[i]])
    include[j] <- keep
    ## NOTE: might be a little agressive, but keeps things tidier.
    include[j[c(1, length(j))]] <- FALSE
  }

  ## Retain lines, so that errors report back to the correct line
  ## number in the source document.
  text[!include] <- ""
  text
}


markdown_localise_code <- function(x, tr) {
  ## TODO: localise options too?  Will be needed for at least figure
  ## captions (fig.cap)
  i <- x$body
  x$value[x$body] <- rewrite_code(x$value[x$body], tr)
  x$value
}


markdown_localise_head <- function(x, tr) {
  ## It's not really possible to to this without messing up the yaml,
  ## so we do a basic heuristic:
  re <- "(\\s*.+:\\s+)(t_\\(.+\\))(\\s*)"
  e <- new.env(parent = globalenv())
  e$t_ <- tr$t
  for (i in grep(re, x$value)) {
    s <- sub(re, "\\2", x$value[[i]])
    value <- sprintf('\\1"%s\\3"', eval(parse(text = s), envir = e))
    x$value[[i]] <- sub(re, value, x$value[[i]])
  }
  x$value
}


markdown_combine <- function(files, filename) {
  xpath <- "/html/body/div"
  base <- xml2::read_html(files[[1]])
  base_body <- xml2::xml_find_first(base, xpath)
  base_head <- xml2::xml_find_first(base, "/html/head")

  dat <- lapply(files, xml2::read_html)
  body <- lapply(dat, xml2::xml_find_first, xpath)

  codes <- names(files)
  labels <- codes
  titles <- vcapply(dat, function(x)
    xml2::xml_text(xml2::xml_find_first(x, "/html/head/title")))

  xml2::xml_set_attr(base, "lang", codes[[1L]])
  xml2::xml_add_child(base_head, build_style(codes))
  xml2::xml_add_child(base_head, build_script(codes, titles))
  for (i in rev(seq_along(body))) {
    xml2::xml_set_attr(body[[i]], "lang", codes[[i]])
    xml2::xml_add_sibling(base_body, body[[i]])
  }
  xml2::xml_add_sibling(base_body, build_select(codes, labels))
  xml2::xml_remove(base_body)

  xml2::write_xml(base, filename)
  filename
}


build_select <- function(codes, labels) {
  opts <- Map(function(code, label) structure(list(label), value = code),
              codes, labels)
  names(opts) <- rep("option", length(opts))
  select <- list(select = structure(opts,
                                    name = "select-language",
                                    onchange = "setLanguage(this.value)"))
  xml2::as_xml_document(select)
}


build_style <- function(codes) {
  end <- rep(c(",", " {"), c(length(codes) - 1, 1))
  selector <- sprintf(':lang(%s) body > div[lang="%s"]%s',
                      codes, codes, end)
  style <- c("body > div {",
             "  display: none;",
             "}",
             selector,
             "  display: block;",
             "}")
  xml2::as_xml_document(list(style = list(paste0(style, "\n", collapse = ""))))
}


build_script <- function(codes, titles) {
  script <- c(
    "function setLanguage(lang) {",
    "  var titles = {",
    paste(sprintf('    %s: "%s"', codes, titles), collapse = ",\n"),
    "  };",
    "  document.documentElement.lang=lang;",
    "  document.title = titles[lang];",
    "}")
  xml2::as_xml_document(
    list(script = list(paste0(script, "\n", collapse = ""))))
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


markdown_translator <- function(metadata) {
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

  i18n(metadata$traduire$resources, options = options)
}
