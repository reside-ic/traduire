lint_translations_html_report <- function(x, title) {
  stopifnot(inherits(x, "lint_translations"))

  style <- read_file(traduire_file("report/style.css"))
  script <- read_file(traduire_file("report/script.js"))
  template <- read_file(traduire_file("report/template.html"))

  res <- lapply(x, lint_translations_html_report_file)
  tabs <- paste(vcapply(res, "[[", "tab"), collapse = "\n")
  content <- paste(vcapply(res, "[[", "content"), collapse = "\n\n")

  d <- list(title = title, style = style, script = script,
            tabs = tabs, content = content)

  unclass(glue::glue(template, .envir = d))
}


lint_translations_html_report_file <- function(x) {
  tags <- list(
    EXPR = html_span("expr"),
    VALID = html_span("valid"),
    MISSING_KEY = html_span("error-missing"),
    INTERPOLATION_UNUSED = html_span("error-interpolation-unused"),
    INTERPOLATION_MISSING = html_span("error-interpolation-missing"))
  ans <- x$info$render(tags, escape = TRUE, filter = FALSE)
  code <- sprintf('<span class="line">%s</span>', ans$text)
  fmt <- '<div id="%s" class="tabcontent">\n<pre>\n%s\n</pre>\n</div>'
  code <- sprintf('<span class="line">%s</span>', ans$text)
  tab <- sprintf(
    '<button class="tablinks" onclick="openTab(event, \'%s\')">%s</button>',
    x$path, x$path)
  content <- sprintf(fmt, x$path, paste(code, collapse = "\n"))
  list(tab = tab, content = content)
}


html_span <- function(class) {
  force(class)
  function(open, msg) {
    if (open) {
      if (is.na(msg)) {
        sprintf('<span class="%s">', class)
      } else {
        sprintf('<span class="%s" data-tooltip="%s">', class, msg)
      }
    } else {
      "</span>"
    }
  }
}