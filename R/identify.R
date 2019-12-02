R6_file <- R6::R6Class(
  "file",

  private = list(
    path = NULL,
    ignore = NULL,
    pattern = NULL,

    text = NULL,
    parse_data = NULL,

    strings = NULL,
    data = NULL,

    reload = function() {
      private$text <- readLines(private$path)

      exprs <- parse(file = private$path, keep.source = TRUE)
      data <- getParseData(exprs)
      data$depth <- cumsum((data$token == "'('") - (data$token == "')'"))
      data$index <- seq_len(nrow(data))
      data$exclude <- FALSE
      private$parse_data <- data

      private$refresh()
    },

    refresh = function() {
      private$strings <- file_identify(private$parse_data, private$ignore,
                                       private$pattern)
      private$data <- file_process(private$text, private$parse_data,
                                   private$strings)
    }
  ),

  public = list(
    initialize = function(path, ignore = NULL, pattern = NULL,
                          translations = NULL) {
      private$path <- path
      private$ignore <- ignore
      private$pattern <- pattern
      private$reload()
    },

    identify = function(ignore = NULL, pattern = NULL) {
      private$ignore <- ignore
      private$pattern <- pattern
      private$refresh()
      invisible(self)
    },

    info = function() {
      list(path = private$path,
           n_lines = length(private$text),
           n_strings = length(private$strings))
    },

    ## This one is the full-on
    render = function(f, id = NULL, context = NULL) {
      d <- private$data
      if (!is.null(id)) {
        d$is_string <- d$id %in% id
      }
      d$text[d$is_string] <- f(d$text[d$is_string])
      ret <- vcapply(unname(split(d$text, d$line)), paste0, collapse = "")
      i <- viapply(unname(split(d$is_string, d$line)), sum) > 0
      split_context(ret, i, context)
    }
  ))


R6_file_cli <- R6::R6Class(
  public = list(
    file = NULL,

    initialize = function(file) {
      self$file <- file
    },

    format = function(id = NULL, context = 3) {
      info <- self$file$info()
      dat <- self$file$render(crayon::inverse, id, context)
      w <- ceiling(log10(max(dat[[length(dat)]]$lines) + 1))
      f <- function(x) {
        paste(sprintf("%s: %s",
                      format(x$lines, width = w, justify = "right"), x$text),
              collapse = "\n")
      }
      sep <- sprintf("\n%s:\n", strrep("~", w))
      header <- sprintf("%s: %s (%s lines, %s strings)\n",
                        strrep("-", w), info$path, info$n_lines, info$n_strings)
      body <- paste(vcapply(dat, f), collapse = sep)
      paste0(header, body)
    }
  ))


split_context <- function(x, i, n) {
  if (!is.null(n) && is.finite(n)) {
    keep <- c(outer(which(i), seq(-n, n), "+"))
    line <- intersect(seq_along(x), keep)
  } else {
    line <- seq_along(x)
  }

  grp <- rep(0, length(line))
  jumps <- which(diff(line) > 1)
  grp[jumps + 1] <- 1
  groups <- split(line, cumsum(grp) + 1)

  ret <- vector("list", length(groups))
  for (g in seq_along(groups)) {
    ret[[g]] <- list(lines = groups[[g]],
                     text = x[groups[[g]]])
  }

  ret
}


file_identify <- function(parse_data, ignore, pattern) {
  exclude <- parse_data$exclude
  loc <- which(parse_data$token == "SYMBOL_FUNCTION_CALL" &
               parse_data$text %in% ignore)
  for (i in loc) {
    stopifnot(parse_data$token[[i + 2]] == "'('")
    j <- which(parse_data$index > i + 2 &
               parse_data$depth == parse_data$depth[[i]])[[1]]
    exclude[i:j] <- TRUE
  }

  idx <- parse_data$token == "STR_CONST" & !exclude
  if (!is.null(pattern)) {
    idx[idx] <- grepl(pattern, parse_data$text[idx])
  }

  parse_data$index[idx]
}


file_process <- function(text, parse_data, strings) {
  text <- as.list(text)
  id <- as.list(rep(0, length(text)))

  ## TODO: multi-line strings
  ## TODO: mutiple strings on a line
  stopifnot(all(parse_data$line1[strings] == parse_data$line2[strings]))
  stopifnot(!any(duplicated(parse_data$line1[strings])))

  for (i in strings) {
    line1 <- parse_data$line1[[i]]
    line2 <- parse_data$line2[[i]]
    from <- parse_data$col1[[i]]
    to <- parse_data$col2[[i]]
    x <- text[[line1]]
    text[[line1]] <- c(substr(x, 1L, from - 1L),
                       parse_data$text[[i]],
                       substr(x, to + 1L, nchar(x)))
    id[[line1]] <- c(0, i, 0)
  }

  id <- unlist(id)
  data_frame(line = rep(seq_along(text), lengths(text)),
             id = unlist(id),
             is_string = id > 0,
             text = unlist(text))
}
