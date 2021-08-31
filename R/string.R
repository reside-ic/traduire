glue_extract <- function(text, prefix, suffix) {
  found <- list()
  pos <- 0L
  extractor <- function(s, envir) {
    loc <- str_find(paste0(prefix, s, suffix), text, pos)
    pos <<- loc[[2]]
    found <<- c(found,
                list(list(text = trimws(s), from = loc[[1]], to = loc[[2]])))
    s
  }
  glue::glue(text,
             .transformer = extractor,
             .open = prefix, .close = suffix)
  data_frame(text = vcapply(found, "[[", "text"),
             from = vnapply(found, "[[", "from"),
             to = vnapply(found, "[[", "to"))
}


str_find <- function(sub, string, pos) {
  res <- gregexpr(sub, string, fixed = TRUE)[[1]]
  i <- res > pos
  if (!any(i)) {
    stop("Failed match!")
  }
  j <- which(i)[[1]]
  res[[j]] + c(0, attr(res, "match.length")[[j]] - 1L)
}


str_insert <- function(str, at, value) {
  paste0(substr(str, 1L, at - 1L), value, substr(str, at, nchar(str)))
}


Markup <- R6::R6Class(
  "Markup",
  public = list(
    text = NULL,
    data = NULL,
    spans = NULL,
    bounds = NULL,
    group = NULL,

    initialize = function(text, data) {
      self$text <- text
      self$data <- data
      self$spans <- list()
      self$group <- 0L
    },

    start = function() {
      self$group <- self$group + 1L
      self$group
    },

    add = function(tag, index, msg = NULL) {
      i <- index[[1]]
      j <- index[[length(index)]]
      group <- self$group
      open <- list(line = self$data$line1[i], col = self$data$col1[i],
                   group = group, tag = tag, open = TRUE, msg = msg)
      close <- list(line = self$data$line2[j], col = self$data$col2[j],
                    group = group, tag = tag, open = FALSE)
      self$spans <- c(self$spans, list(open, close))
      invisible(self)
    },

    render = function(tags, filter = TRUE, group = NULL, escape = FALSE) {
      markup_render(tags, self$spans, self$text, filter, group, escape)
    },

    summary = function() {
      tags <- lint_tags_names()
      spans <- matrix(self$spans, 2)
      m <- matrix(FALSE, length(self$text), length(tags),
                  dimnames = list(NULL, tags))
      for (i in seq_len(ncol(spans))) {
        from <- spans[[1, i]]
        to <- spans[[2, i]]
        m[from$line:to$line, from$tag] <- TRUE
      }

      m
    }
  ))


markup_render <- function(tags, spans, text, filter = TRUE, group = NULL,
                          escape = FALSE) {
  if (!is.null(group)) {
    spans <- spans[vlapply(spans, function(x) x$group %in% group)]
  }

  line <- viapply(spans, "[[", "line")
  col <- viapply(spans, "[[", "col")
  tag <- vcapply(spans, "[[", "tag")
  open <- vlapply(spans, "[[", "open")
  msg <- vcapply(spans, function(x) x$msg %||% NA_character_)

  if (escape) {
    res <- html_escape(text, line, col, open)
    text <- res$text
    col <- res$col
  }

  stopifnot(all(tag %in% names(tags)))

  for (i in order(line, -col, open)) {
    x <- text[[line[[i]]]]
    at <- col[[i]] + (if (open[[i]]) 0L else 1L)
    add <- tags[[tag[[i]]]](open[[i]], msg[[i]])
    text[[line[[i]]]] <- str_insert(x, at, add)
  }

  i <- !is.na(msg)
  messages <- data_frame(tag = tag[i], line = line[i], value = msg[i])

  lines <- unique(line)
  max_length <- length(text)
  if (length(lines) == 0) {
    ## There are no "interesting" lines so default to showing 5
    page <- select_text(text, cbind(c(1, min(max_length, 5))), max_length)
    line_labels <- page$line_labels
    text <- page$text
  } else if (filter) {
    lines <- vapply(lines, function(l) {
      lower <- max(1L, as.integer(l) - 5L) ## Can't include line before line 1
      upper <- min(max_length, as.integer(l) + 5L) ## Can't go beyond last line
      c(lower, upper)
    }, integer(2))
    lines <- union_intervals(lines)
    page <- select_text(text, lines, max_length)
    line_labels <- page$line_labels
    text <- page$text
  } else {
    line_labels <- seq_along(text)
  }

  list(line_labels = line_labels, text = text, messages = messages)
}


## Do html escaping while updating the column locations of symbols.
html_escape <- function(text, line, col, open) {
  tr <- c("&" = "&amp;", "<" = "&lt;", ">" = "&gt;")

  len <- nchar(tr)
  re <- sprintf("[%s]", paste(names(tr), collapse = ""))
  for (i in unique(line)) {
    at <- gregexpr(re, text[[i]])[[1L]]
    if (length(at) == 1 && at < 0) {
      next
    }
    from <- substr(rep(text[[i]], length(at)), at, at)
    offset <- c(0, unname(cumsum(len[from] - 1L)))
    j <- line == i
    k <- order(col[j])
    ci <- col[j][k]
    col[j][k] <- ci +
      offset[findInterval(ci + !open[j][k], at, left.open = TRUE) + 1L]
  }

  for (i in seq_along(tr)) {
    text <- gsub(names(tr)[[i]], tr[[i]], text, fixed = TRUE)
  }

  list(text = text, col = col)
}

## Note intervals must be ordered before this can be used
## for unioning. By ordered for a set of intervals
## (a1, b1), (a2, b2), .. (an, bn)
## It must be that a(i) <= b(i) for all i in 1,...n
## and a(i) <= a(i+1) for all i in 1,..(n-1)
IntervalStack <- R6::R6Class(
  "IntervalStack",
  cloneable = FALSE,
  public = list(
    stack = NULL,
    length = NULL,
    push = function(interval) {
      if (is.null(self$length)) {
        self$stack <- matrix(interval)
        self$length <- 1
      } else {
        last_interval <- self$stack[, self$length]
        ## All intervals are closed so we compare with interval + 1 for the case
        ## e.g. intervals [1, 2], [3, 4] this should return [1, 4]
        if (last_interval[2] + 1 < interval[1]) {
          self$stack <- unname(cbind(self$stack, interval))
          self$length <- self$length + 1
        } else {
          new_interval <- c(last_interval[1], 
                            max(last_interval[2], interval[2]))
          self$stack <- unname(cbind(self$stack[, -(self$length)], 
                                     new_interval))
        }
      }
    }
  )
)

union_intervals <- function(intervals) {
  ordered <- intervals[, order(intervals[1, ]), drop = FALSE]
  unioned <- IntervalStack$new()
  for (i in seq.int(ncol(ordered))) {
    unioned$push(ordered[, i])
  }
  unioned$stack
}

select_text <- function(text, lines, max_length) {
  connector_line <- "..."
  sections <- lapply(seq.int(ncol(lines)), function(i) {
    interval <- lines[, i]
    section_lines <- seq.int(interval[1], interval[2])
    section <- text[section_lines]
    if (interval[2] != max_length) {
      ## Interval has ended before the end of the document, so add a line break
      section <- c(section, connector_line)
      section_lines <- c(section_lines, "...")
    }
    list(section = section,
         line_labels = section_lines)
  })
  list(text = unlist(lapply(sections, "[[", "section")),
       line_labels = unlist(lapply(sections, "[[", "line_labels")))
}
