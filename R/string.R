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
