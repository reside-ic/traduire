#' @get /
#' @html
function(res, req) {
  language <- as.list(req$HEADERS)[["accept-language"]]
  paste0(hello::world(language, type = "string"), "\n")
}

#' @get /hello/<animal>
#' @html
function(res, req, animal) {
  paste0(hello::monde(what = animal, type = "string"), "\n")
}
