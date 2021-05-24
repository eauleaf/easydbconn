#' read in query if saved in a text file
#'
#' @param query file path to a sql query
#'
#' @return text
#'
#' @importFrom magrittr %>%
#' @importFrom readr read_lines
#' @importFrom stringr str_detect
#'
#' @export
read_query <- function(query) {

  # if sql path, read, otherwise assume text input
  if (stringr::str_detect(query, "(?i)\\.(sql|txt|r)$")) {
    query %>%
      readr::read_lines() %>%
      paste(collapse = "\n")
  } else {
    query
  }
}
