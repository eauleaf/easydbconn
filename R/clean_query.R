#' removes sql comments and prepares a human-readable query
#'
#' @param query quoted sql query text
#'
#' @return a single line of query text
#'
#' @importFrom magrittr %>%
#' @export
#'
#' @examples clean_query("select * from  tbl --example comment")
clean_query <- function(query) {


  # echo original query to the console
  cat("\nThe query you're processing is: \n", query, "\n\n")


  # return
  query %>%
    # remove all demarked /*  */ sql comments
    gsub(pattern = "/\\*.*?\\*/", replacement = " ") %>%
    # remove all demarked -- comments
    gsub(pattern = "--[^\r\n]*", replacement = " ") %>%
    # remove everything after the query-end semicolon
    gsub(pattern = ";.*", replacement = " ") %>%
    # remove any line break, tab, etc.
    gsub(pattern = "[\r\n\t\f\v]", replacement = " ") %>%
    # remove extra whitespace
    gsub(pattern = " +", replacement = " ")
}
