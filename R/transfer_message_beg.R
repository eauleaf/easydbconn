#' Mark the beginning of a query to time the query duration
#'
#' @param msg optional character message to include at the begging of a timed process
#'
#' @return a proc.time() object that acts as input to transfer_message_end()
#'
#' @importFrom utils timestamp
#'
#' @export
#' @examples easydbconn::transfer_message_beg()
transfer_message_beg <- function(msg="Beginning Process") {
  cat(glue::glue("---------{msg}---------\n\n"))
  utils::timestamp()
  proc.time()
}
