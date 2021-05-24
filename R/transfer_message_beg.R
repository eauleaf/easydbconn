#' Mark the beginning of a query to time the query duration
#'
#' @return a proc.time() object that acts as input to transfer_message_end()
#'
#' @importFrom utils timestamp
#'
#' @export
#' @examples easydbconn::transfer_message_beg()
transfer_message_beg <- function() {
  cat("---------Beginning Data Transfer---------\n")
  utils::timestamp()
  proc.time()
}
