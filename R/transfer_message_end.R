#' Displays when and how long a query took to retrieve
#'
#' @param dur a proc.time() object from transfer_message_beg()
#' @param msg optional character message to include at the completion of a timed process
#'
#' @return NULL
#'
#' @importFrom utils timestamp
#' @importFrom  data.table timetaken
#'
#' @export
#' @examples transfer_message_end(transfer_message_beg())
transfer_message_end <- function(dur, msg = "Process Complete") {
  cat(glue::glue("\n\n---------{msg}---------\n\n"))
  utils::timestamp()
  cat("\nTime required to perform task:", data.table::timetaken(dur), "\n\n")
}
