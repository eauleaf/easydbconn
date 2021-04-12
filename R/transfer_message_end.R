#' Displays when and how long a query took to retrieve
#'
#' @param dur a proc.time() object from transfer_message_beg()
#'
#' @return NULL
#'
#' @importFrom utils timestamp
#' @importFrom  data.table timetaken
#'
#' @export
#' @examples transfer_message_end(transfer_message_beg())
transfer_message_end <- function(dur){

  cat("---------Done Transferring Data---------\n")
  utils::timestamp()
  cat('\nTime required to perform task:', data.table::timetaken(dur), '\n\n')

}
