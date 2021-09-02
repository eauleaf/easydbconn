#' Remove a table to a database
#'
#' @param conn connection to database
#' @param tablename character string name of the table in database to remove
#'
#' @export
remove_table <- function(conn,
                         tablename) {

  ### for deleting tables
  ##########################
  # if (is.null(tablename)) {
  #   stop("No dataframe specified to remove")
  # }

  dur <- transfer_message_beg()


  # determine what database you're using (i.e. ssms, snowflake, oracle, etc)
  systyp <- get_systyp(conn)

  # msft sql server or snowflake
  if (systyp %in% c("microsoft sql server", "ssms","snowflake")) {
    pool::dbRemoveTable(conn = conn, name = tablename)
  }

  cat(glue::glue("\n---------Done Removing {tablename} from {systyp}---------\n"))
  transfer_message_end(dur)

}

# for testing
# con <- easydbconn::get_db_con("my_db_name")
# DBI::dbRemoveTable(conn = con, name = "test")
# DBI::dbWriteTable(con, "test", iris, append = T)
