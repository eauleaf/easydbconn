#' Save a table to a database
#'
#' @param conn connection to database
#' @param df df to save
#' @param tablename character string to name the table in database
#' @param replace_tbl boolean to replace or append to the database table named tablename
#'
#' @export
save_table <- function(conn
                       ,df
                       ,tablename
                       ,replace_tbl = F
) {

  ### for writing tables
  ##########################
  # if (is.null(df)) {
  #   stop("No dataframe specified to save")
  # }

  dur <- transfer_message_beg()

  # get connection if not passed in
  # if(is.null(con)) con <- get_db_con(friendly_db_name, schema)

  # determine what database you're using (i.e. ssms, snowflake, oracle, etc)
  systyp <- get_systyp(conn)

  # msft sql server or snowflake
  if (systyp %in% c("ssms", "microsoft sql server",
                    "snowflake")) {
    if (replace_tbl) {
      pool::dbRemoveTable(conn = conn, name = tablename)
    }
    pool::dbWriteTable(conn, tablename, df, append = T)


    cat(glue::glue("---------Done Writing {tablename} to {systyp}---------\n"))
    transfer_message_end(dur)
  }


}

# for testing
# con <- easydbconn::get_db_con("my_db_name")
# DBI::dbRemoveTable(conn = con, name = "test")
# DBI::dbWriteTable(con, "test", iris, append = T)
