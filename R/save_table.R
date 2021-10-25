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
  dur <- transfer_message_beg()
  systyp <- get_systyp(conn)

  # determine what database you're using (i.e. ssms, snowflake, oracle, etc)
  if (systyp %in% c("ssms", "microsoft sql server", "snowflake")) {
    is_there <- pool::dbExistsTable(conn = conn, name = tablename)
    if (replace_tbl & is_there) {
      pool::dbRemoveTable(conn = conn, name = tablename)
      pool::dbCreateTable(conn = conn, name = tablename, fields = df)

    } else if (!is_there) {
      pool::dbCreateTable(conn = conn, name = tablename, fields = df)
    }

    split(df, rep(1:ceiling(nrow(df)/10000), length.out = nrow(df), each = 10000)) %>%
      map(~pool::dbAppendTable(conn = conn, name = tablename, value = .))


    cat(glue::glue("---------Done Writing {tablename} to {systyp}---------\n"))
    transfer_message_end(dur)

    return(T)
  }
}


# for testing
# con <- easydbconn::get_db_con("my_db_name")
# DBI::dbRemoveTable(conn = con, name = "test")
# DBI::dbWriteTable(con, "test", iris, append = T)
