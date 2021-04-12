#' Save a table to a database
#'
#' @param df df to save
#' @param friendly_db_name specify a database name; connection parameters retrieved for db in `get_db_con()`
#'
#' @export
save_table <- function(df
                       ,tablename
                       ,friendly_db_name
                       ,replace_tbl = F
){

  ### for writing tables
  ##########################
  if(is.null(df)){ stop("No dataframe specified to save")}

  dur <- transfer_message_beg()

  # get connection
  con <- get_db_con(friendly_db_name)

  # determine if ssms or oracle
  systyp <- Sys.getenv(paste0(friendly_db_name,"_systyp"))

  # msft sql server
  if(systyp == "ssms"){

    if(replace_tbl){DBI::dbRemoveTable(conn = con, name = tablename)}
    DBI::dbWriteTable(con, tablename, df, append = T)
    DBI::dbDisconnect(con)

    cat(glue::glue("---------Done Writing {df} to {friendly_db_name}---------\n"))
    transfer_message_end(dur)

  }

  # TODO: implement snowflake when we can
  # else if(systyp == "oracle"){
  #
  # }


}

# for testing
# con <- easydbconn::get_db_con("my_db_name")
# DBI::dbRemoveTable(conn = con, name = "test")
# DBI::dbWriteTable(con, "test", iris, append = T)
