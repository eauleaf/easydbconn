#' Pass a query to a database and get a table back
#'
#' @param conn pass in connection string if created elsewhere (this is optional)
#' @param query a text query in quotes ('select * from tbl') or a network path to a saved sql query (/path/saved_query.sql)
#' @param cast_as_tibble make the output a tibble
#' @param simplify_dates transform all dates in returned table from datetime to just date
#'
#' @return a table of data output specified by the query
#'
#' @importFrom purrr modify_if
#'
#' @export
get_data <- function(conn
                     ,query
                     ,cast_as_tibble = TRUE
                     ,simplify_dates = TRUE

) {

  # prep query
  cleaned_query <-
    query %>%
    read_query() %>%
    clean_query()


  dur <- transfer_message_beg()


  # get connection (if not passed into function)
  # if(is.null(con)){
  #   con <- get_db_con(friendly_db_name)

  # determine if ssms, snowflake, or oracle
  systyp <- get_systyp(conn)

  ### query results
  # msft sql server and snowflake
  if (systyp %in% c("ssms", "microsoft sql server",
                    "snowflake")) {
    query_results <- pool::dbGetQuery(conn, query)
  }
  # oracle
  else if (systyp %in% c("oracle")) {
    query_results <- ROracle::dbGetQuery(conn, query)
    ROracle::dbDisconnect(conn)
  }

  transfer_message_end(dur)


  # dataframe options
  if (cast_as_tibble) query_results <- dplyr::as_tibble(query_results)
  if (simplify_dates) query_results <- purrr::modify_if(query_results, lubridate::is.POSIXt, as.Date)


  return(query_results)
}
