#' Pass a query to a database and get a table back
#'
#' @param query a text query in quotes ('select * from tbl') or a network path to a saved sql query (/path/saved_query.sql)
#' @param friendly_db_name specify a database name; connection parameters retrieved for db in `get_db_con()`
#' @param cast_as_tibble make the output a tibble
#' @param simplify_dates transform all dates in returned table from datetime to just date
#'
#' @return a table of data output specified by the query
#'
#' @importFrom purrr modify_if
#'
#' @export
get_data <- function(query,
                     friendly_db_name,
                     cast_as_tibble = TRUE,
                     simplify_dates = TRUE) {

  # prep query
  cleaned_query <-
    query %>%
    read_query() %>%
    clean_query()


  dur <- transfer_message_beg()

  # determine if ssms or oracle
  systyp <- Sys.getenv(paste0(friendly_db_name, "_systyp"))
  # get connection
  con <- get_db_con(friendly_db_name)

  ### query results
  # msft sql server
  if (systyp == "ssms") {
    query_results <- DBI::dbGetQuery(con, query)
    DBI::dbDisconnect(con)
  }
  # oracle
  else if (systyp == "oracle") {
    query_results <- ROracle::dbGetQuery(con, query)
    ROracle::dbDisconnect(con)
  }

  transfer_message_end(dur)


  # dataframe options
  if (cast_as_tibble) query_results <- dplyr::as_tibble(query_results)
  if (simplify_dates) query_results <- purrr::modify_if(query_results, lubridate::is.POSIXt, as.Date)


  return(query_results)
}
