#' Determine the type of type of database you are using (i.e. snowflake, ssms, oracle, etc)
#'
#' @param conn connection to database
#'
#'
get_systyp <- function(conn) {

  if((class(conn) == "Pool") %>% sum() != 0){
    systyp <- dbGetInfo(conn)$pooledObjectClass %>% tolower()
  } else {
    systyp <- conn@info$dbms.name %>% tolower()
  }

  return(systyp)
}
