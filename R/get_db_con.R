#' Retrieves the db connection parameters and appropriate methods
#' NOTE: the system environment variables need to already be set
#'
#' @param query query text to send to db
#' @param friendly_db_name simple nickname for database
#'
#' @return table from query
#'
#' @import odbc
#' @import DBI
#'
#' @export
#'
get_db_con <- function(friendly_db_name){

  # determine if ssms or oracle
  systyp <- Sys.getenv(paste0(friendly_db_name,"_systyp"))


  ### create connections
  # msft sql server
  if(systyp == "ssms"){

    con <- DBI::dbConnect(odbc::odbc()
                          ,Driver   =  Sys.getenv("ssms_driver")
                          ,Server   =  Sys.getenv(paste0(friendly_db_name,"_server"))
                          ,Database =  Sys.getenv(paste0(friendly_db_name,"_db"))
                          ,UID      =  Sys.getenv(paste0(friendly_db_name,"_username"))
                          ,PWD      =  Sys.getenv(paste0(friendly_db_name,"_password"))
                          ,Port     =  as.numeric(Sys.getenv(paste0(friendly_db_name,"ssms_port")))
    )

  }

  # oracle
  else if(systyp == "oracle"){

    host <- Sys.getenv(paste0(friendly_db_name,"_host"))
    port <- Sys.getenv(paste0(friendly_db_name,"_port"))
    sid <- Sys.getenv(paste0(friendly_db_name,"_sid"))

    connection_string <- paste(
      "(DESCRIPTION=",
      "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
      "(CONNECT_DATA=(SID=", sid, ")))", sep = "")

    con <- ROracle::dbConnect(DBI::dbDriver("Oracle"),
                              dbname = connection_string,
                              username = Sys.getenv(paste0(friendly_db_name,"_username")),
                              password = Sys.getenv(paste0(friendly_db_name,"_password"))
    )

  }

  return(con)


}
