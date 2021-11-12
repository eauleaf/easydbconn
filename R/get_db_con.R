#' Retrieves the db connection parameters and appropriate methods
#' NOTE: the system environment variables need to already be set
#'
#' @param friendly_db_name simple nickname for database
#' @param schema character string of name of schema to use (if applicable)
#'
#' @return table from query
#'
#' @import odbc
#' @import pool
#' @import DBI
#'
#' @export
#'
get_db_con <- function(friendly_db_name, schema = NULL) {

  # use dns connections if on linux
  curr_os <- get_os()

  # currently windows requires using an .renviron
  if(curr_os %in% c("windows","linux_renviron")){
    # determine if ssms or oracle
    systyp <- Sys.getenv(paste0(friendly_db_name, "_systyp"))


    ### create connections
    # msft sql server
    if (systyp %in% c("ssms", "microsoft sql server")) {
      con <- pool::dbPool(odbc::odbc(),
                          Driver = Sys.getenv("ssms_driver"),
                          Server = Sys.getenv(paste0(friendly_db_name, "_server")),
                          Database = Sys.getenv(paste0(friendly_db_name, "_db")),
                          UID = Sys.getenv(paste0(friendly_db_name, "_username")),
                          PWD = Sys.getenv(paste0(friendly_db_name, "_password")),
                          Port = as.numeric(Sys.getenv(paste0(friendly_db_name, "_port")))
      )
    }

    # snowflake
    else if (systyp == "snowflake") {
      con <- pool::dbPool(odbc::odbc(),
                          Driver = Sys.getenv("snowflake_driver"),
                          Server = Sys.getenv(paste0(friendly_db_name, "_server")),
                          Database = Sys.getenv(paste0(friendly_db_name, "_db")),
                          UID = Sys.getenv(paste0(friendly_db_name, "_username")),
                          PWD = Sys.getenv(paste0(friendly_db_name, "_password")),
                          Warehouse = Sys.getenv(paste0(friendly_db_name, "_wh")),
                          Schema = toupper(schema),
                          LogLevel = 0,
                          tracing = 0
      )
    }

    # oracle
    else if (systyp == "oracle") {
      host <- Sys.getenv(paste0(friendly_db_name, "_host"))
      port <- Sys.getenv(paste0(friendly_db_name, "_port"))
      sid <- Sys.getenv(paste0(friendly_db_name, "_sid"))

      connection_string <- paste(
        "(DESCRIPTION=",
        "(ADDRESS=(PROTOCOL=tcp)(HOST=", host, ")(PORT=", port, "))",
        "(CONNECT_DATA=(SID=", sid, ")))",
        sep = ""
      )

      con <- ROracle::dbConnect(DBI::dbDriver("Oracle"),
                                dbname = connection_string,
                                username = Sys.getenv(paste0(friendly_db_name, "_username")),
                                password = Sys.getenv(paste0(friendly_db_name, "_password"))
      )
    }
  }

  # linux and macos that use the odbc.ini and odbcisnt.ini files
  else {
    con <- pool::dbPool(odbc::odbc(), dsn = friendly_db_name, schema =  toupper(schema))
  }

  return(con)
}
