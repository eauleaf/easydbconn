#' Returns the name of the operating system
#'
#' @return character string of operation system
#'
#' @export
get_os <- function(){
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    # this need to change at some point
    if(grepl("generic",sysinf['release']))
      os <- 'linux_renviron'
    else if (os == 'Darwin')
      os <- "osx"
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os)){
      os <- "linux"
    }
  }
  tolower(os)
}


