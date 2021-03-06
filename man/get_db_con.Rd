% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_db_con.R
\name{get_db_con}
\alias{get_db_con}
\title{Retrieves the db connection parameters and appropriate methods
NOTE: the system environment variables need to already be set}
\usage{
get_db_con(friendly_db_name, schema = NULL)
}
\arguments{
\item{friendly_db_name}{simple nickname for database}

\item{schema}{character string of name of schema to use (if applicable)}
}
\value{
table from query
}
\description{
Retrieves the db connection parameters and appropriate methods
NOTE: the system environment variables need to already be set
}
