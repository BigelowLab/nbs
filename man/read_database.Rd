% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database.R
\name{read_database}
\alias{read_database}
\title{Read a NBS database}
\usage{
read_database(path, filename = "database.csv.gz", add_time = TRUE)
}
\arguments{
\item{path}{char, path description to the database}

\item{filename}{char, name of the file in the path}

\item{add_time}{logical, if TRUE add a time column to the database}
}
\value{
tibble
}
\description{
Read a NBS database
}
