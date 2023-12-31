% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nbs.R
\name{nbs_product_url}
\alias{nbs_product_url}
\title{Craft a nbs URL for a given date}
\usage{
nbs_product_url(
  product = c("uvcomp_SCISixHourGlobal", "stress_SCISixHourGlobal",
    "uvcomp_SCIDailyGlobal", "stress_SCIDailyGlobal", "uvcomp_SCIMonthlyGlobal",
    "stress_SCIMonthlyGlobal", "uvcomp_NRTSixHourGlobal", "stress_NRTSixHourGlobal"),
  root = nbs_base_url()
)
}
\arguments{
\item{product}{character, the name of the product}

\item{root}{character, the root URL}
}
\value{
one or more URLs
}
\description{
Craft a nbs URL for a given date
}
