% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/val_date.R
\name{val_date}
\alias{val_date}
\title{Return the current valuation date}
\usage{
val_date(offset = -1, location = "us")
}
\arguments{
\item{offset}{A negative integer defining the number of business days to offset from today}

\item{location}{Which holiday calendar to use. Either "us" for NYSE or "de" for Boerse Frankfurt}
}
\value{
An object of type \code{date}
}
\description{
Return the current valuation date
}
\examples{
val_date()
val_date(offset = -5)
val_date(offset = -3, location = "de")
}
