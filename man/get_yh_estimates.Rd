% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_yh_estimates.R
\name{get_yh_estimates}
\alias{get_yh_estimates}
\title{Get yh EPS and Revenue estimates and trend}
\usage{
get_yh_estimates(symbols = NULL, as_pivot_long = FALSE, verbose = FALSE)
}
\arguments{
\item{symbols}{A yahoo ticker symbol}

\item{as_pivot_long}{if \code{TRUE} returns result as pivot long}

\item{verbose}{if \code{TRUE} provides instant feedback}
}
\value{
a tibble with corporate estimates
}
\description{
Get yh EPS and Revenue estimates and trend
}
\examples{
\dontrun{
get_yh_estimates("AAPL", verbose = TRUE)
}

}
