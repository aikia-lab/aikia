% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme_aikia.R
\name{theme_aikia}
\alias{theme_aikia}
\title{aikia ggplot2 theme}
\usage{
theme_aikia(
  base_size = 11,
  base_family = "",
  base_line_size = base_size/170,
  base_rect_size = base_size/170
)
}
\arguments{
\item{base_size}{double}

\item{base_family}{character}

\item{base_line_size}{double}

\item{base_rect_size}{double}
}
\value{
function
}
\description{
aikia ggplot2 theme
}
\examples{
library(ggplot2)

ggplot(iris, aes(Sepal.Length)) +
geom_histogram()+
theme_aikia()
}
\seealso{
Other aikia themes: 
\code{\link{scale_fill_aikia_four}()},
\code{\link{theme_aikia_light}()}
}
\concept{aikia themes}
