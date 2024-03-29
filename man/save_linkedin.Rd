% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme_aikia.R
\name{save_linkedin}
\alias{save_linkedin}
\title{Save a ggplot for LinkedIn}
\usage{
save_linkedin(plot, filename, format = "png")
}
\arguments{
\item{plot}{a ggplot object}

\item{filename}{the path and filename to write to}

\item{format}{the output format}
}
\value{
saves the plot to the specified path
}
\description{
Save a ggplot for LinkedIn
}
\examples{
\dontrun{
p <- iris \%>\%
ggplot(aes(Sepal.Length, Sepal.Width)) +
geom_point(aes(color = Species)) +
theme_aikia() +
scale_color_aikia_four()

save_linkedin(p, "testplot")}
}
