% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/descriptive_plot.R
\name{descriptive_plot}
\alias{descriptive_plot}
\title{descriptive_plot}
\usage{
descriptive_plot(data, ..., labels = NULL, ylab = "Density")
}
\arguments{
\item{data}{data.frame.}

\item{...}{additional arguments to be passed to \code{\link[dplyr:select]{dplyr::select()}}.}

\item{labels}{A vector with x-axis labels.}

\item{ylab}{y-axis label.}
}
\value{
\code{descriptive_plot} function returns a plot with density and descriptive statistics.
}
\description{
Plot density with descriptive statistics for numerical values.
}
\examples{

df <- data.frame(var1=rpois(50, 6), var2=rgamma(50, shape=5,rate=.4), var3=rnorm(50, 10))
descriptive_plot(df, var1, var3)


}
\author{
Cesar Gamboa-Sanabria
}
