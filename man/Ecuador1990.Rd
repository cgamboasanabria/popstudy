\name{Ecuador1990}
\alias{Ecuador1990}
\docType{data}
\title{
Ecuador1990
}
\description{
Ecuador census data in 1990 by grouped ages.
}
\usage{data("Ecuador1990")}
\format{
  A data frame with 21 observations on the following 4 variables.
  \describe{
    \item{\code{age}}{a factor with levels \code{0-4} \code{5-9} \code{10-14} \code{15-19} \code{20-24} \code{25-29} \code{30-34} \code{35-39} \code{40-44} \code{45-49} \code{50-54} \code{55-59} \code{60-64} \code{65-69} \code{70-74} \code{75-79} \code{80-84} \code{85-89} \code{90-94} \code{95-99} \code{100+}}
    \item{\code{male}}{a numeric vector with males population}
    \item{\code{female}}{a numeric vector with female population}
    \item{\code{population}}{a numeric vector Ecuador population}
  }
}
\source{
\href{https://microdata.worldbank.org/index.php/catalog/499}{https://microdata.worldbank.org/index.php/catalog/499}
}
\examples{
data(Ecuador1990)
summary(Ecuador1990)
}
\keyword{datasets}
