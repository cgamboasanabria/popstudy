\name{grouped_age_CR_pop}
\alias{grouped_age_CR_pop}
\docType{data}
\title{
grouped_age_CR_pop
}
\description{
Costa Rica population by 5-year-group ages in 2011.
}
\usage{data("grouped_age_CR_pop")}
\format{
  A data frame with 16 observations on the following 2 variables.
  \describe{
    \item{\code{age}}{an ordered factor with levels \code{0 - 4} < \code{5 - 9} < \code{10 - 14} < \code{15 - 19} < \code{20 - 24} < \code{25 - 29} < \code{30 - 34} < \code{35 - 39} < \code{40 - 44} < \code{45 - 49} < \code{50 - 54} < \code{55 - 59} < \code{60 - 64} < \code{65 - 69} < \code{70 - 74} < \code{75 and more}}
    \item{\code{pop}}{a numeric vector with the populaion}
  }
}
\source{
\href{https://www.inec.go.cr/}{https://www.inec.go.cr/}
}
\examples{
data(grouped_age_CR_pop)
str(grouped_age_CR_pop)
}
\keyword{datasets}
