\name{CR_populations_1950_2011}
\alias{CR_populations_1950_2011}
\docType{data}
\title{
Costa Rica population
}
\description{
Estimated y projected populations for Costa Rica 1950-2011.
}
\usage{data("CR_populations_1950_2011")}
\format{
  A data frame with 7656 observations on the following 4 variables.
  \describe{
    \item{\code{Year}}{a numeric vector}
    \item{\code{Age}}{a numeric vector}
    \item{\code{Female}}{a numeric vector with female population}
    \item{\code{Male}}{a numeric vector with male population}
    \item{\code{Total}}{a numeric vector with total population}
  }
}
\source{
\href{https://inec.cr/}{https://inec.cr/}
}
\examples{
data(CR_populations_1950_2011)
summary(CR_populations_1950_2011)
}
\keyword{datasets}
