\name{CR_populations_1950_2015}
\alias{CR_populations_1950_2015}
\docType{data}
\title{
Costa Rica population
}
\description{
Estimated y projected populations for Costa Rica 1950-2015.
}
\usage{data("CR_populations_1950_2015")}
\format{
  A data frame with 7656 observations on the following 4 variables.
  \describe{
    \item{\code{Year}}{a numeric vector}
    \item{\code{Age}}{a numeric vector}
    \item{\code{Female}}{a numeric vector with female population}
    \item{\code{Male}}{a numeric vector with male population}
  }
}
\source{
\href{https://inec.cr/}{https://inec.cr/}
}
\examples{
data(CR_populations_1950_2015)
summary(CR_populations_1950_2015)
}
\keyword{datasets}
