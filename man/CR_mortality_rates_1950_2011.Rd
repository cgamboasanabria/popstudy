\name{CR_mortality_rates_1950_2011}
\alias{CR_mortality_rates_1950_2011}
\docType{data}
\title{
Costa Rica mortality rates
}
\description{
Mortality rates for Costa Rica 1950-2011.
}
\usage{data("CR_mortality_rates_1950_2011")}
\format{
  A data frame with 2170 observations on the following 4 variables.
  \describe{
    \item{\code{Year}}{a numeric vector}
    \item{\code{Age}}{a numeric vector}
    \item{\code{Female}}{a numeric vector with female mortality rates}
    \item{\code{Male}}{a numeric vector with male mortality rates}
    \item{\code{Total}}{a numeric vector with total mortality rates}
  }
}
\source{
\href{https://inec.cr/}{https://inec.cr/}
}
\examples{
data(CR_mortality_rates_1950_2011)
summary(CR_mortality_rates_1950_2011)
}
\keyword{datasets}
