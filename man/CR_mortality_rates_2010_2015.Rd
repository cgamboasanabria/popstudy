\name{CR_mortality_rates_2010_2015}
\alias{CR_mortality_rates_2010_2015}
\docType{data}
\title{
Costa Rica Mortality Rates
}
\description{
Mortality rates for Costa Rica in 2010-2015
}
\usage{data("CR_mortality_rates_2010_2015")}
\format{
  A data frame with 7656 observations on the following 4 variables.
  \describe{
    \item{\code{Year}}{a numeric vector}
    \item{\code{Age}}{a numeric vector}
    \item{\code{Female}}{a numeric vector with female mortality rates}
    \item{\code{Male}}{a numeric vector with male mortality rates}
  }
}

\source{
\href{https://www.inec.go.cr/}{https://www.inec.go.cr/}
}
\examples{
data(CR_mortality_rates_2010_2015)
summary(CR_mortality_rates_2010_2015)
}
\keyword{datasets}
