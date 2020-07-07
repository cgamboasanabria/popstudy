\name{CR_deaths}
\alias{CR_deaths}
\docType{data}
\title{
CR_deaths
}
\description{
Deaths registers in Costa Rica.
}
\usage{data("CR_deaths")}
\format{
  A data frame with 229462 observations on the following 3 variables.
  \describe{
    \item{\code{date_reg}}{a Date}
    \item{\code{age}}{a numeric vector}
    \item{\code{deaths}}{a numeric vector}
  }
}

\source{
\href{https://www.inec.go.cr/}{https://www.inec.go.cr/}
}

\examples{
data(CR_deaths)
summary(CR_deaths)
}
\keyword{datasets}
