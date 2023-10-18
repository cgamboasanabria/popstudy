\name{CR_births}
\alias{CR_births}
\docType{data}
\title{
CR_births
}
\description{
Births registers in Costa Rica.
}
\usage{data("CR_births")}
\format{
  A data frame with 8434 observations on the following 2 variables.
  \describe{
    \item{\code{date_reg}}{a Date}
    \item{\code{births}}{a numeric vector}
  }
}
\source{
\href{https://inec.cr/}{https://inec.cr/}
}
\examples{
data(CR_births)
summary(CR_births)
}
\keyword{datasets}
