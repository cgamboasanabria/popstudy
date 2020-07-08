\name{karup_king_factors}
\alias{karup_king_factors}
\docType{data}
\title{
karup_king_factors
}
\description{
Karup-King separation factors.
}
\usage{data("karup_king_factors")}
\format{
  A data frame with 76 observations on the following 7 variables.
  \describe{
    \item{\code{age}}{a character vector with simple ages}
    \item{\code{f1}}{a numeric vector, Karup-King factor}
    \item{\code{f2}}{a numeric vector, Karup-King factor}
    \item{\code{f3}}{a numeric vector, Karup-King factor}
    \item{\code{d1}}{a numeric vector, used in \code{\link{karup_king}} function, do not edit by hand}
    \item{\code{d2}}{a numeric vector, used in \code{\link{karup_king}} function, do not edit by hand}
    \item{\code{d3}}{a numeric vector, used in \code{\link{karup_king}} function, do not edit by hand}
  }
}
\references{
\insertRef{karup_king}{popstudy}
}
\examples{
data(karup_king_factors)
str(karup_king_factors)
}
\keyword{datasets}
