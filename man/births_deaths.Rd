\name{births_deaths}
\alias{births_deaths}
\docType{data}
\title{
Births and deaths data
}
\description{
Simulated data for Lexis Diagram examples.
}
\usage{data("births_deaths")}
\format{
  The format is:
List of 2
 $ births: tibble [32 x 3] (S3: tbl_df/tbl/data.frame)
  ..$ sex     : chr [1:32] "male" "male" "male" "male" ...
  ..$ date_reg: Date[1:32], format:  ...
  ..$ births  : num [1:32] 121558 126446 130839 130911 127524 ...
 $ deaths: tibble [112 x 4] (S3: tbl_df/tbl/data.frame)
  ..$ sex     : chr [1:112] "male" "male" "male" "male" ...
  ..$ date_reg: Date[1:112], format:  ...
  ..$ age     : num [1:112] 0 0 0 0 0 0 0 0 0 0 ...
  ..$ deaths  : num [1:112] 11411 10494 10814 9872 9457 ...
}
\examples{
data(births_deaths)
summary(births_deaths)
}
\keyword{datasets}
