#' Logistic growth
#'
#' Given two pivots and limits, estimates the growth assuming a logistic behavior.
#'
#' @param pivot_values numeric. Reference values to estimate, like TFR for two specific years.
#'
#' @param pivot_years numeric. Reference years to estimate for both values in \code{pivot_values}.
#'
#' @param upper numeric. Upper asymptotic value.
#'
#' @param lower numeric. Lower asymptotic value.
#'
#' @param t numeric. Year to get logistic value.
#'
#' @return \code{growth_logistic} returns the logistic estimation for specified year.
#'
#' @examples
#'
#' # Given TFR values 3.32 and 2.85 for the years 1986 and 1991, respectively,
#' # estimate the TFR in 1987 assuming 1.5 as lower limit and 8 as upper limit.
#'
#' growth_logistic(pivot_values = c(3.32, 2.85), pivot_years = c(1986, 1991),
#' upper = 8, lower=1.5, t=1987)
#'
#' @seealso \code{\link{growth_exp}}, \code{\link{growth_linear}}
#'
#' @author Cesar Gamboa-Sanabria
#'
#' @references
#'
#' \insertRef{growth_exp}{popstudy}
#'
#' @export


growth_logistic <- function(pivot_values, pivot_years, upper, lower, t){
    k1 <- lower
    k2 <- upper-k1
    y0 <- pivot_values[1]
    y1 <- pivot_values[2]
    x0 <- pivot_years[1]
    x1 <- pivot_years[2]
    a <- log((k1+k2-y0)/(y0-k1))
    b <- (1/(x1-x0))*(log((k1+k2-y1)/(y1-k1))-a)
    delta <- t-x0

    k1+(k2/(1+exp(a+b*delta)))
}
