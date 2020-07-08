#' Linear growth
#'
#' Assuming an linear behavior, estimates the population size at time t, the growth rate, or population at time 0.
#'
#' @param Nt numeric. The population at time t. If null and date = FALSE, then estimate the population at time t.
#'
#' @param N0 numeric. The population at time 0. If null and date = FALSE, then estimate the population at time 0.
#'
#' @param r numeric. The growth rate. If null and date = FALSE, then estimate the growth rate for the time period [t0,t].
#'
#' @param t0 numeric. An object of class character with the date for the first population.
#'
#' @param t numeric. An object of class character with the date for the second population.
#'
#' @param time_interval character. A string with the time interval to calculate Delta_t.
#'
#' @param date logical. If TRUE, then estimates the moment t when Nt reaches a specific value.
#'
#' @return \code{growth_linear} returns a data frame with N0, Ntr, t0, t, delta, and time_interval for desire parameters.
#'
#' @examples
#'
#' # According to the Panama census at 2000-05-14,
#' # the population was 2,839,177. In 2010-05-16, the census
#' # calculates 3,405,813 population.
#' # To get r:
#'
#' growth_linear(N0=2839177, Nt=3405813, t0="2000-05-14", t="2010-05-16", time_interval = "years")
#'
#' # To get Nt at 2000-06-30:
#'
#' growth_linear(N0=2839177, r=0.0182, t0="2000-05-14", t="2000-06-30", time_interval = "years")
#'
#' # The time when the population will be 5,000,000.
#'
#' growth_linear(N0=2839177, Nt=5000000, r=0.0182, t0="2000-05-14", date=TRUE)
#'
#' @seealso \code{\link{growth_exp}},\code{\link{growth_logistic}}
#'
#' @author César Gamboa-Sanabria
#'
#' @references
#'
#' \insertRef{growth_exp}{popstudy}
#'
#' @export
growth_linear <- function(Nt=NULL, N0=NULL, r=NULL, t0, t, time_interval, date=FALSE){

    if(date){

        if(date & (is.null(N0) | is.null(r) | is.null(Nt))){

            stop("When date=TRUE, N0, Nt and r are needed")

        }else({

            delta <- (Nt-N0)/(N0*r)
            t <- date_decimal(decimal_date(ymd(t0))+delta)
            time_interval <- "years"
        })}else({

            if(((is.null(Nt) & is.null(N0)) | (is.null(Nt) & is.null(r)) | (is.null(N0) & is.null(r)))){
                stop("At least two from Nt, N0 and r must be a numeric value when date=FALSE")

            }else({

                delta <- interval(ymd(t0),ymd(t))
                delta <- eval(parse(text = paste("delta/", time_interval, "(1)", sep="")))

                if(delta<=0){

                    stop("t0 can't be greater than t")

                }else({

                    if(!is.null(Nt) & !is.null(N0) & is.null(r)){

                        r <- (Nt-N0)/(N0*delta)

                    }

                    if(is.null(Nt) & !is.null(N0) & !is.null(r)){

                        Nt <- (N0+N0*r*delta)

                    }

                    if(!is.null(Nt) & is.null(N0) & !is.null(r)){

                        N0 <- Nt/(1+r*delta)

                    }
                })
            })

        })

    data.frame(N0=as.integer(N0), Nt=as.integer(Nt), r=round(r, 4), t0=t0, t=paste(t),
               delta=ifelse(date==FALSE, paste(round(delta, 4)), delta), time_interval)

}
