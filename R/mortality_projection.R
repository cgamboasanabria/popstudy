#' mortality_projection
#'
#' Forecasting mortality rates.
#'
#' @param mortality_rates_path PENDIENTE
#'
#' @param total_population_path PENDIENTE
#'
#' @param omega_age PENDIENTE
#'
#' @param horizon PENDIENTE
#'
#' @param first_year_projection PENDIENTE
#'
#' @param ... PENDIENTE
#'
#' @return \code{mortality_projection} PENDIENTE
#'
#' @examples
#'
#' ## PENDIENTE
#'
#' @author Cesar Gamboa-Sanabria
#'
#' @export
mortality_projection <- function(mortality_rates_path, total_population_path,
                                 omega_age, horizon, first_year_projection,...){

    ####################Proceso Hyndman editado con ARIMA##################
    suppressWarnings({
        lt <- function (mx, startage = 0, agegroup = 5, sex)
        {
            # Omit missing ages
            if (is.na(mx[1]))
                mx[1] <- 0
            firstmiss <- (1:length(mx))[is.na(mx)][1]
            if (!is.na(firstmiss))
                mx <- mx[1:(firstmiss - 1)]
            nn <- length(mx)
            if (nn < 1)
                stop("Not enough data to proceed")

            # Compute width of each age group
            if (agegroup == 1)
                nx <- c(rep(1, nn - 1), Inf)
            else if (agegroup == 5) # First age group 0, then 1-4, then 5-year groups.
                nx <- c(1, 4, rep(5, nn - 3), Inf)
            else stop("agegroup must be either 1 or 5")

            if (agegroup == 5 & startage > 0 & startage < 5)
                stop("0 < startage < 5 not supported for 5-year age groups")

            if (startage == 0) # for single year data and the first age (0) in 5-year data
            {
                if (sex == "female")
                {
                    if (mx[1] < 0.107)
                        a0 <- 0.053 + 2.8 * mx[1]
                    else a0 <- 0.35
                }
                else if (sex == "male")
                {
                    if (mx[1] < 0.107)
                        a0 <- 0.045 + 2.684 * mx[1]
                    else a0 <- 0.33
                }
                else # if(sex == "total")
                {
                    if (mx[1] < 0.107)
                        a0 <- 0.049 + 2.742 * mx[1]
                    else a0 <- 0.34
                }
            }
            else if (startage > 0)
                a0 <- 0.5
            else
                stop("startage must be non-negative")
            if (agegroup == 1)
            {
                if (nn > 1)
                    ax <- c(a0, rep(0.5, nn - 2), Inf)
                else
                    ax <- Inf
            }
            else if (agegroup == 5 & startage == 0)
            {
                if (sex == "female")
                {
                    if (mx[1] < 0.107)
                        a1 <- 1.522 - 1.518 * mx[1]
                    else
                        a1 <- 1.361
                }
                else if (sex == "male")
                {
                    if (mx[1] < 0.107)
                        a1 <- 1.651 - 2.816 * mx[1]
                    else
                        a1 <- 1.352
                }
                else # sex == "total"
                {
                    if (mx[1] < 0.107)
                        a1 <- 1.5865 - 2.167 * mx[1]
                    else a1 <- 1.3565
                }
                ax <- c(a0, a1, rep(2.6, nn - 3), Inf)
                ### ax=2.5 known to be too low esp at low levels of mortality
            }
            else # agegroup==5 and startage > 0
            {
                ax <- c(rep(2.6, nn - 1), Inf)
                nx <- c(rep(5, nn))
            }
            qx <- nx * mx/(1 + (nx - ax) * mx)
            # age <- startage + cumsum(nx) - 1
            # if (max(age) >= 75) {
            #    idx <- (age >= 75)
            #   ax[idx] <- (1/mx + nx - nx/(1 - exp(-nx * mx)))[idx]
            #  qx[idx] <- 1 - exp(-nx * mx)[idx]
            #    }
            #qx[qx > 1] <- 1  ################  NOT NEEDED IN THEORY

            #plot(qx)  #### TO CHECK RESULT RE QX>1

            qx[nn] <- 1
            if (nn > 1)
            {
                lx <- c(1, cumprod(1 - qx[1:(nn - 1)]))
                dx <- -diff(c(lx, 0))
            }
            else
                lx <- dx <- 1
            Lx <- nx * lx - dx * (nx - ax)
            Lx[nn] <- lx[nn]/mx[nn]
            Tx <- rev(cumsum(rev(Lx)))
            ex <- Tx/lx
            if (nn > 2)
                rx <- c(Lx[1]/lx[1], Lx[2:(nn - 1)]/Lx[1:(nn - 2)], Tx[nn]/Tx[nn-1])
            else if (nn == 2)
                rx <- c(Lx[1]/lx[1], Tx[nn]/Tx[nn - 1])
            else
                rx <- c(Lx[1]/lx[1])
            if (agegroup == 5)
                rx <- c(0, (Lx[1] + Lx[2])/5 * lx[1], Lx[3]/(Lx[1]+Lx[2]),
                        Lx[4:(nn - 1)]/Lx[3:(nn - 2)], Tx[nn]/Tx[nn-1])
            result <- data.frame(ax = ax, mx = mx, qx = qx, lx = lx,
                                 dx = dx, Lx = Lx, Tx = Tx, ex = ex, rx = rx, nx = nx)
            return(result)
        }

        get.e0 <- function(x,agegroup,sex,startage=0)
        {
            lt(x, startage, agegroup, sex)$ex[1]
        }

        fitmx <- function (kt,ax,bx,transform=FALSE)
        {
            # Derives mortality rates from kt mortality index,
            # per Lee-Carter method
            clogratesfit <- outer(kt, bx)
            logratesfit <- sweep(clogratesfit,2,ax,"+")
            if(transform)
                return(logratesfit)
            else
                return(exp(logratesfit))
        }

        forecast.lca2 <- function(object, h=50, se=c("innovdrift","innovonly"), jumpchoice=c("fit","actual"), level=80,...)
        {
            se <- match.arg(se)
            jumpchoice <- match.arg(jumpchoice)

            # Section 1 Read in data from object
            jumpyear <- max(object$year)
            nyears <- length(object$year)
            nages <- length(object$age)

            # Find jumprates
            if(jumpchoice=="actual")
                jumprates <- object[[4]][,nyears]
            else if(jumpchoice=="fit")
                jumprates <- exp(object$ax + object$bx*object$kt[nyears])
            else
                stop(paste("Unknown jump choice:",jumpchoice))
            object$kt <- object$kt - object$kt[nyears]
            #####################################################################
            # Time series estimation of kt as Random walk with drift
            fit <- forecast::Arima(object$kt, include.drift = T,...)
            kt.drift <- fit$coef["drift"]
            sec <- sqrt(last(c(fit$var.coef)))/sqrt(length(fit$fitted))
            see <- sqrt(fit$sigma2)
            ####################################################################
            # Project kt
            x <- 1:h
            zval <- stats::qnorm(0.5 + 0.005*level)
            kt.forecast <- object$kt[nyears] + (x * kt.drift)

            # Calculate standard errors of forecast kt
            if (se=="innovdrift")
                kt.stderr <- sqrt(x*(see^2) + (x*sec)^2)
            else if(se=="innovonly")
                kt.stderr <- sqrt(x*(see^2))
            kt.lo.forecast <- kt.forecast - (zval*kt.stderr)
            kt.hi.forecast <- kt.forecast + (zval*kt.stderr)
            kt.f <- data.frame(kt.forecast,kt.lo.forecast,kt.hi.forecast)
            names(kt.f) <- c("kt forecast","kt lower","kt upper")
            deltat <- object$year[2] - object$year[1]
            kt.f <- ts(kt.f,start=object$year[nyears]+deltat,deltat=deltat)

            # Calculate expected life and mx forecasts
            e0.forecast <- rep(0,h)
            mx.forecast <- matrix(0,nrow=nages,ncol=h)
            colnames(mx.forecast) <- seq(h)
            rownames(mx.forecast) <- object$age
            mx.lo.forecast <- mx.hi.forecast <- mx.forecast
            logjumprates <- log(jumprates)
            series <- names(object)[4]
            agegroup <- object$age[4]-object$age[3]
            for (cnt in 1:h)
            {
                mx.forecast[,cnt] <- fitmx(kt.f[cnt,1], logjumprates, object$bx)
                mx.lo.forecast[,cnt] <- fitmx(kt.f[cnt,2], logjumprates, object$bx)
                mx.hi.forecast[,cnt] <- fitmx(kt.f[cnt,3], logjumprates, object$bx)
                e0.forecast[cnt] <- get.e0(mx.forecast[,cnt],agegroup,series,startage=min(object$age))
            }
            kt.f <- data.frame(kt.forecast,kt.lo.forecast,kt.hi.forecast)
            names(kt.f) <- c("kt forecast","kt lower","kt upper")
            kt.f <- ts(kt.f,start=object$year[nyears]+deltat,deltat=deltat)

            output = list(label=object$label,age=object$age,year=object$year[nyears] + x*deltat,
                          rate=list(forecast=mx.forecast,lower=mx.lo.forecast,upper=mx.hi.forecast),
                          fitted=object$fitted,
                          e0=ts(e0.forecast,start=object$year[nyears]+deltat,deltat=deltat),
                          kt.f=structure(list(mean=kt.f[,1],lower=kt.f[,2],upper=kt.f[,3],level=level,x=object$kt,
                                              method="Random walk with drift"),class="forecast"),
                          type = object$type,lambda=0)
            names(output$rate)[1] = names(object)[4]
            output$model <- object
            output$model$jumpchoice <- jumpchoice
            output$model$jumprates <- jumprates
            output$call <- match.call()
            output$name <- names(object)[4]
            return(structure(output,class=c("fmforecast","demogdata")))
        }

        #########################Fin Hyndman editado################################

        #mortality_rates_path
        mortality_rates <- read.demogdata(file = mortality_rates_path,
                                          popfile = total_population_path, type = "mortality",
                                          max.mx = 1, skip = 0, popskip = 0, label = "CR")

        final_rates <- extract.years(data = mortality_rates,
                                     years = min(mortality_rates$year):(first_year_projection-1))

        final_rates <- set.upperage(data = final_rates, max.age = omega_age)
        LC_males <- lca(data = final_rates, series = "male", max.age = omega_age)
        LC_females <- lca(data = final_rates, series = "female", max.age = omega_age)

        horizon <- horizon-max(final_rates$year)
        h <- horizon

        male_time_serie <- op.arima(time_serie=LC_males$kt,arima_process=c(6,1,6,0,0,0), seasonal_periodicity=12,
                                    horiz = h, prop=.8, training_weight=.2, testing_weight=.8,
                                    parallelize = F)

        male_model_selected <- gsub(".*\\(|\\).*", "", male_time_serie$bests$arima_model[1])
        male_model_selected <- paste("c(", male_model_selected, ")", sep="")
        males_order <- eval(parse(text = male_model_selected))


        female_time_serie <- op.arima(time_serie=LC_females$kt,arima_process=c(6,1,6,0,0,0), seasonal_periodicity=12,
                                      horiz = h, prop=.8, training_weight=.2, testing_weight=.8,
                                      parallelize = F)

        female_model_selected <- gsub(".*\\(|\\).*", "", female_time_serie$bests$arima_model[1])
        female_model_selected <- paste("c(", female_model_selected, ")", sep="")
        females_order <- eval(parse(text = female_model_selected))

        males_projection <- forecast.lca2(object = LC_males, h = horizon, jumpchoice = "actual",order=males_order)
        females_projection <- forecast.lca2(object = LC_females, h = horizon, jumpchoice = "actual",order=females_order)
    })

    mortality_projected <- list(male = males_projection, female = females_projection)
    mortality_projected <- structure(.Data = mortality_projected, class="fmforecast2")
    mortality_projected

}
