#' netmigration_projection
#'
#' Forecasting net migration.
#'
#' @param mortality_rates_path character. Path to Mortality rates in a .txt file.
#'
#' @param TFR_path character. Path to Fertility rates in a .txt file.
#'
#' @param total_population_path character. Path to Populations in a .txt file.
#'
#' @param WRA_path character. Path to Women of Reproductive Age in a .txt file.
#'
#' @param omega_age numeric. Maximum age.
#'
#' @param horizon numeric. The forecast horizon.
#'
#' @param first_year_projection numeric. Year for the base population.
#'
#' @return \code{netmigration_projection} returns the forecast net migration.
#'
#' @examples
#'
#' \donttest{
#'
#' library(dplyr)
#'
#' data(CR_mortality_rates_1950_2011)
#'
#' #CR_mortality_rates_1950_2011 %>%
#' #write.table(.,
#' #file = "CR_mortality_rates_1950_2011.txt",
#' #sep = "\t",
#' #row.names = FALSE,
#' #col.names = TRUE,
#' #quote = FALSE)
#'
#'
#' data(CR_populations_1950_2011)
#'
#' #CR_populations_1950_2011 %>%
#' #write.table(.,
#' #file = "CR_populations_1950_2011.txt",
#' #sep = "\t",
#' #row.names = FALSE,
#' #col.names = TRUE,
#' #quote = FALSE)
#'
#' data(CR_fertility_rates_1950_2011)
#'
#' #CR_fertility_rates_1950_2011 %>%
#' #write.table(.,
#' #file = "CR_fertility_rates_1950_2011.txt",
#' #sep = "\t",
#' #row.names = FALSE,
#' #col.names = TRUE,
#' #quote = FALSE)
#'
#'
#' data(CR_women_childbearing_age_1950_2011)
#'
#' #CR_women_childbearing_age_1950_2011 %>%
#' #write.table(.,
#' #file = "CR_women_childbearing_age_1950_2011.txt",
#' #sep = "\t",
#' #row.names = FALSE,
#' #col.names = TRUE,
#' #quote = FALSE)
#'
#' #result <- netmigration_projection(mortality_rates_path = "CR_mortality_rates_1950_2011.txt",
#' #total_population_path = "CR_populations_1950_2011.txt",
#' #TFR_path = "CR_fertility_rates_1950_2011.txt",
#' #WRA_path = "CR_women_childbearing_age_1950_2011.txt",
#' #omega_age = 115, first_year_projection = 2011, horizon = 2150)
#'
#'
#' }
#'
#' @author Cesar Gamboa-Sanabria
#'
#' @export
netmigration_projection <- function(mortality_rates_path, TFR_path, total_population_path, WRA_path,
                                    omega_age, horizon, first_year_projection){

    requireNamespace("rainbow", quietly=TRUE)
    #Mortalidad

    mortality_rates <- read.demogdata(file = mortality_rates_path,
                                      popfile = total_population_path, type = "mortality",
                                      max.mx = 1, skip = 0, popskip = 0, label = "CR")

    smoothed_mortality_rates <- extract.years(data = mortality_rates,
                                              #years = min(mortality_rates$year):(first_year_projection-1)) #Esto fue el cambio
                                              years = min(mortality_rates$year):(first_year_projection))

    smoothed_mortality_rates <- set.upperage(data = smoothed_mortality_rates, max.age = omega_age)
    smoothed_mortality_rates <- smooth.demogdata(data = smoothed_mortality_rates)

    #   tasas.mortalidad <- read.demogdata(file = mortalidad,
    #                                    popfile = poblaciones, type = "mortality",
    #                                    max.mx = 1, skip = 0, popskip = 0, label = "CR")
    #
    # tasas.mortalidad.suavizadas <- extract.years( data = tasas.mortalidad, years = 1950:(first_year_projection))
    # tasas.mortalidad.suavizadas <- set.upperage(data = tasas.mortalidad.suavizadas, max.age = omega_age)
    # tasas.mortalidad.suavizadas <- smooth.demogdata(data = tasas.mortalidad.suavizadas)

    #Fecundidad

    TFR <- read.demogdata(file = TFR_path, popfile = WRA_path,
                          type = "fertility", max.mx = 1000,  skip = 0,
                          popskip = 0, label = "CR")

    final_TFR<- extract.years(data = TFR,
                              years = min(TFR$year):(first_year_projection-1))

    horizon <- horizon-max(final_TFR$year)

    smoothed_TFR <- extract.years(TFR, min(TFR$year):(first_year_projection-1))
    smoothed_TFR <- smooth.demogdata(data = smoothed_TFR)

    pos <- which(apply(smoothed_TFR$rate$female, 2, function(x) sum(is.na(x)))>0)
    if(length(pos)>0){
        for(i in pos){

            smoothed_TFR$rate$female[,i] <- smoothed_TFR$rate$female[,pos-1]*(colSums(smoothed_TFR$rate$female)[i-1]/colSums(smoothed_TFR$rate$female)[i-2])
        }
    }

    # tasas.fecundidad <- read.demogdata(file = fecundidad, popfile = WRA_path,
    #                                    type = "fertility",   max.mx = 1000,  skip = 0,
    #                                    popskip = 0, label = "CR")
    #
    # tasas <- extract.years(data = tasas.fecundidad,
    #                        years = 1950:(first_year_projection-1))
    #
    # horizon <- horizon-max(tasas$year)
    #
    # tasas.fecundidad.suavizadas <- extract.years(tasas.fecundidad, 1950:(first_year_projection-1))
    # tasas.fecundidad.suavizadas <- smooth.demogdata(data = tasas.fecundidad.suavizadas)
    #
    # tasas.fecundidad.suavizadas$rate$female[,61] <- tasas.fecundidad.suavizadas$rate$female[,60]*(1905/1938)#ajuste suavizadas

    netmig_estimated <- netmigration(mort = smoothed_mortality_rates, fert = smoothed_TFR,
                                     mfratio = 1.0545)

    migration_model <- coherentfdm(data = netmig_estimated)

    #########Proceso con selección de arimas############

    forecast.fdm2 <- function(object, h=50, level=80, jumpchoice=c("fit","actual"),
                              method="arima", warnings=FALSE, ...)
    {

        jumpchoice <- match.arg(jumpchoice)

        if(sum(object$weights < 0.1)/length(object$weights) > 0.2) # Probably exponential weights for fitting. Can be ignored for forecasting
            object$weights[object$weights > 0] <- 1

        if(warnings)
            fcast <- forecast.ftsm2(object,h=h,level=level,jumpchoice=jumpchoice,method=method,...)
        else
            suppressWarnings(fcast <- forecast.ftsm2(object,h=h,level=level,jumpchoice=jumpchoice,method=method,...))

        # Compute observational variance
        # Update to deal with general transformations
        #    fred <- InvBoxCox(object[[4]],object$lambda)
        #    if(object$type != "migration")
        #        fred <- pmax(fred,0.000000001)
        #    if(object$type == "mortality") # Use binomial distribution
        #        s <- sqrt((1-fred)*fred^(2*object$lambda-1)/pmax(object$pop,1))
        #    else if(object$type=="fertility") # Use Poisson distribution
        #        s <- sqrt((fred/1000)^(2*object$lambda-1)/pmax(object$pop,1))
        #    else
        #      {
        s <- sqrt(abs(object$obs.var))
        #      }
        #    browser()
        ysd <- s*NA
        for(i in 1:ncol(ysd))
            if(sum(!is.na(s[,i])) >= 2) # enough data to fix NAs?
                ysd[,i] <- stats::spline(object$age, s[,i], n=nrow(fcast$var$total))$y
        ysd <- rowMeans(ysd, na.rm = TRUE)

        #    browser()

        # Add observational variance to total variance
        fcast$var$observ <- ysd^2
        fcast$var$total <- sweep(fcast$var$total,1,fcast$var$observ,"+")

        # Correct prediction intervals and reverse transform
        fse <- stats::qnorm(.5 + fcast$coeff[[1]]$level/200) * sqrt(fcast$var$total)
        fcast$upper$y <- InvBoxCox(fcast$mean$y + fse,object$lambda)
        fcast$lower$y <- InvBoxCox(fcast$mean$y - fse,object$lambda)
        fcast$mean$y <- InvBoxCox(fcast$mean$y,object$lambda)
        if(object$type != "migration")
        {
            fcast$mean$y <- pmax(fcast$mean$y, 0.000000001)
            fcast$lower$y <- pmax(fcast$lower$y, 0.000000001)
            fcast$lower$y[is.na(fcast$lower$y)] <- 0
            fcast$upper$y <- pmax(fcast$upper$y, 0.000000001)
        }
        output <- list(
            label=object$label,
            age=object$age,
            year=max(object$year)+(1:h)/stats::frequency(object$year),
            rate=list(forecast=fcast$mean$y,
                      lower=fcast$lower$y,
                      upper=fcast$upper$y),
            error=fcast$error,
            fitted=fcast$fitted,
            coeff=fcast$coeff,
            coeff.error=fcast$coeff.error,
            var=fcast$var,
            model=fcast$model,
            type=object$type,
            lambda=object$lambda)
        names(output$rate)[1] = names(object)[4]
        output$call <- match.call()
        return(structure(output,class=c("fmforecast","demogdata")))
    }



    forecast.ftsm2 <- function (object, h = 10, method = c("ets", "arima", "ar", "ets.na",
                                                           "rwdrift", "rw", "struct", "arfima"), level = 80, jumpchoice = c("fit", "actual"),
                                pimethod = c("parametric", "nonparametric"), B = 100, usedata = nrow(object$coeff),
                                adjust = TRUE, model = NULL, damped = NULL, stationary = FALSE,
                                ...)
    {

        ####################################################################################################
        pegelsna <- function (x, upper = c(alpha = 0.3, beta = 0.2, phi = 0.99),
                              lower = c(alpha = 0.01, beta = 0.01, phi = 0.9), model = "AZN")
        {
            MSE1 <- function(alpha, x) {
                if (alpha > upper[1])
                    return(1e+09)
                if (alpha < 0.01)
                    return(1e+09)
                fit <- Arima(x, order = c(0, 1, 1), fixed = alpha - 1)
                return(fit$sigma2)
            }
            MSE2 <- function(alpha, x) {
                if (any(alpha > upper[1:2]))
                    return(1e+09)
                if (any(alpha < lower[1:2]))
                    return(1e+09)
                theta1 <- alpha[1] + alpha[1] * alpha[2] - 2
                theta2 <- 1 - alpha[1]
                fit <- Arima(x, order = c(0, 2, 2), fixed = c(theta1,
                                                              theta2))
                return(fit$sigma2)
            }
            MSE3 <- function(alpha, x) {
                if (any(alpha > upper))
                    return(1e+09)
                if (any(alpha < lower))
                    return(1e+09)
                if (alpha[3] < alpha[2])
                    return(1e+09)
                theta1 <- alpha[1] + alpha[1] * alpha[2] - 1 - alpha[3]
                theta2 <- (1 - alpha[1]) * alpha[3]
                phi1 <- alpha[3]
                fit <- Arima(x, order = c(1, 1, 2), fixed = c(phi1, theta1,
                                                              theta2))
                return(fit$sigma2)
            }

            n <- length(x)

            if (model == "AZN") {
                fit1 <- nlm(MSE1, (upper[1] + lower[1]) / 2, x = x)
                fit2 <- nlm(MSE2, (upper[1:2] + lower[1:2]) / 2, x = x)
                fit3 <- nlm(MSE3, (upper + lower) / 2, x = x)
                n <- length(x)
                aic <- c(n * log(fit1$minimum) + 2, n * log(fit2$minimum) +
                             4, n * log(fit3$minimum) + 6)
                best <- c("ANN", "AAN", "ADN")[aic == max(aic)]
            }
            else
                best <- model

            if (best == "ANN") {
                fit1 <- nlm(MSE1, (upper[1] + lower[1]) / 2, x = x, stepmax = 10)
                fitarima <- Arima(x, order = c(0, 1, 1), fixed = fit1$estimate -
                                      1)
                fitpar <- c(alpha = fit1$estimate,
                            beta = 0,
                            gamma = 0,
                            phi = 1)
                method <- "Robust SES"
            }
            else if (best == "AAN") {
                fit2 <- nlm(MSE2, (upper[1:2] + lower[1:2]) / 2, x = x)
                theta1 <- fit2$estimate[1] + fit2$estimate[1] * fit2$estimate[2] -
                    2
                theta2 <- 1 - fit2$estimate[1]
                fitarima <- Arima(x, order = c(0, 2, 2), fixed = c(theta1,
                                                                   theta2))
                fitpar <- c(alpha = fit2$estimate[1],
                            beta = fit2$estimate[2],
                            gamma = 0,
                            phi = 1)
                method <- "Robust Holt's"
            }
            else if (best == "ADN") {
                fit3 <- nlm(MSE3, (upper + lower) / 2, x = x)
                theta1 <- fit3$estimate[1] + fit3$estimate[1] * fit3$estimate[2] -
                    1 - fit3$estimate[3]
                theta2 <- (1 - fit3$estimate[1]) * fit3$estimate[3]
                phi1 <- fit3$estimate[3]
                fitarima <- Arima(x, order = c(1, 1, 2), fixed = c(phi1,
                                                                   theta1, theta2))
                fitpar <- c(alpha = fit3$estimate[1],
                            beta = fit3$estimate[2],
                            gamma = 0,
                            phi = fit3$estimate[3])
                method <- "Robust Damped Holt's"
            }
            else stop("Unknown model")

            fitarima$par <- fitpar
            fitarima$method <- method
            return(fitarima)
        }

        struct.forecast <- function(x, h = 10, level = c(80, 95))
        {
            fit1 <- StructTS(x, "level")
            fit2 <- StructTS(x, "trend")
            if (-2 * fit1$loglik < -2 * fit2$loglik + 2)
                fitStruct <- fit1
            else fitStruct <- fit2
            pred <- predict(fitStruct, n.ahead = h)
            nint <- length(level)
            lower <- matrix(NA, ncol = nint, nrow = length(pred$pred))
            upper <- lower
            for (i in 1:nint) {
                qq <- qnorm(0.5 * (1 + level[i]/100))
                lower[, i] <- pred$pred - qq * pred$se
                upper[, i] <- pred$pred + qq * pred$se
            }
            colnames(lower) = colnames(upper) = paste(level, "%", sep = "")
            fits <- rowSums(fitStruct$fitted)
            tsp(fits) <- tsp(x)
            return(structure(list(method = "Structural local linear",
                                  model = fitStruct, level = level, mean = pred$pred, var = pred$se^2,
                                  lower = lower, upper = upper, x = x, fitted = fits),
                             class = "forecast"))
        }


        ftsmPI <- function (object, B, level, h, fmethod = c("ets", "arima"))
        {
            data = object$y$y
            p = nrow(data)
            n = ncol(data)
            ncomp = dim(object$basis)[2] - 1
            mdata = apply(data, 1, mean)
            mdata2 = array(rep(as.matrix(mdata), B * h), dim = c(p, B, h))
            sdata = scale(t(data), scale = FALSE)
            load = as.matrix(svd(sdata)$v[, 1:ncomp])
            sco = sdata %*% load
            olivia = matrix(, ncomp, h)
            if(fmethod == "ets")
            {
                for(i in 1:ncomp)
                {
                    olivia[i, ] = forecast(ets(sco[, i]), h = h)$mean
                }
            }
            if(fmethod == "arima")
            {
                for(i in 1:ncomp)
                {
                    olivia[i, ] = forecast(auto.arima(sco[, i]), h = h)$mean
                }
            }
            forerr = matrix(, (n - ncomp - h + 1), ncomp)
            for(i in h:(n - ncomp))
            {
                k = i + (ncomp - h)
                fore = matrix(, 1, ncomp)
                if(fmethod == "ets")
                {
                    for(j in 1:ncomp)
                    {
                        fore[, j] = forecast(ets(sco[1:k, j]), h = h)$mean[h]
                    }
                }
                if(fmethod == "arima")
                {
                    for(j in 1:ncomp)
                    {
                        fore[, j] = forecast(auto.arima(sco[1:k, j]), h = h)$mean[h]
                    }
                }
                forerr[i - h + 1, ] = sco[k + h, ] - fore
            }
            resi = t(sdata) - load %*% t(sco)
            q = array(NA, dim = c(p, B, h))
            for(j in 1:h)
            {
                for(i in 1:p)
                {
                    q[i, , j] = sample(resi[i, ], size = B, replace = TRUE)
                }
            }
            ny = array(NA, dim = c(ncomp, B, h))
            for(j in 1:h)
            {
                for(i in 1:ncomp)
                {
                    ny[i, , j] = sample(forerr[, i], size = B, replace = TRUE)
                }
            }
            oli = array(rep(olivia, B * h), dim = c(ncomp, B, h))
            fo = array(NA, dim = c(ncomp, B, h))
            for(j in 1:h)
            {
                for(i in 1:B)
                {
                    fo[, i, j] = oli[, i, j] + ny[, i, j]
                }
            }
            pred = array(NA, dim = c(p, B, h))
            for(j in 1:h)
            {
                for(i in 1:B)
                {
                    pred[, i, j] = load %*% fo[, i, j] + mdata2[, i, j] + q[, i, j]
                }
            }
            k1 = k2 = matrix(NA, p, h)
            for(j in 1:h)
            {
                for(i in 1:p)
                {
                    k1[i, j] = quantile(pred[i, , j], (100 - level)/200, na.rm = TRUE)
                    k2[i, j] = quantile(pred[i, , j], 1 - (100 - level)/200, na.rm = TRUE)
                }
            }
            return(list(bootsamp = pred, lb = k1, ub = k2))
        }

        ####################################################################################################

        method <- match.arg(method)
        requireNamespace("rainbow", quietly=TRUE)
        jumpchoice <- match.arg(jumpchoice)
        pimethod <- match.arg(pimethod)
        if (jumpchoice == "actual") {
            var.col <- apply(object$coeff, 2, var)
            idx <- order(var.col)[1]
            if (var.col[idx] > 1e-08)
                stop("No mean function fitted. So jumpchoice cannot be 'actual'")
            trueval <- object$fitted$y + object$residuals$y
            n <- ncol(trueval)
            object$basis[, idx] <- trueval[, n]
            for (i in 1:ncol(object$basis)) {
                if (i != idx)
                    object$coeff[, i] <- object$coeff[, i] - object$coeff[n, i]
            }
        }
        else if (jumpchoice != "fit")
            stop("Unknown jump choice")
        nb <- ncol(object$basis)
        l <- nrow(object$coeff)

        if(method=="ar" | method=="arfima")
            stationary <- TRUE
        if(any(stationary)==TRUE & !(method == "arima" | method == "ar" | method == "arfima"))
            stop("Choose a stationary method")

        meanfcast <- varfcast <- matrix(NA, nrow = h, ncol = nb)
        obs <- fitted <- matrix(NA, nrow = l, ncol = nb)
        qconf <- qnorm(0.5 + level/200)
        usedata <- min(usedata, l)
        ytsp <- tsp(object$coeff)
        x <- xx <- ts(as.matrix(object$coeff[(l - usedata + 1):l,
        ]), start = ytsp[1] + l - usedata, frequency = ytsp[3])
        xx[object$weights[(l - usedata + 1):l] < 0.1, ] <- NA

        fmodels <- list()
        if (method == "ets") {
            if (is.null(model))
                model <- c("ANN", rep("ZZZ", nb - 1))
            else if (length(model) == 1)
                model <- c("ANN", rep(model, nb - 1))
            else if (length(model) == nb - 1)
                model <- c("ANN", model)
            else stop("Length of model does not match number of coefficients")
            if (!is.null(damped)) {
                if (length(damped) == 1)
                    damped <- c(FALSE, rep(damped, nb))
                else if (length(damped) == nb - 1)
                    damped <- c(FALSE, damped)
                else stop("Length of damped does not match number of coefficients")
            }
            for (i in 1:nb) {
                if (!is.null(damped))
                    fmodels[[i]] <- ets(x[, i], model = model[i],
                                        damped = damped[i], ...)
                else fmodels[[i]] <- ets(x[, i], model = model[i],
                                         ...)
                pegelsfit <- forecast(fmodels[[i]], h = h, level = level)
                meanfcast[, i] <- pegelsfit$mean
                varfcast[, i] <- ((pegelsfit$upper[, 1] - pegelsfit$lower[,
                                                                          1])/(2 * qconf[1]))^2
                fitted[, i] <- pegelsfit$fitted
            }
        }
        else if (method == "ets.na") {

            if (is.null(model))
                model <- c("ANN", rep("AZN", nb - 1))
            else if (length(model) == 1)
                model <- c("ANN", rep(model, nb -1))
            else if (length(model) == nb - 1)
                model <- c("ANN", model)
            else stop("Length of model does not match number of coefficients")

            for (i in 1:nb) {
                barima <-  pegelsna(xx[, i], model = model[i])
                fitted[,i] <- fitted(barima)
                pred <- forecast(barima,h=h,level=level)
                fmodels[[i]] <- pred
                meanfcast[,i] <- pred$mean
                varfcast[,i] <- ((pred$upper[,1]-pred$lower[,1])/(2*qconf[1]))^2
            }
        }
        else if (method == "arima") {
            if (length(stationary) == 1)
                stationary <- c(TRUE, rep(stationary, nb))
            else if (length(stationary) == nb - 1)
                stationary <- c(TRUE, stationary)
            else stop("Length of stationary does not match number of coefficients")




            for(i in 1:nb)
            {
                if(var(xx[,i],na.rm=TRUE) < 1e-8)
                {
                    cc <- mean(xx[,i],na.rm=TRUE)
                    fmodels[[i]] <- list("Constant",cc)
                    meanfcast[,i] <- rep(cc,h)
                    varfcast[,i] <- rep(0,h)
                    fitted[,i] <- rep(cc,length(xx[,i]))
                }
                else
                {
                    barima2 <- op.arima(arima_process=c(6,1,6,0,0,0), seasonal_periodicity=12, time_serie=xx[,i],
                                        horiz = h, prop=.8, training_weight=.2, testing_weight=.8,
                                        parallelize = F)

                    texto <- gsub(".*\\(|\\).*", "", barima2$bests$arima_model[1])
                    texto <- paste("forecast::Arima(xx[,i],order=c(", texto, "))", sep="")
                    barima <- eval(parse(text=texto))
                    fitted[,i] <- fitted(barima)
                    pred <- forecast(barima,h=h,level=level)
                    fmodels[[i]] <- pred
                    meanfcast[,i] <- pred$mean
                    varfcast[,i] <- ((pred$upper[,1]-pred$lower[,1])/(2*qconf[1]))^2
                }
            }
        }
        else if (method == "rwdrift") {
            for (i in 1:nb) {
                if(var(xx[,i],na.rm=TRUE) < 1e-8)
                {
                    cc <- mean(xx[,i],na.rm=TRUE)
                    fmodels[[i]] <- list("Constant",cc)
                    meanfcast[,i] <- rep(cc,h)
                    varfcast[,i] <- rep(0,h)
                    fitted[,i] <- rep(cc,length(xx[,i]))
                }
                else
                {
                    barima <-  Arima(xx[,i], include.drift = TRUE,...)
                    fitted[,i] <- fitted(barima)
                    pred <- forecast(barima,h=h,level=level)
                    fmodels[[i]] <- pred
                    meanfcast[,i] <- pred$mean
                    varfcast[,i] <- ((pred$upper[,1]-pred$lower[,1])/(2*qconf[1]))^2
                }
            }
        }
        else if (method == "rw") {
            for (i in 1:nb) {
                if(var(xx[,i],na.rm=TRUE) < 1e-8)
                {
                    cc <- mean(xx[,i],na.rm=TRUE)
                    fmodels[[i]] <- list("Constant",cc)
                    meanfcast[,i] <- rep(cc,h)
                    varfcast[,i] <- rep(0,h)
                    fitted[,i] <- rep(cc,length(xx[,i]))
                }
                else
                {
                    barima <-  Arima(xx[,i], order = c(0,1,0), include.drift = FALSE)
                    fitted[,i] <- fitted(barima)
                    pred <- forecast(barima,h=h,level=level)
                    fmodels[[i]] <- pred
                    meanfcast[,i] <- pred$mean
                    varfcast[,i] <- ((pred$upper[,1]-pred$lower[,1])/(2*qconf[1]))^2
                }
            }
        }
        else if(method=="struct")
        {
            for (i in 1:nb)
            {
                if(var(xx[,i],na.rm=TRUE) < 1e-8)
                {
                    cc <- mean(xx[,i],na.rm=TRUE)
                    meanfcast[,i] <- rep(cc,h)
                    varfcast[,i] <- rep(0,h)
                    fitted[,i] <- rep(cc,length(xx[,i]))
                }
                else
                {
                    fitStruct <- struct.forecast(xx[,i],h=h,level=level,...)
                    meanfcast[,i] <- fitStruct$mean
                    varfcast[,i] <- fitStruct$var
                    fitted[,i] <- fitStruct$fitted
                }
            }
        }
        else if(method=="arfima")
        {
            for(i in 1:nb)
            {
                if(var(xx[,i],na.rm=TRUE) < 1e-8)
                {
                    cc <- mean(xx[,i],na.rm=TRUE)
                    fmodels[[i]] <- list("Constant",cc)
                    meanfcast[,i] <- rep(cc,h)
                    varfcast[,i] <- rep(0,h)
                    fitted[,i] <- rep(cc,length(xx[,i]))
                }
                else
                {
                    barfima <- arfima(xx[,i],...)
                    fitted[,i] <- fitted(barfima)
                    pred <- forecast(barfima,h=h,level=level)
                    fmodels[[i]] <- pred
                    meanfcast[,i] <- pred$mean
                    varfcast[,i] <- ((pred$upper[,1]-pred$lower[,1])/(2*qconf[1]))^2
                }
            }
        }
        else stop("Unknown method")
        ytsp <- tsp(object$fitted$time)
        error <- ts(object$coeff - fitted, start = ytsp[1], frequency = ytsp[3])
        ferror <- onestepfcast <- object$y
        onestepfcast$y <- object$basis %*% t(fitted)
        onestepfcast$yname <- "One step forecasts"
        colnames(onestepfcast$y) = colnames(object$y$y)
        ferror$y <- object$y$y - onestepfcast$y
        ferror$yname <- "One step errors"
        basis_obj_fore = object$basis %*% t(meanfcast)
        colnames(basis_obj_fore) = 1:h
        fmean <- fts(object$y$x, basis_obj_fore, start = ytsp[2] +
                         1/ytsp[3], frequency = ytsp[3], xname = object$y$xname, yname = "Forecasts")
        colnames(fmean$y) = seq(ytsp[2]+1/ytsp[3], ytsp[2]+h/ytsp[3], by=1/ytsp[3])
        res <- object$residuals
        res$y <- res$y^2
        vx <- rowMeans(res$y, na.rm = TRUE)
        modelvar <- object$basis^2 %*% t(varfcast)
        totalvar <- sweep(modelvar, 1, vx + object$mean.se^2, "+")
        if (adjust & nb > 1) {
            adj.factor <- rowMeans(ferror$y^2, na.rm = TRUE)/totalvar[,
                                                                      1]
            totalvar <- sweep(totalvar, 1, adj.factor, "*")
        }
        else adj.factor <- 1
        if (length(qconf) > 1)
            stop("Multiple confidence levels not yet implemented")
        if (pimethod == "parametric") {
            tmp <- qconf * sqrt(totalvar)
            flower <- fts(object$y$x, fmean$y - tmp, start = ytsp[2] +
                              1/ytsp[3], frequency = ytsp[3], xname = object$y$xname, yname = "Forecast lower limit")
            fupper <- fts(object$y$x, fmean$y + tmp, start = ytsp[2] +
                              1/ytsp[3], frequency = ytsp[3], xname = object$y$xname, yname = "Forecast upper limit")
            colnames(flower$y) = colnames(fupper$y) = seq(ytsp[2]+1/ytsp[3], ytsp[2]+h/ytsp[3], by=1/ytsp[3])
            coeff <- list()
            for (i in 1:nb) {
                coeff[[i]] <- structure(list(mean = ts(meanfcast[,
                                                                 i], start = ytsp[2] + 1/ytsp[3], frequency = ytsp[3]), lower = ts(meanfcast[,
                                                                                                                                             i] - qconf * sqrt(varfcast[, i]), start = ytsp[2] + 1/ytsp[3], frequency = ytsp[3]), upper = ts(meanfcast[, i] + qconf * sqrt(varfcast[, i]), start = ytsp[2] + 1/ytsp[3],
                                                                                                                                                                                                                                             frequency = ytsp[3]), level = level, x = x[, i], method = method,
                                             model = fmodels[[i]]), class = "forecast")
            }
            names(coeff) <- paste("Basis", 1:nb)
            return(structure(list(mean = fmean, lower = flower, upper = fupper,
                                  fitted = onestepfcast, error = ferror, coeff = coeff,
                                  coeff.error = error, var = list(model = modelvar,
                                                                  error = vx, mean = object$mean.se^2, total = totalvar,
                                                                  coeff = varfcast, adj.factor = adj.factor), model = object),
                             class = "ftsf"))
        }
        else {
            junk = ftsmPI(object, B = B, level = level, h = h, fmethod = method)
            colnames(junk$lb) = colnames(junk$ub) = seq(ytsp[2]+1/ytsp[3], ytsp[2]+h/ytsp[3], by=1/ytsp[3])
            lb = fts(object$y$x, junk$lb, start = ytsp[2] + 1/ytsp[3], frequency = ytsp[3],
                     xname = object$y$xname, yname = "Forecast lower limit")
            ub = fts(object$y$x, junk$ub, start = ytsp[2] + 1/ytsp[3], frequency = ytsp[3],
                     xname = object$y$xname, yname = "Forecast upper limit")
            return(structure(list(mean = fmean, bootsamp = junk$bootsamp,
                                  lower = lb, upper = ub, model = object), class = "ftsf"))
        }
    }


    ##############################################

    forecast.fdmpr2 <- function(object, h=50, level=80, K=100, drange=c(0.0,0.5), ...)
    {
        fcast.ratio <- fc <- totalvar.r <- list()
        J <- length(object$ratio)
        ny <- length(object$ratio[[1]]$year)
        K <- min(K,ny)

        # GM model
        fcast.mean <- forecast.fdm2(object$product, method="arima", h=h, level=level, ...)
        # Make sure first coefficient is not I(1) with drift.
        #mod <- auto.arima(object$product$coeff[,2],d=2)
        #fcast.mean$coeff[[2]] <- forecast(mod, h=h, level=level, ...)
        #fcast.mean <- update(fcast.mean)

        # Obtain forecasts for each group
        is.mortality <- (object$product$type=="mortality")
        y <- as.numeric(is.mortality) #=1 for mortality and 0 for migration
        for (j in 1:J)
        {
            # Use all available data other than last K years
            # As ARFIMA can't handle missing values
            object$ratio[[j]]$weights <- 0*object$ratio[[j]]$weights +1
            if(K < ny)
                object$ratio[[j]]$weights[1:(ny-K)] <- 0
            fcast.ratio[[j]] <- forecast(object$ratio[[j]], h=h, level=level, method="arfima", estim="mle", drange=drange,...)
            fc[[j]] <- fcast.mean
            if(is.mortality)
                fc[[j]]$rate[[1]] <- fcast.mean$rate$product * fcast.ratio[[j]]$rate[[1]]
            else
                fc[[j]]$rate[[1]] <- fcast.mean$rate$product + fcast.ratio[[j]]$rate[[1]]
            names(fc[[j]]$rate)[1] <- names(object$ratio)[j]
            fc[[j]]$coeff <- fc[[j]]$coeff.error <- fc[[j]]$call <- fc[[j]]$var <- NULL
            if(is.mortality)
                y <- y * fc[[j]]$rate[[1]]
            else
                y <- y + fc[[j]]$rate[[1]]
        }

        # Adjust forecasts so they multiply appropriately.
        if(is.mortality)
        {
            y <- y^(1/J)/fcast.mean$rate$product
            for(j in 1:J)
                fc[[j]]$rate[[1]] <- fc[[j]]$rate[[1]]/y
        }
        else
        {
            y <- y/J - fcast.mean$rate$product
            for(j in 1:J)
                fc[[j]]$rate[[1]] <- fc[[j]]$rate[[1]]-y
        }
        # Variance of forecasts
        qconf <- 2 * stats::qnorm(0.5 + fcast.mean$coeff[[1]]$level/200)
        for (j in 1:J)
        {
            vartotal <- fcast.mean$var$total + fcast.ratio[[j]]$var$total
            tmp <- qconf * sqrt(vartotal)
            fc[[j]]$rate$lower <- InvBoxCox(BoxCox(fc[[j]]$rate[[1]],object$product$lambda) - tmp, object$product$lambda)
            fc[[j]]$rate$upper <- InvBoxCox(BoxCox(fc[[j]]$rate[[1]],object$product$lambda) + tmp, object$product$lambda)
            if(is.mortality)
            {
                fc[[j]]$model[[4]] <- BoxCox(InvBoxCox(object$product[[4]],object$product$lambda) *
                                                 InvBoxCox(object$ratio[[j]][[4]], object$product$lambda),object$product$lambda)
            }
            else
            {
                fc[[j]]$model[[4]] <- BoxCox(InvBoxCox(object$product[[4]],object$product$lambda) +
                                                 InvBoxCox(object$ratio[[j]][[4]], object$product$lambda),object$product$lambda)
            }
            names(fc[[j]]$model)[4] <- names(object$ratio)[j]
            fc[[j]]$coeff <- list(list(level= fcast.mean$coeff[[1]]$level))
        }

        names(fc) <- names(fcast.ratio) <- names(object$ratio)
        fc$product <- fcast.mean
        fc$ratio <- fcast.ratio

        return(structure(fc, class="fmforecast2"))
    }

    ####################################################
    netmigration_projected <- forecast.fdmpr2(object = migration_model, h=horizon)
    netmigration_projected

}
