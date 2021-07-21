#' TFR_projection
#'
#' Forecasting total fertility rates.
#'
#' @param TFR_path PENDIENTE
#'
#' @param WRA_path PENDIENTE
#'
#' @param horizon PENDIENTE
#'
#' @param first_year_projection PENDIENTE
#'
#' @param ... PENDIENTE
#'
#' @return \code{TFR_projection} PENDIENTE
#'
#' @examples
#'
#' ## PENDIENTE
#'
#' @author Cesar Gamboa-Sanabria
#'
#' @export
TFR_projection <- function(TFR_path, WRA_path,
                           horizon, first_year_projection, ...){

    TFR <- read.demogdata(file = TFR_path, popfile = WRA_path,
                          type = "fertility", max.mx = 1000,  skip = 0,
                          popskip = 0, label = "CR")

    final_rates <- extract.years(data = TFR,
                                 years = min(TFR$year):(first_year_projection-1))

    horizon <- horizon-max(final_rates$year)

    smoothed_TFR <- extract.years(TFR, min(TFR$year):(first_year_projection-1))
    smoothed_TFR <- smooth.demogdata(data = smoothed_TFR)

    pos <- which(apply(smoothed_TFR$rate$female, 2, function(x) sum(is.na(x)))>0)
    if(length(pos)>0){
        for(i in pos){

            smoothed_TFR$rate$female[,i] <- smoothed_TFR$rate$female[,pos-1]*(colSums(smoothed_TFR$rate$female)[i-1]/colSums(smoothed_TFR$rate$female)[i-2])
        }
    }

    ##########OJO REVISAR ESTO!!!!
    # if(ajuste_suavizadas==TRUE){
    #     smoothed_TFR$rate$female[,61] <- smoothed_TFR$rate$female[,60]*(1905/1938)
    # }#########

    fertility_model <- fdm(smoothed_TFR, series="female")

    ############versión con bests arimas

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
        method <- match.arg(method)
        require(rainbow)
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



    ###################fin de versión con bests arimas###########################

    TFR_projected <- forecast.fdm2(object = fertility_model, h = horizon, jumpchoice = "actual", method = "arima")

    TFR_projected

}
