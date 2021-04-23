#' op.arima
#'
#' Estimates the best ARIMA model using overparameterization.
#'
#' @param arima_process PENDIENTE
#'
#' @param seasonal_periodicity PENDIENTE
#'
#' @param time_serie PENDIENTE
#'
#' @param reg PENDIENTE
#'
#' @param horiz PENDIENTE
#'
#' @param prop PENDIENTE
#'
#' @param training_weight PENDIENTE
#'
#' @param testing_weight PENDIENTE
#'
#' @param parallelize PENDIENTE
#'
#' @param clusters PENDIENTE
#'
#' @param ... PENDIENTE
#'
#' @return \code{correlate_df} function returns a list with three objects: A data-frame with the correlation matrix and two correlation plots.
#'
#' @examples
#'
#' #PENDIENTE
#'
#' @details PENDIENTE
#'
#' @author Cesar Gamboa-Sanabria
#'
#' @references
#'
#'
#'
#' @export
op.arima <- function(arima_process = c(p = 1, d = 1, q = 1,
                                       P = 1, D = 1, Q = 1),
                     seasonal_periodicity,
                     time_serie, reg = NULL, horiz = 12,
                     prop=.8, training_weight=.2, testing_weight=.8,
                     parallelize=FALSE, clusters=detectCores(logical = FALSE),...){

    data_partition <- round(length(time_serie)*prop, 0)
    train <<- subset(time_serie, end=data_partition)
    test <<- subset(time_serie, start=data_partition+1)

    ####################################################################################
    arima_model <- function(time_serie, non_seasonal, seasonal, periodic, regr = NULL,...){

        if(is.list(non_seasonal)){
            non_seasonal <- unlist(non_seasonal)
        }

        if(is.list(seasonal)){
            seasonal <- unlist(seasonal)
        }

        seasonal_part <- list(order=seasonal, period=periodic)
        if(is.null(regr)){
            arima_model <- tryCatch({
                Arima(time_serie,
                      order = non_seasonal,
                      seasonal = seasonal_part,...)
            },
            error = function(e) NULL)
        }

        if(!is.null(regr)){
            arima_model <-tryCatch({
                Arima(time_serie,
                      order = non_seasonal,
                      seasonal = seasonal_part,
                      xreg = regr,...)
            },
            error = function(e) NULL)
        }

        if(!is.null(arima_model)){
            degrees_of_freedom <- arima_model$nobs - length(arima_model$coef)
            t_value <- arima_model$coef/sqrt(diag(arima_model$var.coef))
            prob <- stats::pf(t_value^2, df1 = 1, df2 = degrees_of_freedom, lower.tail = FALSE)
            ifelse(sum(1*prob>0.05)<1, return(arima_model), 1)
        }

    }
    ############################################################################################
    #Estimación de las final_measures de model_performance y los criterios de información
    arima_measures <- function(arima_model, testing, horizon, regr = NULL){

        ##model_spec del arima_model
        model_spec <- capture.output(arima_model)
        model_spec <- substr(model_spec[2],1, 23)

        ##Criterios de model_info
        data <- capture.output(summary(arima_model))
        data <- data[grepl("AIC", data) == T]
        model_info <- strsplit(data, " ")
        pos <- which(sapply(model_info, nchar)>0)
        model_info <- model_info[[1]][pos]
        model_info <- do.call("rbind", strsplit(model_info, "=")) %>%
            data.frame()
        colnames(model_info) <- c("Medida", "Valor")
        model_info <- model_info %>%
            mutate(Valor = as.numeric(as.character(Valor))) %>%
            spread(Medida, Valor) %>%
            data.frame(arima_model = model_spec)

        ##final_measures de model_performance
        model_performance <- data.frame(arima_model = c(model_spec, paste(model_spec, "Validacion")),
                                        accuracy(forecast(arima_model, horizon, xreg = regr), testing))
        merge(model_info, model_performance, by="arima_model", all = TRUE) %>%
            select(arima_model, AIC, AICc, BIC, MAE, RMSE, MASE)
    }
    ############################################################################################

    arima_selected <- function(model_table, Wtrain=training_weight, Wtest=testing_weight){

        #Elimina posibles arima_models duplicados
        model_table <- model_table %>%
            distinct(arima_model, .keep_all = TRUE)

        #Crea variable para agrupar arima_models en train y test
        model_table <- model_table %>%
            mutate(mod = as.character(c(0, rep(1:(nrow(model_table)-1)%/%2))))

        #Crea la variable de puntaje ponderado
        tabla2 <- model_table %>%
            mutate_at(vars(contains("C")), function(x){x-min(x, na.rm=TRUE)}) %>%
            mutate_if(is.numeric, function(x) ifelse(is.na(x),0,x)) %>%
            mutate(puntaje = AIC+AICc+BIC+MAE+RMSE+MASE,
                   ponde = ifelse(grepl("Validacion", arima_model)==TRUE, Wtest, Wtrain),
                   puntaje = puntaje*ponde)

        #Busca el(los) puntaje(s) mínimo(s)
        suppressMessages({
            minimal_score <- tabla2 %>%
                group_by(mod) %>%
                summarise(puntaje=sum(puntaje)) %>%
                ungroup
        })

        #Obtiene la(s) posición(es) con puntaje(s) más bajo(s)
        pos <- minimal_score$mod[which(minimal_score$puntaje==min(minimal_score$puntaje))]

        #Devuelve el(los) mejor(es) arima_model(s)
        model_table %>%
            filter(mod %in% pos) %>%
            dplyr::select(arima_model:MASE)
    }

    ############################################################################################
    suppressWarnings({
        #Matriz para la identificacion de los parametros del arima_model
        valores <- expand.grid(p = 0:arima_process[1], d = 0:arima_process[2], q = 0:arima_process[3],
                               P = 0:arima_process[4], D = 0:arima_process[5], Q = 0:arima_process[6])

        non_seasonal_values <- split(as.matrix(valores[, 1:3]),
                                     row(valores[, 1:3]))
        seasonal_values <- split(as.matrix(valores[, 4:6]),
                                 row(valores[, 4:6]))

        #Estimación de los arima_models
        if(parallelize==FALSE){
            arima_models <- mapply(arima_model,
                                   non_seasonal=non_seasonal_values,
                                   seasonal=seasonal_values,
                                   MoreArgs = list(time_serie=train, regr=reg, periodic=seasonal_periodicity),
                                   SIMPLIFY = FALSE)
        }else({

            #Nombres de los objetos a exportar a cada cluster

            # obj <- sapply(as.list(match.call()), paste)
            # pos <- which(names(obj) %in% c("time_serie", "validacion"))

            #Generar los cluster
            clp <- makeCluster(clusters, type = "SOCK", useXDR=FALSE)
            #clusterExport(clp, varlist = c(obj[pos], "arima_measures", "arima_model"))
            clusterEvalQ(clp, expr = {
                library(forecast)
            })

            #Proceso en parallelize
            arima_models <- clusterMap(cl=clp, fun = arima_model,
                                       non_seasonal=non_seasonal_values,
                                       seasonal=seasonal_values,
                                       MoreArgs = list(time_serie=train, regr=reg, periodic=seasonal_periodicity),
                                       SIMPLIFY = FALSE, .scheduling = "dynamic")
            stopCluster(clp)

        })

        #Obtiene las posiciones de la lista donde se pudieron estimar los arima_models
        pos <- which(sapply(lapply(arima_models, class), length)>1)

        #Calcula las final_measures de model_performance y los criterios de información
        final_measures <- do.call("rbind", lapply(arima_models[pos],
                                                  arima_measures,
                                                  testing = test,
                                                  horizon= horiz,
                                                  regr = reg)) %>%
            mutate_if(is.numeric, round, 2)
        #Devuelve una lista con los arima_models y las final_measures de model_performance
        final_list <- list(arima_models=arima_models[pos],
                           final_measures=final_measures,
                           bests=arima_selected(final_measures, Wtrain = training_weight, Wtest = testing_weight))

        mod_index <- final_list$bests %>%
            row.names %>%
            as.numeric %>%
            floor %>%
            unique %>%
            as.character

        final_list$best_model <- eval(parse(text = paste0("final_list$arima_models$", "`", mod_index, "`")))

        final_list
    })
}
