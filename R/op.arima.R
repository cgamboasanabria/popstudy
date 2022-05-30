#' op.arima
#'
#' Estimates the best predictive ARIMA model using overparameterization.
#'
#' @param arima_process numeric. The ARIMA(p,d,q)(P,D,Q) process.
#'
#' @param seasonal_periodicity numeric. The seasonal periodicity, 12 for monthly data.
#'
#' @param time_serie ts. The univariate time series object to estimate the models.
#'
#' @param reg Optionally, a vector or matrix of external regressors, which must have the same number of rows as time_serie.
#'
#' @param horiz numeric. The forecast horizon.
#'
#' @param prop numeric. Data proportion for training dataset.
#'
#' @param training_weight numeric. Importance weight for the goodness of fit and precision measures in the training dataset.
#'
#' @param testing_weight numeric. Importance weight for the goodness of fit and precision measures in the testing dataset.
#'
#' @param parallelize logical. If TRUE, then use parallel processing.
#'
#' @param clusters numeric. The number of clusters for the parallel process.
#'
#' @param LAMBDA Optionally. See \code{\link[forecast:Arima]{forecast::Arima()}} for details.
#'
#' @param ISP numeric. Overparameterization indicator to filter the estimated models in the (0,100] interval.
#'
#' @param ... additional arguments to be passed to \code{\link[forecast:Arima]{forecast::Arima()}}.
#'
#' @return \code{op.arima} function returns a list with the estimated models, the best model and their measures.
#'
#' @examples
#'
#' \donttest{
#'
#' op.arima(arima_process = c(6,1,6,6,1,6), time_serie = AirPassengers, seasonal_periodicity = 12)
#'
#' }
#'
#' @author Cesar Gamboa-Sanabria
#'
#' @references
#'
#'
#'
#' @export
op.arima <- function (arima_process = c(
    p = 1,
    d = 1,
    q = 1,
    P = 1,
    D = 1,
    Q = 1
),
seasonal_periodicity,
time_serie,
reg = NULL,
horiz = 12,
prop = 0.8,
training_weight = 0.2,
testing_weight = 0.8,
parallelize = FALSE,
clusters = detectCores(logical = FALSE),
LAMBDA = NULL,
ISP = 100,
...)
{
    data_partition <- round(length(time_serie) * prop, 0)
    train <<- subset(time_serie, end = data_partition)
    test <<- subset(time_serie, start = data_partition + 1)
    arima_model <- function(time_serie,
                            non_seasonal,
                            seasonal,
                            periodic,
                            regr = reg,
                            lambda = LAMBDA,
                            ...) {
        if (is.list(non_seasonal)) {
            non_seasonal <- unlist(non_seasonal)
        }
        if (is.list(seasonal)) {
            seasonal <- unlist(seasonal)
        }
        seasonal_part <- list(order = seasonal, period = periodic)
        if (is.null(regr)) {
            arima_model <- tryCatch({
                Arima(
                    time_serie,
                    order = non_seasonal,
                    seasonal = seasonal_part,
                    lambda = LAMBDA,
                    ...
                )
            }, error = function(e)
                NULL)
        }
        if (!is.null(regr)) {
            arima_model <- tryCatch({
                Arima(
                    time_serie,
                    order = non_seasonal,
                    seasonal = seasonal_part,
                    xreg = regr,
                    lambda = LAMBDA,
                    ...
                )
            }, error = function(e)
                NULL)
        }
        if (!is.null(arima_model)) {
            degrees_of_freedom <- arima_model$nobs - length(arima_model$coef)
            t_value <-
                arima_model$coef / sqrt(diag(arima_model$var.coef))
            prob <-
                stats::pf(
                    t_value ^ 2,
                    df1 = 1,
                    df2 = degrees_of_freedom,
                    lower.tail = FALSE
                )
            ifelse(sum(1 * prob > 0.05) < 1, return(arima_model),
                   1)
        }
    }
    arima_measures <- function(arima_model, testing, horizon,
                               regr = NULL) {
        model_spec <- capture.output(arima_model)
        model_spec <- substr(model_spec[2], 1, 23)
        data <- capture.output(summary(arima_model))
        data <- data[grepl("AIC", data) == T]
        model_info <- strsplit(data, " ")
        pos <- which(sapply(model_info, nchar) > 0)
        model_info <- model_info[[1]][pos]
        model_info <-
            do.call("rbind", strsplit(model_info, "=")) %>%
            data.frame()
        colnames(model_info) <- c("Medida", "Valor")
        model_info <-
            model_info %>% mutate(Valor = as.numeric(as.character(Valor))) %>%
            spread(Medida, Valor) %>% data.frame(arima_model = model_spec)
        model_performance <- data.frame(arima_model = c(model_spec,
                                                        paste(model_spec, "Validacion")),
                                        accuracy(forecast(arima_model,
                                                          horizon, xreg = regr), testing))
        merge(model_info,
              model_performance,
              by = "arima_model",
              all = TRUE) %>% select(arima_model, AIC, AICc, BIC,
                                     MAE, RMSE, MASE)
    }
    arima_selected <-
        function(model_table,
                 Wtrain = training_weight,
                 Wtest = testing_weight) {
            model_table <- model_table %>% distinct(arima_model,
                                                    .keep_all = TRUE)
            model_table <-
                model_table %>% mutate(mod = as.character(c(0,
                                                            rep(
                                                                1:(nrow(model_table) - 1) %/% 2
                                                            ))))
            tabla2 <- model_table %>% mutate_at(vars(contains("C")),
                                                function(x) {
                                                    x - min(x, na.rm = TRUE)
                                                }) %>% mutate_if(is.numeric, function(x)
                                                    ifelse(is.na(x),
                                                           0, x)) %>% mutate(
                                                               puntaje = AIC + AICc + BIC + MAE +
                                                                   RMSE + MASE,
                                                               ponde = ifelse(grepl("Validacion", arima_model) ==
                                                                                  TRUE, Wtest, Wtrain),
                                                               puntaje = puntaje * ponde
                                                           )
            suppressMessages({
                minimal_score <-
                    tabla2 %>% group_by(mod) %>% summarise(puntaje = sum(puntaje)) %>%
                    ungroup
            })
            pos <- minimal_score$mod[which(minimal_score$puntaje ==
                                               min(minimal_score$puntaje))]
            model_table %>% filter(mod %in% pos) %>% dplyr::select(arima_model:MASE)
        }
    suppressWarnings({
        valores <-
            expand.grid(
                p = 0:arima_process[1],
                d = 0:arima_process[2],
                q = 0:arima_process[3],
                P = 0:arima_process[4],
                D = 0:arima_process[5],
                Q = 0:arima_process[6]
            )

        pesos <-
            rep(c(no_estacional = 7.424944, estacional = 36.043200),
                each = 2)

        valores <- valores %>%
            mutate(
                isp = apply(
                    dplyr::select(valores, p, q, P, Q),
                    1,
                    weighted.mean,
                    pesos
                ),
                isp = scales::rescale(isp, to = c(0, 100))
            ) %>%
            filter(isp <= ISP) %>%
            dplyr::select(-isp)


        non_seasonal_values <- split(as.matrix(valores[, 1:3]),
                                     row(valores[, 1:3]))
        seasonal_values <-
            split(as.matrix(valores[, 4:6]), row(valores[,
                                                         4:6]))
        if (parallelize == FALSE) {
            arima_models <-
                mapply(
                    arima_model,
                    non_seasonal = non_seasonal_values,
                    seasonal = seasonal_values,
                    MoreArgs = list(
                        time_serie = train,
                        regr = reg,
                        periodic = seasonal_periodicity
                    ),
                    SIMPLIFY = FALSE
                )
        }
        else
            ({
                clp <- makeCluster(clusters, type = "SOCK", useXDR = FALSE)
                clusterEvalQ(clp, expr = {
                    library(forecast)
                })
                arima_models <- clusterMap(
                    cl = clp,
                    fun = arima_model,
                    non_seasonal = non_seasonal_values,
                    seasonal = seasonal_values,
                    MoreArgs = list(
                        time_serie = train,
                        regr = reg,
                        periodic = seasonal_periodicity
                    ),
                    SIMPLIFY = FALSE,
                    .scheduling = "dynamic"
                )
                stopCluster(clp)
            })
        pos <- which(sapply(lapply(arima_models, class), length) >
                         1)
        final_measures <-
            do.call("rbind",
                    lapply(arima_models[pos],
                           function(x) {
                               tryCatch(
                                   expr = {
                                       arima_measures(
                                           x,
                                           testing = test,
                                           horizon = horiz,
                                           regr = reg
                                       )
                                   },
                                   error = function(e)
                                       data.frame(
                                           arima_model = NA,
                                           AIC = NA,
                                           AICc = NA,
                                           BIC = NA,
                                           MAE = NA,
                                           RMSE = NA,
                                           MASE = NA
                                       )
                               )
                           })) %>% mutate_if(is.numeric, round, 2)
        final_list <- list(
            arima_models = arima_models[pos],
            final_measures = final_measures,
            bests = arima_selected(final_measures,
                                   Wtrain = training_weight, Wtest = testing_weight)
        )
        mod_index <-
            final_list$bests %>% row.names %>% as.numeric %>%
            floor %>% unique %>% as.character
        final_list$best_model <-
            eval(parse(
                text = paste0("final_list$arima_models$",
                              "`", mod_index, "`")
            ))
        final_list
    })
}
