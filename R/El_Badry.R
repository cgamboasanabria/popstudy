#' El-Badry method
#'
#' The method corrects the zero parity omission error.
#'
#' @param data data.drame. It contains at least three variables: five-year grouped ages, number of childs and Children Ever Born (CEB).
#'
#' @param age variable name in \code{data} of the five-year grouped age.
#'
#' @param CEB variable name in \code{data} with number of Children Ever Born .
#'
#' @param childs variable name in \code{data} with the number of childs for each five-year grouped age and number of Children Ever Born.
#'
#' @param req_ages optional character string that specifies the five-year grouped age to estimates the intercept.
#'
#' @return \code{Moultrie} returns a list with two elements: a data.frame with corrected children for each number of Children Ever Born and five-year grouped ages and a data.frame with combinations of five-year grouped age to estimate intercept, slope, and R-squared. By default, the method uses the best value of R-squared to apply the El Badry correction.
#'
#' @examples
#'
#' CEB_data <- tidyr::gather(CEB, ages, childs, -Children_Ever_Born)
#' results <- Moultrie(CEB_data, ages, childs, Children_Ever_Born)
#' CEB_data <- tidyr::pivot_wider(results, names_from=age, values_from=childs)
#' CEB_data <- tidyr::gather(CEB_data, ages, children, -CEB)
#' El_Badry(CEB_data,ages, CEB, children)
#'
#'
#' @author Cesar Gamboa-Sanabria
#'
#' @seealso
#'
#' \code{\link{CEB}}
#' \code{\link{Moultrie}}
#'
#' @references
#'
#' \insertRef{moultrie}{popstudy}
#'
#' @export
El_Badry <- function(data, age, CEB, childs, req_ages=NULL){

    match.call.defaults <- function(definition = sys.function(sys.parent()),
                                    call = sys.call(sys.parent()),
                                    expand.dots = TRUE,
                                    envir = parent.frame(2L)) {
        call <- match.call(definition, call, expand.dots, envir)
        formals <- formals(definition)

        if(expand.dots && '...' %in% names(formals))
            formals[['...']] <- NULL

        for(i in setdiff(names(formals), names(call)))
            call[i] <- list( formals[[i]] )


        match.call(definition, call, TRUE, envir)
    }

    vars <- match.call.defaults(expand.dots = TRUE)[-1] %>% as.list
    data <- data %>%
        select(vars$age, vars$CEB, vars$childs)
    names(data) <- c("age", "CEB", "childs")
    data <- data %>%
        mutate(CEB=ifelse(grepl("[A-Za-z]", CEB) | CEB=="99", "Unknown", CEB),
               childs=ifelse(is.na(childs), 0, childs))
    data2 <- data %>%
        mutate(Unknown=ifelse(CEB=="Unknown", childs, 0),
               Zi=ifelse(CEB=="0", childs, 0)) %>%
        group_by(age) %>%
        summarise(Unknown=sum(Unknown),
                  childs=sum(childs),
                  Zi=sum(Zi)) %>%
        ungroup() %>%
        mutate(Ui=Unknown/childs,
               Zi=Zi/childs)

    combns <- lapply(1:nrow(data2), function(x){
        combn(data2$age, x) %>%
            as.data.frame
    })

    bests <- lapply(combns, function(x){
        lapply(1:ncol(x), function(y){
            mod <- data2 %>%
                filter(age %in% x[,y]) %>%
                lm(Ui~Zi, data=.)
            data.frame(ages=paste(x[,y], collapse = ", "),
                       beta0=coef(mod)[1],
                       beta1=coef(mod)[2],
                       R2=summary(mod)$r.squared)
        }) %>%
            do.call(rbind, .)
    }) %>%
        do.call(rbind, .) %>%
        data.frame(row.names = NULL) %>%
        na.omit %>%
        filter(R2<1) %>%
        arrange(desc(R2))
    if(!is.null(req_ages)){
        bests <- bests %>%
            filter(ages==req_ages)
    }
    bests <- bests %>%
        separate(ages, into=paste("G", 1:nrow(data2), sep=""), sep=",")

    beta0 <- bests$beta0[1]
    beta1 <- bests$beta1[2]
    R2 <- bests$R2[1]

    list(results=do.call(rbind, list({
        data %>%
            pivot_wider(names_from=age, values_from=childs) %>%
            filter(CEB!="Unknown")
    },
    {
        data2 %>%
            mutate(Niu=childs*beta0,
                   Ni0=childs*(Zi+Ui-beta0)) %>%
            gather(CEB, value, -age) %>%
            pivot_wider(names_from=age, values_from=value) %>%
            mutate_if(is.numeric, round, 4)
    })),
    bests=bests)
}
