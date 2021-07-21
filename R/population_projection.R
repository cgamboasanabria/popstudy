#' population_projection
#'
#' Forecasting population
#'
#' @param ... PENDIENTE
#'
#' @return \code{population_projection} PENDIENTE
#'
#' @examples
#'
#' ## PENDIENTE
#'
#' @author Cesar Gamboa-Sanabria
#'
#' @export
population_projection <- function(...){
    fun_args <- stackoverflow::match.call.defaults(expand.dots = TRUE)[-1] %>% as.list

    mortality_rates <- read.demogdata(file = fun_args$mortality_rates_path,
                                      popfile = fun_args$total_population_path, type = "mortality",
                                      max.mx = 1, skip = 0, popskip = 0, label = "CR")
    final_rates <- extract.years(data = mortality_rates, years = min(mortality_rates$year):(fun_args$first_year_projection))
    final_rates <- set.upperage(data = final_rates, max.age = fun_args$omega_age)

    horizonte2 <-fun_args$horizon
    horizon <- fun_args$horizon-max(final_rates$year)

    mortality_projected <- mortality_projection(...)
    TFR_projected <- TFR_projection(...)
    proy.mig <- netmigration_projection(...)
    suppressWarnings({
        poblaciones <- pop.sim(mort = mortality_projected, fert = TFR_projected, mig = proy.mig,
                               firstyearpop = final_rates, bootstrap = TRUE, N = 500, mfratio = 1.0545)})

    ####Pob hombres
    hombres_estimacion <- final_rates$pop$male%>%
        as.data.frame() %>%
        mutate(edad=row.names(.),
               sexo="Male") %>%
        gather(año, poblacion, -c(sexo, edad)) %>%
        mutate(LI=NA,
               LS=NA)

    hombres <- apply(poblaciones$male, 1:2, quantile, probs=.5, na.rm=TRUE) %>%
        as.data.frame() %>%
        mutate(edad=row.names(.),
               sexo="Male") %>%
        gather(año, poblacion, -c(sexo, edad))

    LI <- apply(poblaciones$male, 1:2, quantile, probs=.1, na.rm=TRUE) %>%
        as.data.frame() %>%
        mutate(edad=row.names(.),
               sexo="Male") %>%
        gather(año, LI, -c(sexo, edad))

    LS <- apply(poblaciones$male, 1:2, quantile, probs=.9, na.rm=TRUE) %>%
        as.data.frame() %>%
        mutate(edad=row.names(.),
               sexo="Male") %>%
        gather(año, LS, -c(sexo, edad))

    hombres <- merge(hombres, LI, by=c("edad", "sexo", "año")) %>%
        merge(., LS, by=c("edad", "sexo", "año"))

    hombres <- do.call(rbind, list(hombres, hombres_estimacion))

    ####Pob mujeres
    mujeres_estimacion <- final_rates$pop$female%>%
        as.data.frame() %>%
        mutate(edad=row.names(.),
               sexo="Female") %>%
        gather(año, poblacion, -c(sexo, edad)) %>%
        mutate(LI=NA,
               LS=NA)

    mujeres <- apply(poblaciones$female, 1:2, quantile, probs=.5, na.rm=TRUE) %>%
        as.data.frame()
    colnames(mujeres) <- paste(c(fun_args$first_year_projection:horizonte2))
    mujeres <- mujeres %>%
        mutate(edad=row.names(.),
               sexo="Female") %>%
        gather(año, poblacion, -c(sexo, edad))

    LI <- apply(poblaciones$female, 1:2, quantile, probs=.1, na.rm=TRUE) %>%
        as.data.frame()
    colnames(LI) <- c(fun_args$first_year_projection:horizonte2)
    LI <- LI %>%
        mutate(edad=row.names(.),
               sexo="Female") %>%
        gather(año, LI, -c(sexo, edad))

    LS <- apply(poblaciones$female, 1:2, quantile, probs=.9, na.rm=TRUE) %>%
        as.data.frame()
    colnames(LS) <- c(fun_args$first_year_projection:horizonte2)
    LS <- LS %>%
        mutate(edad=row.names(.),
               sexo="Female") %>%
        gather(año, LS, -c(sexo, edad))

    mujeres <- merge(mujeres, LI, by=c("edad", "sexo", "año")) %>%
        merge(., LS, by=c("edad", "sexo", "año"))

    mujeres <- do.call(rbind, list(mujeres, mujeres_estimacion))

    poblaciones_edades_simples <- do.call(rbind, list(hombres, mujeres)) %>%
        mutate(edad=as.numeric(edad),
               año=as.numeric(año)) %>%
        arrange(año, edad)

    poblaciones <- poblaciones_edades_simples %>%
        dplyr::select(edad:poblacion) %>%
        data.frame() %>%
        pivot_wider(names_from=sexo, values_from=poblacion, values_fn = list(poblacion=mean)) %>%
        rename(Age=edad, Year=año) %>%
        dplyr::select(Year, Age, Female, Male)

    list(mort = mortality_projected, fert = TFR_projected, mig = proy.mig, poblaciones = poblaciones)
}
