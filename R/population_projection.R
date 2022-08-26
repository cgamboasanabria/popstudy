#' population_projection
#'
#' Forecasting population using the components method.
#'
#' @param ... required arguments for \code{\link{mortality_projection}}, \code{\link{TFR_projection}} and \code{\link{netmigration_projection}}.
#'
#' @return \code{population_projection} returns an object of class \code{list} with the following components:
#'
#' \item{mort}{mortality projections from \code{\link{mortality_projection}}.}
#' \item{fert}{fertility projections from \code{\link{TFR_projection}}.}
#' \item{mig}{netmigration projections from \code{\link{netmigration_projection}}.}
#' \item{pop}{the national projections by sex and year.}
#'
#' @seealso
#'
#' \code{\link{mortality_projection}}
#' \code{\link{TFR_projection}}
#' \code{\link{netmigration_projection}}
#'
#' @examples
#'
#' \donttest{
#' \dontrun{
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
#' #result <- population_projection(mortality_rates_path = "CR_mortality_rates_1950_2011.txt",
#' #total_population_path = "CR_populations_1950_2011.txt",
#' #TFR_path = "CR_fertility_rates_1950_2011.txt",
#' #WRA_path = "CR_women_childbearing_age_1950_2011.txt",
#' #omega_age = 115, first_year_projection = 2011, horizon = 2020)
#'
#'}
#' }
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
        gather(periodo, poblacion, -c(sexo, edad)) %>%
        mutate(LI=NA,
               LS=NA)

    hombres <- apply(poblaciones$male, 1:2, quantile, probs=.5, na.rm=TRUE) %>%
        as.data.frame() %>%
        mutate(edad=row.names(.),
               sexo="Male") %>%
        gather(periodo, poblacion, -c(sexo, edad))

    LI <- apply(poblaciones$male, 1:2, quantile, probs=.1, na.rm=TRUE) %>%
        as.data.frame() %>%
        mutate(edad=row.names(.),
               sexo="Male") %>%
        gather(periodo, LI, -c(sexo, edad))

    LS <- apply(poblaciones$male, 1:2, quantile, probs=.9, na.rm=TRUE) %>%
        as.data.frame() %>%
        mutate(edad=row.names(.),
               sexo="Male") %>%
        gather(periodo, LS, -c(sexo, edad))

    hombres <- merge(hombres, LI, by=c("edad", "sexo", "periodo")) %>%
        merge(., LS, by=c("edad", "sexo", "periodo"))

    hombres <- do.call(rbind, list(hombres, hombres_estimacion))

    ####Pob mujeres
    mujeres_estimacion <- final_rates$pop$female%>%
        as.data.frame() %>%
        mutate(edad=row.names(.),
               sexo="Female") %>%
        gather(periodo, poblacion, -c(sexo, edad)) %>%
        mutate(LI=NA,
               LS=NA)

    mujeres <- apply(poblaciones$female, 1:2, quantile, probs=.5, na.rm=TRUE) %>%
        as.data.frame()
    colnames(mujeres) <- paste(c(fun_args$first_year_projection:horizonte2))
    mujeres <- mujeres %>%
        mutate(edad=row.names(.),
               sexo="Female") %>%
        gather(periodo, poblacion, -c(sexo, edad))

    LI <- apply(poblaciones$female, 1:2, quantile, probs=.1, na.rm=TRUE) %>%
        as.data.frame()
    colnames(LI) <- c(fun_args$first_year_projection:horizonte2)
    LI <- LI %>%
        mutate(edad=row.names(.),
               sexo="Female") %>%
        gather(periodo, LI, -c(sexo, edad))

    LS <- apply(poblaciones$female, 1:2, quantile, probs=.9, na.rm=TRUE) %>%
        as.data.frame()
    colnames(LS) <- c(fun_args$first_year_projection:horizonte2)
    LS <- LS %>%
        mutate(edad=row.names(.),
               sexo="Female") %>%
        gather(periodo, LS, -c(sexo, edad))

    mujeres <- merge(mujeres, LI, by=c("edad", "sexo", "periodo")) %>%
        merge(., LS, by=c("edad", "sexo", "periodo"))

    mujeres <- do.call(rbind, list(mujeres, mujeres_estimacion))

    poblaciones_edades_simples <- do.call(rbind, list(hombres, mujeres)) %>%
        mutate(edad=as.numeric(edad),
               periodo=as.numeric(periodo)) %>%
        arrange(periodo, edad)

    poblaciones <- poblaciones_edades_simples %>%
        dplyr::select(edad:poblacion) %>%
        data.frame() %>%
        pivot_wider(names_from=sexo, values_from=poblacion, values_fn = list(poblacion=mean)) %>%
        rename(Age=edad, Year=periodo) %>%
        dplyr::select(Year, Age, Female, Male)

    list(mort = mortality_projected, fert = TFR_projected, mig = proy.mig, pop = poblaciones)
}
