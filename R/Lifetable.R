#' Life Table
#'
#' Estimates a lifetable from mortality rates and population data.
#'
#' @param rates character. A character string that specifies mortality data path. The dataset is a .txt file like \code{\link{CR_mortality_rates_2010_2015}}.
#'
#' @param pops character. A character string that specifies population data path. The dataset is a .txt file like \code{\link{CR_populations_1950_2015}}.
#'
#' @param sex character. "female" or "male".
#'
#' @param max_age numeric. Desire omega age. If \code{NULL}, \code{Lifetable} function takes the dataset's maximum age.
#'
#' @param first_year numeric. First year to start estimation.
#'
#' @param threshold numeric. Maximum forecast year.
#'
#' @param jump character. Same purpose to \code{jumpchoice} argument in \code{\link{forecast}} function.
#'
#' @param element character. Wanted estimation element, one of "mx", "qx", "lx", "dx", "Lx", "Tx", "ex" or "rx".
#'
#' @param ... additional arguments to be passed to \code{\link{read.demogdata}}, such as \code{label}.
#'
#' @return \code{Lifetable} function returns a list with both data frames, wide and long format, for specified element in argument \code{element} for desire years.
#'
#' @examples
#'
#' ## write.table(CR_mortality_rates_2010_2015,
#' ## file = "CR_mortality_rates_2010_2015.txt",
#' ## sep = "\t", row.names = FALSE, quote = FALSE)
#'
#' ## write.table(CR_populations_1950_2015,
#' ## file = "CR_populations_1950_2015.txt",
#' ## sep = "\t", row.names = FALSE, quote = FALSE)
#'
#' ## Lifetable("CR_mortality_rates_2010_2015.txt", "CR_populations_1950_2015.txt",
#' ## sex="female", first_year=2011, threshold=2150, jump="actual", max_age = 100,
#' ## element="ex", label="CR")
#'
#'
#' @author César Gamboa-Sanabria
#'
#' @references
#'
#' \insertRef{Lifetable}{popstudy}
#'
#' @export
Lifetable <- function(rates, pops, sex, max_age=NULL, first_year,
                threshold, jump, element=c("mx", "qx", "lx", "dx", "Lx", "Tx", "ex", "rx"),...){
    data <- read.demogdata(file=rates, popfile=pops, type="mortality",
                           max.mx = 1, skip = 0, popskip = 0,...)
    if(is.null(max_age)){
        max_age <- max(data$age)
    }
    data <- extract.years(data=data, years = min(data$year):(first_year-1)) %>%
        set.upperage(., max.age = max_age)
    threshold <- threshold-max(data$year)

    mod.mort <- lca(data, series=sex, max.age=max_age, interpolate = TRUE)
    proy.morta <- forecast(mod.mort, threshold, jumpchoice=jump)

    projection_table <- lifetable(proy.morta,
                                  series=sex,
                                  type="period",
                                  max.age = max_age)
    projection_table <- eval(parse(text = paste("projection_table$", element, sep=""))) %>%
        as.data.frame()

    estimation_table <- lifetable(data,
                                  series = sex,
                                  type = "period",
                                  max.age = max_age)
    estimation_table <- eval(parse(text = paste("estimation_table$", element, sep=""))) %>%
        as.data.frame()
    full_table <- do.call(cbind, list(estimation_table, projection_table))%>%
        mutate(age=0:max_age,
               sex=sex)

    full_table <- full_table[, c(ncol(full_table)-1, ncol(full_table), 1:(ncol(full_table)-2))] %>%
        mutate(sex=ifelse(sex=="male", "Males", "Females"),
               age=as.double(age))

    short_table <- gather(full_table, year, estimation, -c(age, sex)) %>%
        mutate(year=as.numeric(year))

    list(wide_table = full_table, long_table = short_table)

}


