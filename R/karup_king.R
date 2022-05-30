#' karup_king
#'
#' Separate grouped-age data to simple ages data using Karup-King separation factors.
#'
#' @param data data.frame. A dataset with two variables: \code{age}, the group age each 5 years; and \code{pop}, the population for that age.
#'
#' @return \code{karup_king} function returns a a data frame with separated simple ages.
#'
#' @examples
#'
#' karup_king(grouped_age_CR_pop)
#'
#' @author Cesar Gamboa-Sanabria
#'
#' @references
#'
#' \insertRef{karup_king}{popstudy}
#'
#' @seealso \code{\link{grouped_age_CR_pop}}
#'
#' @export
karup_king <- function(data){
    data1 <- data %>%
        mutate(age=gsub(" - ", ":", age))

    age <- lapply(data1$age[1:15], function(x){
        eval(parse(text = x))
    }) %>% unlist


    pob <- lapply(data1$pop[1:15], function(x){
        rep(x,5)
    }) %>% unlist

    data2 <- data.frame(age=age, pop=pob)
    data2[76,] <- data1[16,]

    factors <- karup_king_factors

    factors[1:10,5] <- data2$pop[1]
    factors[1:10,6] <- data2$pop[6]
    factors[1:10,7] <- data2$pop[11]

    factors[11:76,5] <- c(data2$pop[6:65], data2$pop[61:65], data2$pop[76])
    factors[11:76,6] <- c(data2$pop[11:70], data2$pop[66:70], data2$pop[76])
    factors[11:76,7] <- c(data2$pop[16:75], data2$pop[71:75], data2$pop[76])

    factors %>%
        mutate(pop=ifelse(age=="75 and more", d1,round(f1*d1+f2*d2+f3*d3, 0))) %>%
        dplyr::select(age, pop)
}
