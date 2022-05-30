#' required_packages
#'
#' Install/load the required packages from CRAN.
#'
#' @param ... packages names.
#'
#' @examples
#'
#' required_packages(tidyr, dplyr, ggplot2)
#'
#' @author Cesar Gamboa-Sanabria
#'
#' @export
required_packages <- function(...){
    paq <- as.list(match.call())[-1]
    paq <- sapply(paq, paste)
    paq.nuevo <- paq[!(paq %in% installed.packages()[, "Package"])]
    if(length(paq.nuevo))
        install.packages(paq.nuevo, dependencies = TRUE, quiet = TRUE)
    suppressPackageStartupMessages(sapply(paq, require, character.only = TRUE)) #
}
