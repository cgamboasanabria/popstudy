#' required_packages
#'
#' Install/load the required packages from CRAN.
#'
#' @param ... packages names.
#'
#' @return \code{required_packages} does not return a value, it only install and load the desired packages.
#'
#' @examples
#'\donttest{
#'\dontrun{
#' #If you need to install and load the tidyr, dplyr and ggplot2 packages, run the following line:
#' #required_packages(tidyr, dplyr, ggplot2)
#'}
#'}
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
