#' popstudy Package
#'
#' Applied techniques to demographic analysis.
#'
#' @docType package
#'
#' @author César Gamboa-Sanabria \email{info@cesargamboasanabria.com}
#'
#' @name popstudy
#'
#' @importFrom Rdpack reprompt
#' @importFrom lubridate date_decimal decimal_date interval ymd years year days
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select filter group_by mutate summarise arrange
#' @importFrom ggplot2 ggplot aes geom_segment scale_y_continuous geom_text theme element_text element_blank margin ggtitle labs
#' @importFrom tidyr gather
#' @importFrom stats complete.cases
#' @importFrom demography read.demogdata extract.years set.upperage lifetable lca
#' @importFrom forecast forecast
NULL
