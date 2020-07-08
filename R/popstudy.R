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
#' @importFrom dplyr select filter group_by mutate summarise arrange mutate_all
#' @importFrom ggplot2 ggplot theme_classic scale_x_continuous geom_density aes geom_segment scale_y_continuous geom_text theme element_text element_blank margin ggtitle labs dup_axis element_line geom_area geom_density geom_vline
#' @importFrom tidyr gather
#' @importFrom stats complete.cases density quantile sd
#' @importFrom demography read.demogdata extract.years set.upperage lifetable lca
#' @importFrom forecast forecast
#' @importFrom moments kurtosis skewness
#' @importFrom ggpubr ggarrange ggtexttable ttheme
NULL
