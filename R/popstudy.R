#' popstudy Package
#'
#' Applied techniques to demographic analysis.
#'
#' @docType package
#'
#' @author Cesar Gamboa-Sanabria \email{info@cesargamboasanabria.com}
#'
#' @name popstudy
#'
#' @importFrom Rdpack reprompt
#' @importFrom lubridate date_decimal decimal_date interval ymd years year days
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select filter group_by mutate summarise arrange mutate_all row_number left_join right_join rename desc mutate_if ungroup contains
#' @importFrom ggplot2 ggplot theme_classic scale_x_continuous geom_density aes geom_segment scale_y_continuous geom_text theme element_text element_blank margin ggtitle labs dup_axis element_line geom_area geom_density geom_vline
#' @importFrom tidyr gather pivot_wider separate
#' @importFrom stats complete.cases density quantile sd na.omit coef lm xtabs
#' @importFrom demography read.demogdata extract.years set.upperage lifetable lca
#' @importFrom forecast forecast
#' @importFrom moments kurtosis skewness
#' @importFrom ggpubr ggarrange ggtexttable ttheme
#' @importFrom grid unit
#' @importFrom stackoverflow match.call.defaults
#' @importFrom utils combn
#' @importFrom rcompanion wilcoxonRG
#' @importFrom DescTools Lambda
NULL
