#' popstudy Package
#'
#' Applied techniques to demographic and time series analysis.
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
#' @importFrom dplyr select filter distinct group_by mutate summarise arrange mutate_all row_number left_join right_join rename desc mutate_if ungroup contains last mutate_at vars n
#' @importFrom ggplot2 ggplot theme_classic scale_x_continuous geom_density aes geom_segment scale_y_continuous geom_text theme element_text element_blank margin ggtitle labs dup_axis element_line geom_area geom_density geom_vline
#' @importFrom tidyr gather pivot_wider separate spread
#' @importFrom stats complete.cases density quantile sd na.omit coef lm xtabs cor.test qnorm tsp ts var weighted.mean nlm StructTS predict tsp tsp<-
#' @importFrom demography read.demogdata extract.years set.upperage lifetable lca smooth.demogdata netmigration coherentfdm pop.sim fdm
#' @importFrom forecast forecast Arima accuracy InvBoxCox ets arfima auto.arima BoxCox
#' @importFrom moments kurtosis skewness
#' @importFrom ggpubr ggarrange ggtexttable ttheme
#' @importFrom grid unit
#' @importFrom stackoverflow match.call.defaults
#' @importFrom utils combn capture.output installed.packages install.packages
#' @importFrom rcompanion wilcoxonRG
#' @importFrom DescTools Lambda
#' @importFrom corrr network_plot
#' @importFrom Hmisc %nin%
#' @importFrom corrplot corrplot
#' @importFrom correlation correlation
#' @importFrom parallel detectCores makeCluster clusterExport clusterMap stopCluster clusterEvalQ
#' @importFrom here here
#' @importFrom stringr str_to_title
#' @importFrom scales rescale
#' @importFrom rainbow fts
NULL
