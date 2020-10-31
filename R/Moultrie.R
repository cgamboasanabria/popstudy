#' Moultrie rule for Children Ever Born
#'
#' Moultrie's proposal for correction of Children Ever Born in five-year grouped ages.
#'
#' @param data data.drame. It contains at least three variables: five-year grouped ages, number of childs and Children Ever Born (CEB).
#'
#' @param ... Arguments to be passed to \code{dplyr::select}, i.e., five-year grouped ages, number of childs and Children Ever Born.
#'
#' @return \code{Moultrie} returns a data.frame with corrected childs for each number of Children Ever Born and five-year grouped ages.
#'
#' @examples
#'
#' CEB_data <- tidyr::gather(CEB, ages, childs, -Children_Ever_Born)
#' results <- Moultrie(CEB_data, ages, childs, Children_Ever_Born)
#' tidyr::pivot_wider(results, names_from=age, values_from=childs)
#'
#'
#' @author Cesar Gamboa-Sanabria
#'
#' @seealso
#'
#' \code{\link{CEB}}
#' \code{\link{El_Badry}}
#'
#' @references
#'
#' \insertRef{moultrie}{popstudy}
#'
#' @export
Moultrie <- function(data,...){
    vars <- select(data,...) %>%
        as.list
    data <- data.frame(age=vars[[1]],
                       childs=vars[[3]],
                       CPW=vars[[2]]) %>%
        mutate(childs=ifelse(grepl("[A-Za-z]", childs) | childs=="99", "Unknown", childs),
               CPW=ifelse(is.na(CPW), 0, CPW))

    data <- data.frame(age=c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"),
                       Max=as.character(c(5,8,12,15,18,22, 25))) %>%
        right_join(., data) %>%
        mutate(Max=ifelse(childs !="Unknown" & as.numeric(Max) < as.numeric(childs), 1, 0))

    data %>%
        filter(Max>0) %>%
        group_by(age) %>%
        summarise(Ignored=sum(CPW)) %>%
        right_join(., data) %>%
        mutate(Ignored=ifelse(is.na(Ignored), 0, Ignored),
               CPW=ifelse(childs=="Unknown", CPW+Ignored, CPW),
               CPW=ifelse(Max>0, 0, CPW)) %>%
        select(age, childs, CPW) %>%
        rename(CEB=childs,
               childs=CPW)
}
