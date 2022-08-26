#' Myer's Blended Index
#'
#' An upgrade over the Whipple index allows analyzing digit's attraction (or repulsion) from 0 to 9.
#'
#' @param data data.drame. It contains at least two variables: specific ages and population.
#'
#' @param ... Arguments to be passed to \code{dplyr::select}, i.e., age and population, respectively.
#'
#' @return \code{Myers} returns a list with two objects:
#'
#' \item{Mmat}{a data.frame with specific digits index}
#' \item{MI}{the Myer's Blend Index.}
#'
#' @examples
#'
#' results <- Myers(Panama1990, age, pop)
#' results$Mmat
#' results$MI
#'
#' @author Cesar Gamboa-Sanabria
#'
#' @references
#'
#' \insertRef{Shryock}{popstudy}
#'
#' @export
Myers <- function(data,...){
    vars <- select(data,...) %>%
        as.list

    Mmat <- data.frame(age=vars[[1]],
                       pop=vars[[2]]) %>%
        filter(!grepl("[A-Za-z]", age)) %>%
        mutate(age=as.numeric(age)) %>%
        filter(age %in%10:98) %>%
        mutate(U=substr(age, 2,2),
               D=substr(age, 1,1)) %>%
        filter(D %in% paste(1:8)) %>%
        select(-age) %>%
        group_by(D) %>%
        mutate(row = row_number()) %>%
        pivot_wider(names_from=D, values_from=pop) %>%
        select(-row) %>%
        mutate(aj=as.numeric(U)+1,
               aj.=as.numeric(rev(U)),
               Nj=`1`+`2`+`3`+`4`+`5`+`6`+`7`,
               Nj.=`2`+`3`+`4`+`5`+`6`+`7`+`8`,
               ajNj=aj*Nj,
               aj.Nj.=aj.*Nj.,
               IMj=((ajNj+aj.Nj.)/(sum(ajNj)+sum(aj.Nj.)))-.1)

    index <- sum(abs(Mmat$IMj))*100

    list(Mmat=Mmat, MI=index)
}
