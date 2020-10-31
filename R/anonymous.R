#' anonymous
#'
#' Anonymising a data frame by avoiding vulnerability to a rainbow table attack.
#'
#' @param data data.frame. A dataset with the a variable to change its values.
#' @param ID character. A string with the variable name to change its values.
#' @param string_length numeric. It defines the string length of the new identification variable.
#' @param SEED to be passed to \code{\link{set.seed}} to keep the the same new id's.
#'
#' @return \code{anonymous} function returns a list two data frames: original data with the new variable and another data frame with the original variable and the new one.
#'
#' @examples
#' library(dplyr)
#' df <- select(mutate(mtcars, id=rownames(mtcars)), id, !contains("id"))
#' anonymous(df, ID="id", string_length = 5, SEED=160589)
#'
#' @author Cesar Gamboa-Sanabria
#'
#' @references
#'
#' \insertRef{anonymous}{popstudy}
#'
#' @export
anonymous <- function(data, ID, string_length=15, SEED=NULL){
    cols <- select(data, ID)
    ids <- unname(apply(cols, 1, paste, collapse = ""))
    ids <- as.integer(factor(ids))
    n <- length(unique(ids))
    special_chars <- rawToChar(as.raw(1:255), multiple = TRUE)
    set.seed(SEED)
    anonymized_id <- replicate(n, paste(sample(special_chars, string_length, replace = TRUE), collapse = ""))
    list(data=eval(parse(text = paste("mutate(data,", ID, "=anonymized_id[ids])"))),
         diccionary=unique(do.call(cbind, list(select(data, ID), anonymized_id=as.character(anonymized_id[ids])))))
}
