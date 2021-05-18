#' project_structure
#'
#' Create a basic structure for a project repo.
#'
#' @return \code{project_structure} create basic diretories and files in the current working direcotory/repository.
#'
#' @examples
#'
#' project_structure()
#'
#' @author Cesar Gamboa-Sanabria
#'
#' @export
project_structure <- function(){
    dir.create("Data")
    dir.create("Data/Raw")
    dir.create("Data/Processed")
    dir.create("Output")
    dir.create("Output/Results")
    dir.create("Output/Plots")
    dir.create("References")
    dir.create("References/Documents")
    dir.create("Script")
    dir.create("Script/Functions")
    dir.create("Products")
    dir.create("Products/Static_Documents")
    dir.create("Products/Interactive_Documents")

    file.create("References/references.bib")
    file.create("Script/01_data_cleansing.Rmd")
    file.create("Script/02_exploratory_data_analysis.Rmd")

    cat("Done!")
}
