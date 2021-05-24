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
    dir.create("Deliverables")

    file.create("README.md")
    file.create("Data/.gitkeep")
    file.create("Data/Raw/.gitkeep")
    file.create("Data/Processed/.gitkeep")
    file.create("Output/.gitkeep")
    file.create("Output/Results/.gitkeep")
    file.create("Output/Plots/.gitkeep")
    file.create("References/.gitkeep")
    file.create("References/Documents/.gitkeep")
    file.create("Script/.gitkeep")
    file.create("Script/Functions/.gitkeep")
    file.create("Products/.gitkeep")
    file.create("Products/Static_Documents/.gitkeep")
    file.create("Products/Interactive_Documents/.gitkeep")
    file.create("Deliverables/.gitkeep")

    file.create("References/references.bib")
    file.create("Script/01_data_cleansing.Rmd")
    file.create("Script/02_exploratory_data_analysis.Rmd")

    cat("Done!")
}
