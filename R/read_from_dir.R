#' read_from_dir
#'
#' Get full path from a file.
#'
#' @param file The file name.
#'
#' @param path The file location.
#'
#' @return \code{read_from_dir} returns an object of class character with the normalizaed path for a file.
#'
#' @examples
#' \donttest{
#' \dontrun{
#' file.create("test_file.txt")
#' read_from_dir("test_file.txt")
#' }
#' }
#'
#' @author Cesar Gamboa-Sanabria
#'
#' @export
read_from_dir <- function(file, path=NULL){
    suppressWarnings({
        if(is.null(path)){
            normalizePath(paste(here::here(), file, sep = "/"))
        }else({
            if(!grepl(":", path)){
                normalizePath(paste(here::here(), path, file, sep = "/"))
            }else({
                normalizePath(paste(path, file, sep = "/"))
            })

        })
    })
}
