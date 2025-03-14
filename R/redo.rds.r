#' Rebuild and Read RDS files
#'
#' Uses "redo" build system to check if the specified .rds file is out of date,
#' rebuilds it if necessary and then reads the file.
#'
#' Technically the function first invokes a "redo-ifchange" system command
#' and then reads the specified .rds file using \code{readRDS()}.
#' For an example of "redo" implementation see \href{http://www.goredo.cypherpunks.su}.
#'
#' @param file name of the file where the R object is saved.
#' @param ... optional arguments passed to readRDS.
#'
#' @return an R object.
#'
#' @author Karolis Koncevičius
#' @export
redo.rds <- function(file, ...) {
  system(paste("redo-ifchange", file))
  readRDS(file, ...)
}
