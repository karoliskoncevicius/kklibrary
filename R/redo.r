#' Rebuild and Read files
#'
#' Uses "redo" build system to check if the specified file is out of date,
#' rebuilds it if necessary and then tries to read the file.
#'
#' Technically the function first invokes a "redo-ifchange" system command
#' and then reads the specified file using the appropriate R function.
#' For an example of "redo" implementation see \href{http://www.goredo.cypherpunks.su}{http://www.goredo.cypherpunks.su}.
#'
#' List of currently supported file types: .rds, .csv.
#'
#' @param file the name of the file which the data are to be read from.
#' @param ... optional arguments passed to the function reading the file.
#'
#' @return an R object.
#'
#' @author Karolis Koncevičius
#' @export
redo <- function(file, ...) {
  system(paste("redo-ifchange", file))
  ext <- tolower(sub(".*\\.", "", file))
  if(ext == "rds") {
    readRDS(file, ...)
  } else if(ext == "csv") {
    read.csv(file, ...)
  } else {
    stop("file type: '", ext, "' is not supported")
  }
}
