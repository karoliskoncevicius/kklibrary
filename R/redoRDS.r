redoRDS <- function(file, ...) {
  system(paste("redo-ifchange" file))
  readRDS(file, ...)
}
