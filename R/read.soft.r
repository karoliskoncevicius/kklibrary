#' Read .soft Files
#'
#' Reads a file in the ".soft" format and creates a data frame.
#' The file should be obtained from the GEO repository by selecting:
#' scope = samples, format = soft, amount = brief.
#'
#' @param file the name of the file which the data are to be read from.
#' @param trim a logical value indicating whether the fields (columns) that
#'        have identical values across all the rows of the data should be
#'        removed from the final data frame.
#'
#'        This does not affect fields of sample characteristics, which are
#'        always returned, even if all the values are identical.
#'
#' @return A data frame containing sample information from the ".soft" file.
#'
#' @author Karolis Koncevičius
#' @export
read.soft <- function(file, trim = TRUE) {
  # read data as two separate columns
  key <- readLines(file)
  key <- read.delim(textConnection(sub(" = ", "\t", key)), header = FALSE)

  # remove potential white spaces
  key[,1] <- trimws(key[,1])
  key[,2] <- trimws(key[,2])

  # get rid of prefixes
  key[,1] <- sub("^\\^", "", key[,1])
  key[,1] <- sub("^\\!Sample_", "", key[,1])

  # rename characteristics with actual fields
  inds <- key[,1] == "characteristics_ch1"
  key[inds,1] <- paste0("sample_", gsub(" ", "_", sub(": .*", "", key[inds,2])))
  key[inds,2] <- sub(".*: ", "", key[inds,2])

  # turn into vectors
  key <- split(key, cumsum(key[,1] == "SAMPLE"))
  key <- Map(function(x) { setNames(x[,2], make.unique(x[,1])) }, key)

  # make sure all samples have the same fields
  names <- Map(names, key)
  if(length(unique(names)) != 1) {
    warning("Not all samples had the same fields - inserting NA values")
  }
  names <- unique(unlist(names))
  names <- names[c(which(!startsWith(names, "sample_")), which(startsWith(names, "sample_")))]
  key <- Map(`[`, key, list(names))
  key <- Map(setNames, key, list(names))

  # convert into data frame
  key <- do.call(rbind, key)
  key <- type.convert(as.data.frame(key), as.is = TRUE)

  # remove general information that is the same for all samples
  if (trim) {
    key <- key[,startsWith(colnames(key), "sample_") | sapply(key, function(x) length(unique(x))) != 1]
  }

  key
}



