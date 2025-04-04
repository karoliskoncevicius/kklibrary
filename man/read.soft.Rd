% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.soft.r
\name{read.soft}
\alias{read.soft}
\title{Read .soft Files}
\usage{
read.soft(file, trim = TRUE)
}
\arguments{
\item{file}{the name of the file which the data are to be read from.}

\item{trim}{a logical value indicating whether the fields (columns) that
       have identical values across all the rows of the data should be
       removed from the final data frame.

       This does not affect fields of sample characteristics, which are
       always returned, even if all the values are identical.}
}
\value{
A data frame containing sample information from the ".soft" file.
}
\description{
Reads a file in the ".soft" format and creates a data frame.
The file should be obtained from the GEO repository by selecting:
scope = samples, format = soft, amount = brief.
}
\author{
Karolis Koncevičius
}
