% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/redo.r
\name{redo}
\alias{redo}
\title{Rebuild and Read files}
\usage{
redo(file, ...)
}
\arguments{
\item{file}{the name of the file which the data are to be read from.}

\item{...}{optional arguments passed to the function reading the file.}
}
\value{
an R object.
}
\description{
Uses "redo" build system to check if the specified file is out of date,
rebuilds it if necessary and then tries to read the file.
}
\details{
Technically the function first invokes a "redo-ifchange" system command
and then reads the specified file using the appropriate R function.
For an example of "redo" implementation see \href{http://www.goredo.cypherpunks.su}{http://www.goredo.cypherpunks.su}.

List of currently supported file types: .rds, .csv.
}
\author{
Karolis Koncevičius
}
