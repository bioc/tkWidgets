\name{colInfo-class}
\docType{class}
\alias{colInfo-class}
\alias{name<-}
\alias{name}
\alias{type<-}
\alias{type}
\alias{dropOrNot<-}
\alias{dropOrNot}
\alias{colInfo}
\title{Class "colInfo" a class to keep column information for a data file}
\description{Objects of this class store column information for a data
  file. The information include name, type, and drop that will be used
  by \code{\link{importWizard}} to import data}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("colInfo", ...)}.
    A constructor (colInfo) is also available and should be used as much
    as possible. 
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"} a character
      string for the name of a data column}
    \item{\code{type}:}{Object of class \code{"character"} a character
      string for the type of the data column}
    \item{\code{dropOrNot}:}{Object of class \code{"logical"} a boolean
      indicating whether the column will be droped (TRUE) when the data
      file is read by \code{\link{read.table}}}
  }
}

\section{Methods}{
  \describe{
    \item{name<-}{\code{signature(object = "colInfo")}: Sets the value
      for name}
    \item{name}{\code{signature(object = "colInfo")}: Gets the value for
      name}
    \item{type<-}{\code{signature(object = "colInfo")}: Sets the value
      for type}
    \item{type}{\code{signature(object = "colInfo")}: Gets the value for
      type}
    \item{dropOrNot<-}{\code{signature(object = "colInfo")}: Sets the value
      for dropOrNot}
    \item{dropOrNot}{\code{signature(object = "colInfo")}: Gets the value for
      dropOrNot}
  }
}
\references{Programming with data}
\author{Jianhua Zhang}
\note{This class is part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through R}

\seealso{\code{\link{importWizard}}}

\examples{
  test <- colInfo("info1", "character", FALSE)
  name(test)
  type(test)
  dropOrNot(test)
}
\keyword{classes}
