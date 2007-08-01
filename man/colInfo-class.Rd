\name{colInfo-class}
\docType{class}
\alias{colInfo-class}
\alias{colInfo}
\alias{colName<-}
\alias{colName}
\alias{colType}
\alias{colType<-}
\alias{dropOrNot}
\alias{dropOrNot<-}
\alias{colName<-,colInfo-method}
\alias{colName,colInfo-method}
\alias{colType,colInfo-method}
\alias{colType<-,colInfo-method}
\alias{dropOrNot,colInfo-method}
\alias{dropOrNot<-,colInfo-method}
\title{Class "colInfo" presents column information for a data frame}
\description{This class is for keeping information about a data frame to
be processed. The class is mainly for use by \code{\link{importWizard}}}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("colInfo", ...)}
}
\section{Slots}{
  \describe{
    \item{\code{colName}:}{Object of class \code{"character"} - a
      character string for the name of the column}
    \item{\code{colType}:}{Object of class \code{"character"} - a
      character string for the data type of the column. Can only be
      "character" or "numeric"}
    \item{\code{dropOrNot}:}{Object of class \code{"logical"} - a
      boolean indicationg whether the column will be droped}
  }
}
\section{Methods}{
  \describe{
    \item{colName}{\code{signature(object = "character")}: The get
      method for slot "colName"}
    \item{colName<-}{\code{signature(object = "character")}: The set
      method for slot "colName"}
    \item{colType}{\code{signature(object = "character")}: The get
      method for slot "colType"}
    \item{colType<-}{\code{signature(object = "character")}: The set
      method for slot "colName"}
    \item{dropOrNot}{\code{signature(object = "logical")}: The get
      method for slot "dropOrNot"}
    \item{dropOrNot}{\code{signature(object = "logical")}: The set
      method for slot "dropOrNot"}
  }
}
\author{Jianhua Zhang}

\seealso{
  \code{\link{importWizard}}
}
\examples{
  newInfo <- new("colInfo", colName = "aaaa", colType = "character",
dropOrNot = FALSE)  
}
\keyword{classes}











