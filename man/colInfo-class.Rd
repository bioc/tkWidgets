\name{colInfo-class}
\docType{class}
\alias{colInfo-class}
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
No methods defined with class "colInfo" in the signature.
}
\author{Jianhua Zhang}
\note{This class is part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through R}

\seealso{
  \code{\link{importWizard}}
}
\examples{
  newInfo <- new("colInfo", colName = "aaaa", colType = "character",
dropOrNot = FALSE)  
}
\keyword{classes}

