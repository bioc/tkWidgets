\name{stdType}
\alias{stdType}
\alias{stdView}
\title{Functions that provide default behaviors for typeFun and valueFun
functions of listSelect function} 
\description{
  The function listSelect takes two functions that defined how the type
  information and content of R objects will be shown on the widget
  created by listSelect. Functions stdType and stdView provide the
  default behavior.
}
\usage{
stdType(toCheck)
stdView(toView)
}

\arguments{
  \item{toCheck}{Any R object}
  \item{toView}{any R object}
}
\details{
  These functions can be viewed as exmaples of defining functions for
  typeFun and valueFun pamaraters of listSelect function.
}
\value{
  \item{stdType}{Returns a character string describing the type of the R
    object}
}
\author{Jianhua Zhang}
\note{These functions are part of the Bioconductor project at
  Dana-Farber Cancer Institute to provide Bioinformatics functionalities
  through R}

\seealso{\code{\link{listSelect}}}

\examples{
stdType(123)
stdType("What am I")
stdType(as.data.frame(matrix(1:10, ncol = 2)))
stdType(stdType)
}
\keyword{misc}

