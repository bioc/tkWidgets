\name{pickObjs}
\alias{pickObjs}

\title{A function that determines what to be sent the GUI for display.}
\description{
  This function takes a vector of object names and determines what will
  be sent to the objectBrowser GUI for display based on the default and
  user input requirements.
}
\usage{
pickObjs(objNames, fun = function(x) TRUE)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{objNames}{A vector of character strings for object names to be
    processed.} 
  \item{fun}{A function that checks the object names for satisfaction of
  certain requirement.} 
}
\details{
  Packages and environments are always displayed.
}
\value{
  \item{objNames}{A vector of character string for object names that
    satisfy the requirements.}
}

\author{Jianhua (John) Zhang}
\note{This function is part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through R.}

\seealso{\code{\link{objectBrowser}}}

\examples{
# Returns names of package and environment objects in the search path.
pickObjs(search())
}
\keyword{manip}

