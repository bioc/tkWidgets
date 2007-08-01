\name{pickObjs}
\alias{pickObjs}
\alias{noAuto}

\title{Determine What to Be Sent to a Widget}
\description{
  This function takes a vector of object names and determines what will
  be sent to (e.g. the \code{\link{objectBrowser}}) widget for display
  based on the default and user input requirements.
}
\usage{
pickObjs(objNames, fun = noAuto)
noAuto(x)
}

\arguments{
  \item{objNames}{\code{objNames} character vector with object names to
    be processed} 
  \item{fun}{\code{fun} function checking the object names for satisfaction of
    certain requirement} 
  \item{x}{\code{x} a character string for the name of an object}
}
\details{
  Packages and environments are always displayed.
}
\value{
  Character vector of object names that satisfy the requirements.}
}

\author{Jianhua (John) Zhang}

\seealso{\code{\link{objectBrowser}}}

\examples{
# Returns names of package and environment objects in the search path.
pickObjs(search())
}
\keyword{manip}

