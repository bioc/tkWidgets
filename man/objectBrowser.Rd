\name{objectBrowser}
\alias{objectBrowser}

\title{A function to view the objects in the work space.}
\description{
  This function provides a GUI to view objects and select one that is
  going to be returned. 
}
\usage{
objectBrowser()
}

\details{
  A copy of the selected object will be returned at the end of the session.
}
\value{
  \item{obj}{The selected object.}
}
\references{Tcltk, R News Vol 1/3. A Primer on the R-Tck/Tk Package.}
\author{Jianhua (John) Zhang}
\note{This function is part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through R.}

\seealso{\code{\link{writeObj}}}

\examples{
# The example is commented out as it requires user interference which
# may not be desirable for auto build.

# Call the funciton
#objectBrowser()
  
}
\keyword{interface}
