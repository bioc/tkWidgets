\name{objectBrowser}
\alias{objectBrowser}

\title{A function to view the objects in the work space.}
\description{
  This function provides a GUI to view objects and select one that is
  going to be returned. When the "End" button is pressed,the selected
  object will be returned. 
}
\usage{
objectBrowser(fun = function(x) TRUE)
}

\arguments{
  \item{fun}{A function to test whether certain conditions are met by
    the objects. Only objects that meet the conditions will be displyed.}
}

\details{
  If the selected object is a package, a description of the contents of
  the package will be returned. If the selected object is a function, a
  text string of the original code will be returned. Otherwise, the
  object will be returned.

  See function isCharacter for an example of writting a function for
  objectBrowser. 

  The function is at its preliminary stage of development and will be
  enhanced later.
}
\value{
  \item{obj}{The selected object.}
}
\references{Tcltk, R News Vol 1/3. A Primer on the R-Tck/Tk Package.}
\author{Jianhua (John) Zhang}
\note{This function is part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through R.}

\seealso{\code{\link{writeObj}}, \code{\link{isCharacter}}}

\examples{
# The example is commented out as it requires user interference which
# may not be desirable for auto build.

# Call the function with the isCharacter function.
#objectBrowser(isCharacter)
  
}
\keyword{interface}

