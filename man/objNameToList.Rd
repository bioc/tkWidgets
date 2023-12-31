\name{objNameToList}
\alias{objNameToList}

\title{Convert Object Names to List of Lists with (name, object) Pairs}
\description{
  This function supports \code{\link{objectBrowser}} by converting a
  vector of selected object names to a list of lists with object names
  and the corresponding objects. 
}
\usage{
objNameToList(objNames, env)
}
\arguments{
  \item{objNames}{character vector giving the names of objects.}
  \item{env}{an R environment where R objects are stored}
}
\details{
  Each list in the list that is going to be returned contains a name for
  the object and the real value of the object. If the object name is a
  package name, the contents of the package will be the value associated
  with the package name.
}
\value{
  A list of lists each with a \code{name} and an \code{obj} component.
}

\author{Jianhua (John) Zhang}

\seealso{\code{\link{objectBrowser}}}

\examples{
# Create two R objects
obj1 <- c("aaa", "bbb", "ccc")
env1 <- new.env(parent = baseenv())

# Get a list containing the two objects
nl <- objNameToList(c("obj1", "env1"), parent.frame())
str(nl)

}
\keyword{manip}

