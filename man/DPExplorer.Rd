\name{DPExplorer}
\alias{DPExplorer}
\alias{getTopLevel}
\alias{loadDataPkg}

\title{Functions constuct a widget to explore BioC's data packages}
\description{
  These functions construct a widget that allow users to visually
  explore a data package of Bioconductor and read selected elements to R
}
\usage{
DPExplorer(pkgName = "", title = "BioC Data Package Explorer")
getTopLevel(title)
loadDataPkg(pkgName)
}

\arguments{
  \item{pkgName}{\code{pkgName} a character string for the name of a
    Bioconductor's data package that has already been loaded}
  \item{title}{\code{title} a character string for the title of the widget}
}
\details{
  If \code{pkgName} is not provided when \code{\link{DPExplorer}} is
  called, an entry box is available for users to put a \code{pkgName} in
  later. In either cases, the data package specified by \code{pkgName}
  should have been loaded.

  \code{\link{getTopLevel}} creates a top level window for the widget.

  \code{\link{loadDataPkg}} filters out valid environment objects from a
  data package.
}
\value{
  If only one key is selected, \code{\link{DPExplorer}} returns a vector
  of one to more elements. If more than one key is selected,
  \code{\link{loadDataPkg}} returns a list of vectors.

  \code{\link{getTopLevel}} returns a tkwin object for the top level
  window.

  \code{\link{loadDataPkg}} returns a vector of character strings for
  available data environments.
}
\references{Documents for a Bioconductor data package}
\author{Jianhua Zhang}
\note{The functions are part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through R}

\examples{
if(interactive() && require(hgu95av2)){
    DPExplorer("hgu95av2")
}
}
\keyword{interface}

