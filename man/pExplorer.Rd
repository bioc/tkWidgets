\name{pExplorer}
\alias{pExplorer}
\alias{getPkgContents}
\alias{getFileContents}
\title{A widget to explore R packages}
\description{
  This widget allows users to explore R packages in the R library and
  try the example code.
}
\usage{
pExplorer(pkgName = "")
getPkgContents(pkgName)
getFileContents(fileName)
}
\arguments{
  \item{pkgName}{\code{pkgName} a character string for name of an R
    package to be explored}
  \item{fileName}{\code{fileName} a character string for the name
    (including path) of a file of interest} 
}
\details{
  With or without a package name, the widget will have all the installed
  R package names in a dropdown list for user to select. As the default,
  the first element from \code{\link{list.files}} will be selected and
  the contents displayed if no package name is given.

  \code{\link{getPkgContents}} gets the contents of a given R package
  and \code{\link{getFileContents}} gets the contents of a givan file.
}
\value{
  The widget returns invisiable()
}
\references{Documentation on R packages }
\author{Jianhua Zhang}
\note{The functions are part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinfomatics functionalities through R}

\seealso{\code{\link{eExplorer}}}
\examples{
    require("tkWidgets") || stop("tkWidgets not available")
    getPkgContents("tkWidgets")
    getFileContents(list.files(file.path(.path.package("tkWidgets"),
        "help"), full.name = TRUE)[1])
    if(interactive()){
        pExplorer()
    }
}
\keyword{interface}

