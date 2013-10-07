\name{pExplorer}
\alias{pExplorer}
\alias{getPkgContents}
\alias{getFileContents}
\alias{getExclude}
\alias{getRPkgs}
\alias{hasDesc}
\alias{procRda}
\alias{procHelp}
\alias{procPDF}
\alias{procHTML}
\title{A widget to explore R packages}
\description{
  This widget allows users to explore R packages in the R library and
  try the example code.
}
\usage{
pExplorer(pkgName = "", pkgPath = "", exclude = getExclude(), getFocus =
TRUE)
getPkgContents(pkgName, exclude = getExclude())
getFileContents(path, fileName)
getExclude()
getRPkgs(pkgPath)
hasDesc(pkgPath)
procRda(fileName)
procHelp(fileName)
procPDF(fileName)
procHTML(fileName)
}
\arguments{
  \item{pkgPath}{\code{pkgPath} a character string for the path where R
    packages are loacted}
  \item{path}{\code{path} a character string for the path of a given file}
  \item{pkgName}{\code{pkgName} a character string for the name
    (including path) of an R package to be explored}
  \item{fileName}{\code{fileName} a character string for the name
    (including path) of a file of interest}
  \item{exclude}{\code{exclude} a vector of character strings containing
    the directory or file names that will not be available for
    explorering. Package names have to have a system file separator
    appanded to the end (e. g. "/" under Unix)}
   \item{getFocus}{\code{getFocus} a boolean indicating whether a widget
    should grab the focus}
}
\details{
  With or without a package name, the widget will have all the installed
  R package names in a dropdown list for user to select. As the default,
  the first element from \code{\link{list.files}} will be selected and
  the contents displayed if no package name is given.

  \code{\link{getPkgContents}} gets the contents of a given R package
  and \code{\link{getFileContents}} gets the contents of a givan file.

  \code{\link{getRPkgs}}, \code{\link{hasDesc}}, \code{\link{procRda}},
  \code{\link{procHelp}}, \code{\link{procPDF}}, and
    \code{\link{procHTML}} are functions called by
    \code{\link{pExplorer}} to process different file or directory types.  
}
\value{
  The widget returns invisiable()
}
\references{Documentation on R packages }
\author{Jianhua Zhang}

\seealso{\code{\link{eExplorer}}}
\examples{
    require("tkWidgets") || stop("tkWidgets not available")
    getPkgContents(.libPaths(), "tkWidgets")
    getFileContents(file.path(path.package("tkWidgets"), "help"),
        list.files(file.path(path.package("tkWidgets"), "help"))[1])
    if(interactive()){
        pExplorer()
    }
}
\keyword{interface}

