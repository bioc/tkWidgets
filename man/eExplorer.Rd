\name{eExplorer}
\alias{eExplorer}
\alias{getExCode}
\alias{getHelpFile}
\title{A widget that allows users to explore the example code and help
  files of a given R package}
\description{
  Given a valid package name as a character string,
  \code{\link{eExplorer}} collects all the example code from the "R-ex"
  directory from the R library for that package and then displays the
  names of the code examples in a list box. When a name in the list box
  is clicked, the corresponding code will be displayed and users are
  allowed to execute the code or view the help file for the function the
  example code is for.
}
\usage{
eExplorer(pkgName, font = "arial 13", getFocus = TRUE)
getExCode(pkgName)
getHelpFile(pkgName, fileName)
}
\arguments{
  \item{pkgName}{\code{pkgName} a character string for the name of an R
    package of interest. The R package needs to be installed}
  \item{font}{\code{font} a character string for the font to be used by
    the widget to display the text. The default is "arial 13"}
  \item{fileName}{\code{fileName} a character string for the name of a
    file in "R-ex" with the ".R" extension removed. The file contains a
    chunk of example code and may have a corrsponding help file in the
    "help" directory}
   \item{getFocus}{\code{getFocus} a boolean indicating whether a widget
    should grab the focus}
}
\details{
  \code{\link{getExCode}} and \code{\link{getHelpFile}} are called by
  \code{\link{eExplorer}} to get the code examples or help help files
  contained by a given package.
}
\value{
  \code{\link{eExplorer}} does not return anything useful.
}
\references{Writing R Extension for information on "R-ex" and "help"
  directories} 
\author{Jianhua Zhang}

\seealso{\code{\link{vExplorer}}}
\examples{
  if(interactive()){
      require("Biobase") || stop("Does not run without Biobase")
      eExplorer("Biobase")
  }
}
\keyword{interface}

