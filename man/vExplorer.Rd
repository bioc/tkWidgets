\name{vExplorer}
\alias{vExplorer}
\alias{viewVignette}

\title{An interface to interact with vignette code chunks}
\description{
  This function provides a widget for viewing, editing, and executing
  code chunks of vignettes.
}
\usage{
vExplorer(title = "BioC Vignettes Browser", pkgName = "",  font =
ifelse(.Platform$OS.type  == "unix", "arial 14", "arial 11"))
viewVignette(title, packName, vigPath, pdfPath, font = "arial 11")
}

\arguments{
  \item{title}{character string for the name to be
    displayed as the title of the widget to interact with code chunks.}
  \item{pkgName}{vector (of length 1 for \code{pkgName}) of
    character strings for names of Bioconductor packages the code chunks
    of whose vignettes will be explored.}
  \item{packName}{same as pkgName}
  \item{vigPath}{character string for the full qualified name of a
    vignette to be explored.}
  \item{pdfPath}{character string for the full path of a pdf file (if
    any) associated with a vignette.}
  \item{font}{a character string for the name and size of the font to be
    used for text rendered on the widgets (e. g. "arial 11")}
}
\details{
  By default, \code{packNames = ""}, all the installed packages will be
  examined and those that have vignettes will be listed to allow users
  to choose from.
}
\value{
  This function does not return any value.
}
\references{\url{http://www.bioconductor.org}}
\author{Jianhua Zhang}
\note{This function is part of the Bioconductor project at Dana-Faber
  Cancer Institute to provide Bioinformatics functionalities through \R.
}
\examples{
if(interactive()){
    library(Biobase)
    library(tools)
    vExplorer(packNames = c("AnnBuilder", "annotate", "Biobase"))
    path <- .path.package("Biobase")
    vigList <- getPkgVigList(path)
    viewVignette("BioC VignetteBrowser", "Biobase", vigList[[1]]$VigPath)
}
}
\keyword{interface}

