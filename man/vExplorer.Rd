\name{vExplorer}
\alias{vExplorer}
\alias{viewVignette}

\title{An interface to interact with vignette code chunks}
\description{
  This function provides a widget for viewing, editing, and executing
  code chunks of vignettes.
}
\usage{
vExplorer(title = "BioC Vignettes Browser", packNames = "")
viewVignette(title, packName, vigPath, pdfPath)
}

\arguments{
  \item{title}{character string for the name to be
    displayed as the title of the widget to interact with code chunks.}
  \item{packNames,packName}{vector (of length 1 for \code{packName}) of
    character strings for names of Bioconductor packages the code chunks
    of whose vignettes will be explored.}
  \item{vigPath}{character string for the full qualified name of a
    vignette to be explored.}
  \item{pdfPath}{character string for the full path of a pdf file (if
    any) associated with a vignette.}
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

