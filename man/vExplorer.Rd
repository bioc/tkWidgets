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
  \item{title}{\code{title} a character string for the name to be
    displied as the title of the widget to interact with code chunks}
  \item{packNames}{\code{packNames} a vector of character strings for
    names of Bioconductor packages the code chunks of whose vignettes
    will be explored} 
  \item{packName}{\code{packName} a character string for the name of a
    Bioconductor package the code chunks of whose vignettes will be
    explored}
  \item{vigPath}{\code{vigPath} a character string for the full
    qualified name of a vignette to be explored}
  \item{pdfPath}{\code{pdfPath} a character string for the full path of
    a pdf file (if any) associated with a vignette}
}
\details{
  If the default packNames is used, all the installed packages will be
  examined and those that have vignettes will be listed to allow users
  to choose from.  
}
\value{
  This function does not return any value
}
\references{\url{www.bioconductor.org}}
\author{Jianhua Zhang}
\note{This function is part of the Bioconductor project at Dana-Faber
  Cancer Institute to provide Bioinformatics functionalities through R}

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

