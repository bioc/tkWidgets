\name{fileWizard}
\alias{fileWizard}

\title{A function that import a text file into R}
\description{
  Given a file name, this function imports the text file into R.
}
\usage{
fileWizard(filename = "", fun = read.table, file = "file",
baseic = c("header", "sep"))
}

\arguments{
  \item{filename}{A character string for the name of the text file to be
    imported}
  \item{fun}{An R function that is going to be used to read the
    file. Default to    \code{\link{read.table}}}
  \item{file}{A character string for the name of the argument to fun
    that defines the name of the file to be read}
  \item{basic}{A vector of character strings for names of the arguments
    to fun that will have separate entry boxes on the widget to be
    produced. Default to "header" and "sep"}
}
\details{
  This function is only partially finished and will be improved soon. It
  currently allows uesrs to view a given file and arguments that are
  going to be used to process the file.
}
\value{
  This function does not return any value
}
\references{R News Vol. 1/3, September 2001}
\author{Jianhua Zhang}
\note{This function is part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through R}

\seealso{\code{\link{fileBrowser}}}

\examples{
if(interactive()) {
  # Only the interface is displyed as no real file is given
  fileWizard()  
}
}
\keyword{interface}
