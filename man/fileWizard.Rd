\name{fileWizard}
\alias{fileWizard}

\title{A function that import a text file into R}
\description{
  Given a file name, this function imports the text file into R in the
  way similar to what MS Excel does.
}
\usage{
fileWizard(filename = "", fun = read.table)
}

\arguments{
  \item{filename}{A character string for the name of the text file to be
    imported}
  \item{fun}{An R function that is going to be used to read the file. Default to    \code{\link{read.table}}}
}
\details{
  This function is only partially finished and will be improved soon.
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
