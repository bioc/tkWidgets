\name{fileWizard}
\alias{fileWizard}

\title{A function that import a text file into R}
\description{
  Given a file name, this function imports the text file into R.
}
\usage{
fileWizard(filename = "", fun = read.table, file = "file",
basic = c("header", "sep"), getFocus = TRUE)
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
  \item{getFocus}{\code{getFocus} a boolean indicating whether a widget
    should grab the focus}
}
\details{
  This function is only partially finished and will be improved soon. It
  currently allows uesrs to view a given file and change the settings
  for header and sep arguments of read.table. A file will be read in
  based on the values of the two arguments and return.
}
\value{
  This function returns a data frame for the file read in.
}
\references{R News Vol. 1/3, September 2001}
\author{Jianhua Zhang}

\seealso{\code{\link{fileBrowser}}}

\examples{
if(interactive()) {
  # Only the interface is displyed as no real file is given
  fileWizard()  
}
}
\keyword{interface}
