\name{fileBrowser}
\alias{fileBrowser}

\title{An simple interface to view files and pick one as the return.}
\description{
  This function provides the GUI for users to go up and down a path and
  view the files. When a file/directory shown is selected and the "End"
  button pressed, the function will return the full path for that
  file/directory.  
}
\usage{
fileBrowser(path="")
}

\arguments{
  \item{path}{A character string for the full path to be view. Defaults
    to the current working directory if no path is provided.}
}
\details{
  When a path is viewed, files will be displayed as they are and
  directories will be displayed with a system file separacter appended
  (e.g. "/" for the Unix system). Single click on a file/directory name
  will make a selection and double clicks on a directory name will go
  into that directory and the files displayed. When a file/directory is
  selected and the "End" button pressed, the full path for the selected
  file/directory will be returned. The GUI is modal and has to be closed
  by pressing the "End" button before doing any other operations.

  This function is at its preliminary stage of development and will be
  enhanced later.
}
\value{
  \item{fileSelected}{A character string for the full path for the file
    selected.}
}
\references{tcltk, R News Vol. 1/3, 2001. A Primer on the R-Tcl/Tk Package.}
\author{Jianhua (John) Zhang}
\note{This function is part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics funcitons through R.}

\seealso{\code{\link{makeView}},\code{\link{writeDir}}}

\examples{
# The example here is commented out as it requires user interference
# that may cause problem is not available. Auto build for example. 
# Call the function to view the current directory
# fileBrowser()

# To call the function with a path do
# fileBrowser(path = "yourPath")
}
\keyword{interface}




