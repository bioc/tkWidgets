\name{fileBrowser}
\alias{fileBrowser}

\title{An simple interface to view files and pick one as the return.}
\description{
  This function provides the widget for users to go up and down a path and
  view the files. When files are selected and the "End" button pressed,
  the function will return the a vector of strings with the full path
  for the selected files. 
}
\usage{
fileBrowser(path="", testFun = function(x) TRUE, prefix = NULL, suffix =
NULL,textToShow = "Choose file(s) to be read in.", nSelect = -1)
}

\arguments{
  \item{path}{A character string for the full path to be view. Defaults
    to the current working directory if no path is provided.}
  \item{testFun}{A function that checks to see if a given file name
    satisfies predefined requirements. The default is no checking.}
  \item{prefix}{A character string for a prefix used to screen out file
    names that do not have that prefix as part of their names.}
  \item{suffix}{A character string for a suffix used to screen out file
    names that do not have that suffix as part of their names.}
  \item{textToShow}{A character string to be shown on the widget to given
    instructions to users.}
  \item{nSelect}{An integer for the number of files that should be
    selected. No limitation if nSelect = -1}
}
\details{
  When a path is viewed, files will be displayed as they are and
  directories will be displayed with a system file separator appended
  (e.g. "/" for the Unix system). Single click on a file name will make
  the file selectable when the select button is pressed. Multiple
  selection is possible by dragging with mouse button 1 pressed. Double
  clicks on a directory name will go into that directory and the files
  displayed. When a file/directory is selected and the "End" button
  pressed, the full path for the selected files selected will be
  returned. The widget is modal and has to be closed by pressing the "End"
  button before doing any other operations. Functions, prefix, and
  suffix can be specified. For examples of valid functions, see code in
  hasPrefix.R and hasSuffix.R.

  The following is a list of the buttons and the associated behavior:

  Up - Moves the directory whose content is to be displayed in the box
  for file names one level up along the directory tree. No action if
  already on top of the tree. 

  Select >> - When a file or files in the box for file names in a
  directory have been highlighted by clicking or clicking and draging
  mouse button 1 and the "Select >>" button pushed, the highlighted file(s)
  will be displayed in the box for selected file(s) on the right.

  << Remove - When a file or files in the box for selected files have been
  highlighted by clicking and clicking and draging and the "<< Remove"
  button pushed, the highlighted file(s) will be removed from the box.

  Clear - Clears everything in the box for selected files when pushed.

  end - Returns a vector containing all the names in the box
  for selected files or NULL if the box is empty. The full path will be
  appended to the file names.
}
\value{
  \item{fileSelected}{A vector of character strings for the full path
    for the files selected.}
}
\references{tcltk, R News Vol. 1/3, 2001. A Primer on the R-Tcl/Tk Package.}
\author{Jianhua (John) Zhang}
\note{This function is part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functions through R.}

\seealso{\code{\link{makeView}},\code{\link{writeDir}},
  \code{\link{pickFiles}}, \code{\link{hasPrefix}}, \code{\link{hasSuffix}}}

\examples{
# The example here is commented out as it requires user interference
# that may cause problem is not available. Auto build for example. 
# Call the function to view the current directory
# fileBrowser()

# To call the function with a path do
# fileBrowser(path = "yourPath")
}
\keyword{interface}






