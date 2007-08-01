\name{fileBrowser}
\alias{fileBrowser}
\title{Simple Interface to View and Pick Files}
\description{
  This function provides the widget for users to go up and down a path and
  view the files.  When files are selected and the \code{"End"} button
  is pressed, the function returns a vector of character strings with
  the full paths of the selected files.
}
\usage{
fileBrowser(path="", testFun = function(x) TRUE, prefix = NULL,
	    suffix = NULL, textToShow = "Select file(s)",
	    nSelect = -1)
}
\arguments{
  \item{path}{character string for the full path to be view. Defaults
    to the current working directory if no path is provided.}
  \item{testFun}{function that checks to see if a given file name
    satisfies predefined requirements. The default is no checking.}
  \item{prefix}{character string for a prefix used to screen out file
    names that do not have that prefix as part of their names.}
  \item{suffix}{character string for a suffix used to screen out file
    names that do not have that suffix as part of their names.}
  \item{textToShow}{character string to be shown on the widget to given
    instructions to users.}
  \item{nSelect}{integer indicating the number of files that should be
    selected.  No limitation if \code{nSelect = -1} as per default.}
}
\details{
  When a path is viewed, files will be displayed as they are and
  directories will be displayed with a system file separator appended
  (e.g. \code{"/"} for Unix systems).
%
  Single click on a file name will make the file selectable when the
  select button is pressed.  Multiple selection is possible by dragging
  with mouse button 1 pressed.
%
  Double click on a directory name will go into that directory and
  display its files.  When a file/directory is selected and the "End" button
  pressed, the full path for the selected files selected will be
  returned.

  The widget is modal and has to be closed by pressing the "End" button
  before doing any other operations. Functions, prefix, and suffix can
  be specified.	 Examples of validity functions are
  \code{\link{hasPrefix}} and \code{\link{hasSuffix}}.

  The following is a list of the buttons and their associated behavior:
  \describe{
    \item{\code{Up}}{Moves the directory whose content is to be
      displayed in the box for file names one level up along the
      directory tree.  No action if already on top of the tree.}

    \item{\code{Select \>\> }}{When a file or files in the box for file
      names in a directory have been highlighted by clicking or dragging
      mouse button 1 and this button is pushed, the highlighted file(s)
      will be displayed in the box for selected file(s) on the right.}

    \item{\code{\<\< Remove}}{When a file or files in the box for
      selected files have been highlighted by clicking or dragging this
      button is pushed, the highlighted file(s) will be removed from the box.}

    \item{\code{Clear}}{Clears everything in the box for selected files
      when pushed.}

    \item{\code{end}}{Returns a vector containing all the names in the box for
      selected files or \code{NULL} if the box is empty.  The full path
      will be appended to the file names.}
  }
}
\value{
  A vector of character strings containing the full path of each file
  selected.
}
\author{Jianhua (John) Zhang}

\seealso{\code{\link{pickFiles}},
  \code{\link{hasPrefix}}, \code{\link{hasSuffix}}} 

\examples{
## The example here is only run interactively since it requires user
## interference which may cause problems if not available:
if(interactive()) {

  # Call the function to view the current directory
  flist <- fileBrowser()
  flist

  # To call the function with a path do
  # fileBrowser(path = "yourPath")
}
}
\keyword{interface}


