\name{importWizard}
\alias{importWizard}
\alias{initImportWizard}
\alias{getTopCan}
\alias{getAFrame}
\alias{finish}
\alias{getState1Frame}
\alias{setState1BFrame}
\alias{setState1TFrame}
\alias{showData4State1}
\alias{setState1MFrame}
\alias{getState2Frame}
\alias{setState2MFrame}
\alias{setSepRadios}
\alias{setQuoteList}
\alias{setQuote}
\alias{setState2BFrame}
\alias{showData4State2}
\alias{getState3Frame}
\alias{setState3TFrame}
\alias{setState3BFrame}
\alias{getName4Data}
\alias{writeCol4Matrix}
\alias{popStartLine}

\title{A widget for importing data to R}
\description{
  Functions constructs a widget that allows users to inport data
  file to R. The imported data will be returned as an R data frame
  together with the argument list used to import the data using read.table
}
\usage{
importWizard(filename = "", maxRow = 400)
initImportWizard(env)
getTopCan(base, env)
getAFrame(base, env)
finish(env)
getState1Frame(base, env)
setState1BFrame(frame, env)
setState1TFrame(frame, viewer, delims, env, startList)
showData4State1(widget, env)
setState1MFrame(frame, env, dataViewer)
getState2Frame(base, env, state = "state2", reset = FALSE)
setState2MFrame(frame,env)
setSepRadios(frame, env, state = "state2")
setQuoteList(frame, env)
setQuote(listBox, env, state = "state2")
setState2BFrame(frame, env)
showData4State2(canvas, env, state = "state2")
getState3Frame(base, env)
setState3TFrame(frame, env)
setState3BFrame(frame, env)
getName4Data(filename, objType)
writeCol4Matrix(tempFrame, dataFile, colInfos, env)
popStartLine(startList, env)
}

\arguments{
  \item{filename}{\code{filename} a character string for the name of the
    file to be imported. The default is an empty string and users have to
    click a browse button to get the file name through
    \code{\link{fileBrowser}}}  
  \item{maxRow}{\code{maxRow} an integer for the maximum number of rows
    of the data file to be imported to be shown on the widget. The
    default is 200 rows}
  \item{env}{\code{env} an R environment object for storing the required
    information}
  \item{base}{\code{base} a tcltk window to hold a canvas on the top and
    frames in the bottom}
  \item{frame}{\code{frame} a tcktl frame}
  \item{viewer}{\code{viewer} a tkwin object for a widget}
  \item{delims}{\code{delims} a character string for a file separater}
  \item{widget}{\code{widget} a tcltk widget}
  \item{state}{\code{state} a character string for the state of
    importing process}
  \item{listBox}{\code{listBox} a tcltk list box}
  \item{canvas}{\code{canvas} a tcltk canvas}
  \item{tempFrame}{\code{tempFrame} a tcltk frame that will be used to
    hold widget elements}
  \item{dataFile}{\code{dataFile} a data matrix holding data to be
    displayed}
  \item{colInfos}{\code{colInfos} an object of class colInfo with a
    name, type, and drop slot}
  \item{reset}{\code{reset} a boolean that is TRUE when the window needs
    to be reset}
  \item{dataViewer}{\code{dataViewer} a tkwin object for a list box}
  \item{objType}{\code{objType} a character string indicating the data
    type of an object to be saved. Defaulted to "object"}
  \item{startList}{\code{startList} a tk text box object}
}
\details{
  \code{\link{importWizard}} mimics the interface of MS Excel and
  collects arguments for the function \code{\link{read.table}}. Due to
  performace concern, a maximum number of rows (maxRow) set by 
  users will be displayed. Overly long data set may cause slow response
  of the system.

  \code{\link{initImportWizard}} initializes the interface for
  importWizard by creating a widget with an empty top canvas and bottom
  frame filled with four buttons.

  \code{\link{getTopCan}} Creates a canvas that is going to be filled by
  a frame created by other functions depending on the state of the
  importing process.

  \code{\link{getAFrame}} Gets a frame for the canvas created by
  \code{\link{initImportWizard}} based on the current state of importing
  process.

  \code{\link{finish}} Finishes the importing process and returns a data
  frame read from a file using \code{\link{read.table}}.

  \code{\link{getState1Frame}} Returns a tcltk frame containing a list
  box to show a data file read by \code{\link{readLines}} and widgets
  for user imports.

  \code{\link{setState1BFrame}} Fills the bottom frame of the frame
  created by \code{\link{getState1Frame}} with a list box.

  \code{\link{setState1TFrame}} Fills the top frame of the frame
  created by \code{\link{getState1Frame}} with a list box.

  \code{\link{showData4State1}} Populates a tcltk list or text widget
  with data read using readLines.

  \code{\link{setState1MFrame}} Fills the mid frame of the frame
  created by \code{\link{getState1Frame}}.

  \code{\link{getState2Frame}} Returns a tcltk frame containing a canvas
  to show a data file read by \code{\link{read.table}} and widgets
  for user imports.

  \code{\link{setState2MFrame}} Fills the mid frame of the frame
  created by \code{\link{getState2Frame}}.

  \code{\link{setSepRadios}} Renders radio buttons for options of file
  separators in the frame created by \code{\link{setState2MFrame}}.

  \code{\link{setQuoteList}} Renders the selection list for the quote
  used by a data file in the frame created by
  \code{\link{setState2MFrame}}.

  \code{\link{setQuote}} Sets the value when a user has selected the
  quote used by a data file.

  \code{\link{setState2BFrame}} Fills the bottom frame of the frame
  created by \code{\link{getState2Frame}} with a canvas.

  \code{\link{showData4State2}} Populates the canvas created by
  \code{\link{setState2BFrame}} using data read by
  \code{\link{read.table}}.
  
  \code{\link{getState3Frame}} Returns a tcltk frame containing a canvas
  to show a data file read by \code{\link{read.table}} and widgets
  for user imports.

  \code{\link{setState3TFrame}} Fills the top frame of the frame
  created by \code{\link{getState3Frame}}.

  \code{\link{setState3BFrame}} Fills the bottom frame of the frame
  created by \code{\link{getState3Frame}}.

  \code{\link{getName4Data}} Takes user input for a file name using a
  widget.

  \code{\link{writeCol4Matrix}} Creates a tcltk frame with list boxes as
  columns displaying data of a data matrix.
}
\value{

  \code{\link{getTopCan}} returns a tcltk canvas.

  \code{\link{getAFrame}} returns a tcltk frame.

  \code{\link{finish}} returns a data.frame.

  \code{\link{getState1Frame}} returns a tcltk frame with several
  widgets.

  \code{\link{setState1BFrame}} returns the tkwin object of list box.

  \code{\link{getState2Frame}} returns a tcltk frame with several
  widgets.

  \code{\link{getState3Frame}} returns a tcltk frame with several
  widgets.
  
  \code{\link{getName4Data}} returns a character string for the name of
  a file to be saved.
}

\author{Jianhua Zhang}
\note{This function is part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through R}

\seealso{\code{\link{fileBrowser}}, \code{\link{argsWidget}}}

\examples{
  if(interactive()){
      importWizard()
  }
}
\keyword{interface}

