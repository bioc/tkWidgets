\name{setArgsList}
\alias{setArgsList}
\alias{whatDeli}
\alias{getMoreArgs}
\alias{assignArgs}
\alias{getArgs}
\alias{assignShowNum}
\alias{getShowNum}
\alias{assignCState}
\alias{getCState}
\alias{assignColInfo}
\alias{getColInfo}
\alias{setColInfos}
\alias{changeState}
\alias{setNewState}
\alias{addArgs}
\alias{dropArgs}
\alias{setSkip}
\alias{moreArgs}
\alias{dropColumn}
\alias{setColName}
\alias{setColType}
\alias{assignLineData}
\alias{getLineData}
\title{Functions to support importWizard}
\description{
  The functions are to support importWizard and may not have much
  practical use otherwise.
}
\usage{
setArgsList(filename, env, isFile = TRUE, init = TRUE)
whatDeli(delimiter)
getMoreArgs()
assignArgs(value, env)
getArgs(env)
assignShowNum(value, env)
getShowNum(env)
assignCState(value, env)
getCState(env)
assignColInfo(value, env)
getColInfo(env)
setColInfos(types, env)
changeState(canvas, backBut, nextBut, env, forward = TRUE, endBut, viewBut)
setNewState(env, backBut, nextBut, forward = TRUE, endBut, viewBut)
addArgs(env)
dropArgs(env)
setSkip(widget, env, state = "state1")
moreArgs(env)
dropColumn(index, env)
setColName(index, entryBox, env)
setColType(index, entryBox, env)
assignLineData(lineData, env)
getLineData(env)
}

\arguments{
  \item{filename}{\code{filename} a character string for the full name
    of a file}
  \item{env}{\code{env} an R environment object for storing information}
  \item{delimiter}{\code{delimiter} a character string for the delimiter
    whose letter representation is sought}
  \item{value}{\code{value} a character or numerical value to be
    assigned to a variable}
  \item{backBut}{\code{backBut} a tkwin object for the button that
    shifts back to the previous state}
  \item{nextBut}{\code{nextBut} a tkwin object for the button that
    shifts to the next state}
  \item{forward}{\code{forward} a boolean indicating the direction of
    state change}
  \item{widget}{\code{widget} a tcltl widget}
  \item{state}{\code{state} a character string for the state of
    importing process}
  \item{index}{\code{index} an integer for the index of the list for
    column information}
  \item{entryBox}{\code{entryBox}} a tcltk entry box.
  \item{canvas}{\code{canvas} a tcltk canvas}
  \item{types}{\code{types} a vecter of string indicating the types of
    data columns}
  \item{lineData}{\code{lineData} a vector of character strings read in
    using \code{\link{readLines}}}
  \item{endBut}{\code{endBut} a tkwin object for the button that ends
    the process when pressed}
  \item{viewBut}{\code{viewBut} a tkwin object for the button that
    refresh the window when pressed}
  \item{init}{\code{init} a boolean that is TRUE when the widget is
    first set up and FALSE otherwise}
  \item{isFile}{\code{isFile} a boolean that is TRUE if \code{fileName}
    is a file}
}
\details{
  \code{\link{setArgsList}} calls function \code{\link{guess.sep}} to
  figure out the the header, sep, and data type of a file and sets the
  values for argument list and colInfo.

  \code{\link{whatDeli}} gets the word representation of delimiters
  (e.g. tab for "\t").

  \code{\link{getMoreArgs}} generates a widget using widgetTools to
  collect some of the arguments for read.table.

  \code{\link{assignArgs}} updates "argsList" stored in a predefined
  environment. 

  \code{\link{getArgs}} Gets "argsList" from a predefined environment.
  
  \code{\link{assignShowNum}} Updates the value for "showNum" (number of
  rows to show in the interface.
  
  \code{\link{getShowNum}} Gets the value for "showNum" (number of
  rows to show in the interface. 
  
  \code{link{assignCState}} Updates the value of "currentState" that is
  stored in a predefined environment.
  
  \code{\link{getCState}} Gets the vlaue of "currentState" that is
  stored in a predefined environment.
  
  \code{\link{assignColInfo}} Updates the values of "colInfos" (column
  information) that is stroed in a predefined environment.
  
  \code{\link{getColInfo}} Gets the values of "colInfos" (column
  information) that is stroed in a predefined environment.
  
  \code{\link{setColInfos}} Creates colInfo objects and sets the value
  of 'colInfos' list.

  \code{\link{changeState}} changes the state and thus the interface of
  a widget.

  \code{\ling{setNewState}} sets the state of a importing process.

  \code{\link{addArgs}} adds a new state to the argument list for
  states.

  \code{\link{dropArgs}} removes a state from the argument list for
  states.

  \code{\link{setSkip}} Sets the value for the number of lines to skip
  when readling a data file.

  \code{\link{moreArgs}} Gets some of the arguments for importing data
  using \code{\link{read.table}}.

  \code{\link{dropColumn}} Sets the index values for data columns that
  are going to be droped when read using \code{\link{read.table}}.

  \code{\link{setColName}} Sets the column names for a data file by
  getting column names from correct entry boxes.

  \code{\link{setColType}} Sets the column type for a data file by
  getting column type information from correct entry boxes.
}
\value{
  \code{\link{whatDeli}} returns a character string.

  \code{\link{getMoreArgs}} returns a list of arguments.

  \code{\link{getArgs}} returns a list of the arguments for read.table.

  \code{\link{getShowNum}} returns an integer for the number of rows to
  show.

  \code{\link{getCState}} returns a character string for the current
  state.

  \code{\link{getColInfo}} returns a colInfo object contains column
  information.
}

\author{Jianhua Zhang}
\note{The functions are part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics funcitonalities through R}

\seealso{\code{\link{importWizard}}}
\examples{
  # No example is given as functions require the set up of the working
  # environment. 
}
\keyword{misc}

