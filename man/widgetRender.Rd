\name{widgetRender}
\alias{widgetRender}

\title{A function to render a tk widget based on the specifications
  passed as a list.}
\description{
  This function takes a list that specifies the appearance and behavior
  of a tk widget and renders the widget. 
}
\usage{
widgetRender(iWidget, tkTitle)
}

\arguments{
  \item{iWidget}{A list of lists that specifies the appearance and
    behavior of the widget to be rendered.}
  \item{tkTitle}{a character string for the text to be appear in the
    title bar of the widget to be rendered.}
}
\details{
  The widget to be rendered normally consists of frames with three
  widgets arranged in a row. The first widget is normally a label for
  the name of the second widget. The second widget can be any type of
  widgets. The third widget is a button widget that defines some
  behavior to be associated with the second widget. For example, a
  button that will cause something to be displayed in the second widget
  when pressed. The third widget can be missing if no such association
  is required.

  The widget to be rendered also has two buttons at the bottom part of
  the widgets. The followings are the name and behavior of the buttons:

  Cancel - The unmodified list passed to the function at the time of
  invocation will be returned when pressed.

  End - A modified version of the passed list will be returned when
  pressed. The returned list has the same number of elements as the
  original one but with the values modified based on the entries in
  corresponding widgets items. 
}
\value{
  \item{list}{A list of lists with the original values of the passed
    modified or unmodified depending on whether the cancel or end button
    pressed.} 
}
\references{A Primer on the R-Tcl/Tk Package. R News val.1/3, 2001}
\author{Jianhua (John) Zhang}
\note{This function is part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through R.}

\seealso{\code{\link{fileBrowser}},\code{\link{objectBrowser}}}

\examples{
# Create the list to be passed
pW1 <- list(Name="AAA", Value="bbb", toText=function(x) paste(x,collapse
= ","), fromText=NULL, canEdit=TRUE, buttonFun = fileBrowser,
buttonText = "Browse") 

pW2 <- list(Name="BBB", Value="x,y,z", toText=function(x) paste(x,
collapse=","), fromText=NULL, canEdit=TRUE, buttonFun = ls,  
buttonText = "List")

pW3 <- list(Name="CCC", Value="ccc",toText=function(x) paste(x, collapse
= ","), fromText=NULL, canEdit=TRUE, buttonFun=NULL,  buttonText=NULL)

widget1 <- list(wList = list(a = pW1, b = pW2, c = pW3),preFun =
function() "Hi", postFun = function() "Bye")

# Call the funciton 

if(interactive()){
    x <- widgetRender(widget1, "Test Widget")
}
}
\keyword{interface}




