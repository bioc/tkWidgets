\name{getWvalues}
\alias{getWvalues}

\title{Obtaining values of widgets on a given widget}
\description{
   This function returns a list containing the values for widgets on a
   widget created by function widgetRender. It takes a list defining the
   widget and returns a named list containing the values for each of the
   widgets.
}
\usage{
getWvalues(W)
}

\arguments{
  \item{W}{\code{W} a list of lists defining the widgets that are used
    to make a widget}
}
\details{
  For a widget containing 3 widgets each with some associated
  functionalities, the list is defined as this:

  pW1 <- list(Name="AAA", Value="bbb",
            toText=function(x) paste(x,collapse= ","), fromText=NULL,
            canEdit=TRUE,
            buttonFun = fileBrowser, buttonText = "Browse")

  pW2 <- list(Name="BBB", Value="x,y,z",
            toText=function(x) paste(x, collapse=","), fromText=NULL,
            canEdit=TRUE, buttonFun = ls, buttonText = "List")

  pW3 <- list(Name="CCC", Value="ccc",
            toText=function(x) paste(x, collapse = ","), fromText=NULL,
            canEdit=TRUE, buttonFun=NULL,  buttonText=NULL)

  widget1 <- list(wList = list(a = pW1, b = pW2, c = pW3),
                preFun  = function() "Hi",
                postFun = function() "Bye")

  widget1 will be used to create a widget with 3 entry boxes. When users
  modify the values through the widget created, new values will be kept
  in the list and widget1 will be returned up exist. getWvalues is
  useful to capture the values for each widgets on the widget.
}
\value{
  This function returns a list of:

  \item{comp1 }{Description of `comp1'}
  \item{comp2 }{Description of `comp2'}

}

\author{Jianhua Zhang}
\note{This function is part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics funcitonalities through R}

\seealso{\code{\link{widgetRender}}}

\examples{
# Define the widgets
  pW1 <- list(Name="AAA", Value="bbb",
            toText=function(x) paste(x,collapse= ","), fromText=NULL,
            canEdit=TRUE,
            buttonFun = fileBrowser, buttonText = "Browse")

  pW2 <- list(Name="BBB", Value="x,y,z",
            toText=function(x) paste(x, collapse=","), fromText=NULL,
            canEdit=TRUE, buttonFun = ls, buttonText = "List")

  pW3 <- list(Name="CCC", Value="ccc",
            toText=function(x) paste(x, collapse = ","), fromText=NULL,
            canEdit=TRUE, buttonFun=NULL,  buttonText=NULL)

  widget1 <- list(wList = list(a = pW1, b = pW2, c = pW3),
                preFun  = function() "Hi",
                postFun = function() "Bye")

  if(interactive()){
      tt <- widgetRender(widget1, "try")
      getWvalues(tt)
  }else{
      getWvalues(widget1)
  }
}
\keyword{misc}

