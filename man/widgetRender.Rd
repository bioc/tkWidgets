\name{widgetRender}
\alias{widgetRender}

\title{Render a Tk Widget from Given Specifications}
\description{
  This function takes a list that specifies the appearance and behavior
  of a Tk widget and renders the widget accordingly.
}
\usage{
widgetRender(iWidget, tkTitle)
}

\arguments{
  \item{iWidget}{list of lists that specifies the appearance and
    behavior of the widget to be rendered.}
  \item{tkTitle}{character string for the text to appear in the
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

  \describe{
    \item{\bold{Cancel} -}{The unmodified list passed to the function at
      the time of invocation will be returned when pressed.}

    \item{\bold{End} -}{A modified version of the \code{iWidget}
      argument will be returned when pressed.  The returned list has the
      same number of elements as the original one but with the values
      modified based on the entries in corresponding widgets items.}
    }
}
\value{
  A \code{\link{list}} of lists with the original values of the passed
  modified or unmodified depending on whether the cancel or end button
  pressed.}
}
\references{
  Peter Dalgaard (2001)
  A Primer on the R-Tcl/Tk Package;
  R News \bold{1} (3), 27--31
  \url{http://CRAN.R-project.org/doc/Rnews/}
}
\author{Jianhua (John) Zhang}
\note{This function is part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through \R.}

\seealso{\code{\link{fileBrowser}},
  \code{\link{objectBrowser}}.
}

\examples{
# Create the list to be passed
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

# Call the function
if(interactive()){
    x <- widgetRender(widget1, "Test Widget")
    str(x)
}
}
\keyword{interface}




