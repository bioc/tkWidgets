\name{WName}
\alias{WName}
\alias{WValue} 
\alias{"WValue<-"} 
\alias{WtoText}
\alias{WfromText}
\alias{WcanEdit}
\alias{WbuttonText}
\alias{WbuttonFun}
\alias{WwList}
\alias{WpreFun}
\alias{WpostFun}

\title{Accessor functions for primitive widget objects.}
\description{
  Currently primitive widget object (pwidgets) are implemented as lists,
  but this will change. Users should only rely on these accessor
  functions, not the implementation.
}
\usage{
WName(x)
WValue(x) 
WValue(x) <-  value 
WtoText(x)
WfromText(x)
WcanEdit(x)
WbuttonText(x)
WbuttonFun(x)
WwList(x)
WpreFun(x)
WpostFun(x)
}

\arguments{
  \item{x}{A list of lists with a Name, Value, toText, fromText,
    canEdit, buttonText, buttonFun, preFun, postFun element.}
  \item{value}{A character string to be assigned to the Name or Value
    element of the list of lists passed}
}
\details{
  WName(x) will return the value of the Name element of the list passed.
  WValue(x) will return the value of the Value element of the list passed.
  WValue(x) <- value will change the value of the Value element of the list
  passed to the value specified by "value" passed.
  WtoText(x) will return the value of the toText element of the list
  passed.
  WfromText(x) will return the value of the fromText element of the list
  passed.
  WcanEdit(x) will return the value of the canEdit element of the list
  passed.
  WbuttonText(x) will return the value of the buttonText element of the
  list passed.
  WbuttonFun(x) will return the value of the buttonFun element of the
  list passed.
  WwList(x) will return the value of the wList element of the list
  passed.
  WpreFun(x) will return the value of the preFun element of the list
  passed.
  WpostFun(x) will return the value of the post element of the list passed.
}
\value{
  \item{x}{A character string or R function represented by the element
    of the list whose value is to be retrieved.}
}

\author{Jianhua (John) Zhang}
\note{This function is part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through R.}

\seealso{\code{\link{widgetRender}}}

\examples{
# Create the list of lists
pW1 <- list(Name="AAA", Value="bbb",
              toText=function(x) paste(x,collapse = ","),
              fromText=NULL, canEdit=TRUE, buttonFun = fileBrowser,
              buttonText = "Browse")

widget1 <- list(wList = list(pW1),
                  preFun = function() "Hi",
                  postFun = function() "Bye")

# Call the functions
WName(pW1)
WValue(pW1) 
WValue(pW1) <- "lll" 
WtoText(pW1)
WfromText(pW1)
WcanEdit(pW1)
WbuttonText(pW1)
WbuttonFun(pW1)
WwList(widget1)
WpreFun(widget1)
WpostFun(widget1)
}
\keyword{manip}






