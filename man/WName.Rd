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
\alias{WListByName}
\alias{WListNewValue}
\alias{WpreFun}
\alias{WpostFun}
\alias{WEnd}

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
WListByName(x, name)
WListNewValue(x, index, newVal)
WpreFun(x)
WpostFun(x)
WEnd{x}
}

\arguments{
  \item{x}{A list of lists with a Name, Value, toText, fromText,
    canEdit, buttonText, buttonFun, preFun, postFun element.}
  \item{name}{A character string for the name of the list to be returned}  
  \item{index}{An integer as an index for the list in the first list of
    a list of lists for a widget.}
  \item{newVal}{An R data type that is going to be used to update the value.}
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
  WListByName(x, name) will return the list with the name = "name" that
  is contained by list x.
  WListNewValue(x, index, newVal) will update the value of list in the
  first list of a widget list as indicated by the index number.
  WpreFun(x) will return the value of the preFun element of the list
  passed.
  WpostFun(x) will return the value of the post element of the list passed.
  WEnd(x) will return the value of the end element of the list passed.
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

widget1 <- list(wList = list(a = pW1),
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
WListByName(widget1, "a")
WListNewValue(widget1, 1, "New Value")
WpreFun(widget1)
WpostFun(widget1)
WEnd(widget1)
}
\keyword{manip}






