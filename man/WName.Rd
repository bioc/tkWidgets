\name{WName}
\title{Accessors for Primitive Widget Objects}
\alias{WName}
\alias{WValue}
\alias{"WValue<-"}
\alias{WtoText}
\alias{WfromText}
\alias{WcanEdit}
\alias{WbuttonText}
\alias{WbuttonFun}
\alias{WwList}
\alias{"WwList<-"}
\alias{WLValue}
\alias{"WLValue<-"}
\alias{WRButtons}
\alias{WpreFun}
\alias{WpostFun}
\alias{WEnd}
\description{
  Currently primitive widget objects (\code{pwidget}s) are implemented
  as \code{\link{list}}s, but this will change.  Users should only rely
  on these accessors functions, not the implementation.
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
WwList <- value
WLValue(x, index)
WLValue(x, index) <- value
WRButtons(x)
WpreFun(x)
WpostFun(x)
WEnd{x}
}
\arguments{
  \item{x}{list of lists with a Name, Value, toText, fromText,
    canEdit, buttonText, buttonFun, preFun, postFun element.}
  \item{name}{character string for the name of the list to be returned}
  \item{index}{integer or character string as an index or name for
    the list in the first list of a list of lists for a widget.}
  \item{value}{An \R data type that is going to be used to update the value.}
}
\details{
  \code{WName(x)} returns the \code{Name} element of \code{x}.
  \code{WValue(x)} returns the \code{Value} element of \code{x}.
  \code{WValue(x) <- value} will change the \code{Value} element of
  \code{x} to \code{value}.

  \code{WtoText(x)} returns the \code{toText} element of \code{x}.
  \code{WfromText(x)} returns the \code{fromText} element of \code{x}.
  \code{WcanEdit(x)} returns the \code{canEdit} element of \code{x}.

  \code{WbuttonText(x)} returns the \code{buttonText} element of \code{x}.
  \code{WbuttonFun(x)} returns the \code{buttonFun} element of \code{x}.
  \code{WwList(x)} returns the \code{wList} element of \code{x}.
  \code{WwList <- value} will update the \code{wList} element of a
  Widget list with \code{value}.

%% FIXME: the following is not well understandable...
  \code{WLValue(x, index)} returns a list indicated by the
  index in the wList of a widget list.
  \code{WLValue(x, index) <- value} will update indicated
  by the index in the wList of a widget list with the value.

  \code{WpreFun(x)} returns the preFun element of \code{x}.
  \code{WpostFun(x)} returns  the post element of \code{x}.
  \code{WEnd(x)} returns  the end element of \code{x}.
}
\value{
  A character string or \R function represented by the element
  of the list whose value is to be retrieved.
}
\author{Jianhua (John) Zhang}
\seealso{\code{\link{widgetRender}} for references etc.
}
\examples{
# Create the list of lists
pW1 <- list(Name="AAA", Value="bbb",
            toText = function(x) paste(x,collapse = ","),
            fromText = NULL, canEdit = TRUE,
            buttonFun = ls, buttonText = "Browse")

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
WwList(widget1) <- list(Name = "New list", Value = "New value")
WLValue(widget1, 1)
WLValue(widget1, 1) <- "New value"
WpreFun(widget1)
WpostFun(widget1)
WEnd(widget1)
}
\keyword{manip}






