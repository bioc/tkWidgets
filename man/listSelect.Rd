\name{listSelect}
\alias{listSelect}
\alias{writeSelBox}
\alias{writeBut}
\alias{writeLabel}
\title{Utilities Creating a Widget With Selection Boxes}
\description{
  These functions create a widget with selection boxes allowing users to
  view and make selections of items shown on the interface.
}
\usage{
listSelect(aList, topLbl = "Select Elements From The Following List",
           typeFun = stdType, valueFun = stdView)
writeSelBox(baseW, aList, typeFun = NULL, valueFun = NULL)
   writeBut(baseW, butList, butWidth = 6)
writeLabel(baseW, typeFun, valueFun)
}

\arguments{
  \item{aList}{list with names and object pairs (e. g. \code{a = "AAA"}).}
  \item{topLbl}{character string for the text to be shown as a title.}
  \item{typeFun}{function that takes an \R object as an arguement and
    returns a description of the object.}
  \item{valueFun}{function that takes an \R object as an argument
    and shows the content of the object.  The function should get the
    representation of the object and calls \code{\link{objViewer}} to have
    the representation rendered in a widget.}
  \item{baseW}{a window widget to which the selection boxes will be put.}
  \item{butList}{a list with names and function pairs that define the
    name and behavior of buttons to be put on the widget to be
    generated}
  \item{butWidth}{numerical value specifying the width of buttons to
    be created.}
}
\details{
  Both \code{typeFun} and \code{valueFun} have to take an argument (the
  \R object to be shown).  It works well for the \code{valueFun}
  function to call \code{\link{objViewer}()} with whatever to be shown
  passed to \code{objViewer} as an argument.
}
\value{
   \code{listSelect()} returns a list with the names of the \R objects in the
   original list associated with \code{TRUE} (selected) or \code{FALSE}
   (deselected).
}
\author{Jianhua Zhang}
\seealso{\code{\link{objViewer}}}

\examples{
aList <- list(a = "AAA", b = c(123, 456, 789),
              c = as.data.frame(matrix(1:10, ncol = 2)),
              d = stdType)
# Since user interference is required, the example code does not run
# automatically
if(interactive())
  listSelect(aList)
}
\keyword{interface}
