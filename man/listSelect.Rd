\name{listSelect}
\alias{listSelect}
\alias{writeSelBox}
\alias{writeBut}
\title{Functions that make a widget with selection boxes based on user
  specifications} 
\description{
  These functions creates a widget that allows users to view and make
  selections of items shown on the interface.
}
\usage{
listSelect(aList, topLbl = "Select Elements From The Following List", typeFun = stdType, valueFun = objViewer)
}

\arguments{
  \item{aList}{A list with names and object pairs (e. g. a = "AAA")}
  \item{topLbl}{A character string for the text to be shown as a title}
  \item{typeFun}{An R funtion that takes an R object as an arguement and
    returns a description of the object}
  \item{valueFun}{An R function that takes an R object as an argument
    and show the content of the object. The function should get the
    representation of the object and calls function objViewer to have
    the representation rendered in a widget}
}
\details{
   Both typeFun and valueFun have to take an argument (the R object to
   be shown). It works well for the valueFun function to call the
   objViewer function with whatever to be shown passed to objViewer as
   an argument.
}
\value{
   listSelect returns a list with the names of the R objects in the
   original list associated with TRUE (selected) or FALSE (deselected).
}
\author{Jianhua Zhang}
\note{The functions are part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioninformatics functionalities through R}

\seealso{\code{\link{objViewer}}}

\examples{
# Since user interference is required, the example code does not run
# automatically
aList <- list(a = "AAA", b = c(123, 456, 789), c =
as.data.frame(matrix(1:10, ncol = 2)), d = stdType)
\dontrun{listSelect(aList)}
}
\keyword{interface}
