\name{objViewer}
\alias{objViewer}
\title{A function to show the content of the passed argument on a widget}
\description{
  This function takes an R object and shows the content in a list box on
  a widget.
}
\usage{
objViewer(toView, width = 40, height = 10)
}

\arguments{
  \item{toView}{An R object whose content is to be viewed}
  \item{width}{A numerical value for the width of the widget}
  \item{height}{A numerical value for the height of the widget}
}
\details{
  The function makes no check of the R object passed and will show
  whatever the object will be shown when the name is type at an R
  prompt. Formating is required before passing the R object to the function.
}
\value{
  This function does not return any value
}
\references{A Primer on the R-tcl/tk Package (R News Vol. 1/3, 2001)}
\author{Jianhua Zhang}
\note{This function is part of the Bioconductor project at Dana-Farber
  Cance Institute to provide Bioinformatics functionalities through R}

 ~Make other sections like WARNING with \section{WARNING }{....} ~

\seealso{\code{\link{listSelect}}}

\examples{
# Since user interference is required, the example code only runs
# interactively 
if(interactive())
objViewer("Just to show that the content gets posted") 
}
\keyword{interface}

