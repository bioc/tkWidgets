\name{objViewer}
\alias{objViewer}
\title{Show the Content of an R Object in a Widget}
\description{
  This function takes an \R object and shows the content in a list box on
  a widget.
}
\usage{
objViewer(toView, width = 40, height = 10)
}

\arguments{
  \item{toView}{\R object whose content is to be viewed}
  \item{width, height}{positive values specifying the width and height
    of the widget.}
}
\details{
  The function makes no check of the \R object passed and will show
  whatever the object will be shown when the name is type at an \R
  prompt.  Formatting is required before passing the \R object to the function.
}
\value{
  This function does not return any value
}
\author{Jianhua Zhang}
\note{This function is part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through \R.
}
\seealso{\code{\link{listSelect}}}

\examples{
# Since user interference is required, the example code only runs
# interactively
if(interactive())
   objViewer("Just to show that the content gets posted")
}
\keyword{interface}

