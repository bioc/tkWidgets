\name{pickItems}
\alias{pickItems}

\title{Function that builds a widget to allow users to select items from
available sources} 
\description{
  Given a vector of characters, this function creates a widget
  containing list box to allow users to visually select elements from
  the vector.
}
\usage{
pickItems(items)
}

\arguments{
  \item{items}{\code{items} a vector for the available source elements
    to be selected}
}
\details{
  This function is to provide visual support to other functions and thus
  may not have much use otherwise.
}
\value{
  This function returns a vector of select items.
}
\references{R tcltk}
\author{Jianhua Zhang}
\note{This function is part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through R}

\seealso{\code{\link{dataViewer}}}
\examples{
options <- paste("Option", 1:10, sep = "")
if(interactive()){
    pickItems(options)
}
}
\keyword{interface}
