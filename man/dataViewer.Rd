\name{dataViewer}
\alias{dataViewer}
\title{Function to view a data object passed}
\description{
  This function creates a widget to allow users to view the content of a
  data frame passed and decide whether to save the data or not.
}
\usage{
dataViewer(data, caption = "", save = TRUE)
}

\arguments{
  \item{data}{\code{data} a data frame (or alike) to be viewed}
  \item{caption}{\code{caption} a character string for the title of the
    widget}
  \item{save}{\code{save} a boolean to indicate whether to have the
    option to allow users to save the data}
}
\details{
  Taking a data frame as one of the arguments, this function builds a
  widget that allows users to view the content of the data and save the
  data as a file.
}
\value{
  This function does not return any value
}
\references{R tcltk}
\author{Jianhua Zhang}
\note{This function is part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through R}

\seealso{\code{\link{importWizard}}}
\examples{
# Create matrix
data <- matrix(1:20, ncol = 4)
if(interactive()){
    # View data using dataViewer
    dataViewer(data, "test", TRUE)
}
}
\keyword{interface}
