\name{importPhenoData}
\alias{importPhenoData}
\alias{createPhenoData}
\alias{showIOError}

\title{A function to input data for a phenoData object}
\description{
  This function allows users to read data from an existing file or an R
  data frame object and use the data frame to construct a phenoData object
}
\usage{
importPhenoData(sampleNames)
createPhenoData(pdata, sampleNames)
showIOError(what)
}

\arguments{
  \item{sampleNames}{\code{sampleNames} a vector of character strings
    for the names of samples. The length of \code{smapleNames} should be
    the same as the number of rows of an existing file or data frame if
    a phenoData object is to be created based on a file or data frame}
  \item{pdata}{\code{pdata} a data frame for the experimental data}
  \item{what}{\code{what} a character string to indicate whether a name
    for a file ("file"), data frame ("data frame"), or phenoData
    ("phenoData") has been entered to a corresponding entry box on the
    interface}
}

\details{
  When import a data frame, the default path will be the .GlobelEnv. All
  the data frame objects will be listed in a browser.
}
\value{
  A phenoData object
}
\references{phenoData class in Biobase}
\author{Jianhua Zhang}
\note{This function is intended for use by funciton read.phenoData of Biobase}

\examples{

}
\keyword{ interface }
