\name{importPhenoData}
\alias{importPhenoData}
\alias{createPhenoData}
\alias{writePDRowNames}
\alias{writePhenoTable}
\alias{makePhenoData}
\alias{convert2PData}
\alias{getPDFromFile}
\alias{getPDFromObj}
\alias{getNameWidget}
\alias{objExists}
\alias{readFileByUserArgs}
\alias{getSNCNums}

\title{Functions to input data for a phenoData object}
\description{
  This functions allow users to read data from an existing file or an R
  data frame object and use the data frame to construct a phenoData object
}
\usage{
importPhenoData(sampleNames)
createPhenoData(pdata)
writePDRowNames(pdata, sampleNames)
writePhenoTable(base, textWidget, pdata)
makePhenoData(pdata)
convert2PData(phenoList)
getPDFromFile()
getPDFromObj(type = c("data.frame", "phenoData"))
getNameWidget(what = c("file", "object"), type = NULL)
objExists(name, type = NULL)
readFileByUserArgs(fileName, keep = c("file", "header", "sep", "nrows",
"skip", "strip.white", "comment.char"))
getSNCNums(sampleNames)
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
  \item{base}{\code{base} an RTcl object for the base window a widget
    resides}
  \item{textWidget}{\code{textWidget} an RTcl object for a text box
    widget}
  \item{phenoList}{\code{phenoList} a list of lists for tclVar()
    objects}
  \item{type}{\code{type} a character string for the class of a object
    e. g. data.frame, phenoData}
  \item{name}{\code{name} a character string for the name of an object
    in .GlobalEnv}
  \item{fileName}{\code{fileName} a character string for the name of
    file to be read using read.table}
  \item{keep}{\code{keep} a vector of character strings for the formal
    arguments of read.table that require user inputs through a widget}
}

\details{
  When import a data frame or phenoData object, the object to be
  imported should have been stored in .GlobelEnv. All
  the objects of data frame or phenoData will be made available through
  a browser.

  The main widget if \code{importPhenoData} that calls other
  functions/widgets to have the job done.

  Package Biobase is required for importPhenoData but the requirement id
  not forced as it is the only time the package is used. Uers have to
  make sure that Biobase is available.
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
