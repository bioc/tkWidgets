\name{importPhenoData}
\alias{importPhenoData}
\alias{createPData}
\alias{writePDRowNames}
\alias{writePhenoTable}
\alias{makePhenoData}
\alias{convert2PData}
\alias{getOBJWidget}
\alias{objExists}
\alias{getSNCNums}
\alias{getCovarDesc}
\title{Functions to input data for an AnnotatedDataFrame object}
\description{
  This functions allow users to read data from an existing file or an R
  data.frame object and use the data frame to construct an
  AnnotatedDataFrame object. 
}
\usage{
importPhenoData(fileName, sampleNames = NULL, from = NULL)
createPData(pdata, varList)
writePDRowNames(pdata, sampleNames)
writePhenoTable(base, textWidget, pdata)
makePhenoData(pdata)
convert2PData(phenoList)
getOBJWidget(type = NULL)
objExists(name, type = NULL)
getSNCNums(sampleNames)
getCovarDesc(varList)
}
\arguments{
  \item{fileName}{a character string for the name of a file that is going to be
    used to build an AnnotatedDataFrame object.}
  \item{sampleNames}{a vector of character strings for the names of samples. The
    length of \code{sampleNames} should be the same as the number of rows of an
    existing file or data.frame if an AnnotatedDataFrame object is to be created
    based on a file or data.frame.}
  \item{pdata}{a data.frame for the experimental data.}
  \item{base}{an RTcl object for the base window a widget resides.}
  \item{textWidget}{an RTcl object for a text box widget.}
  \item{phenoList}{a list of lists for tclVar() objects.}
  \item{type}{a character string for the class of a object e.g. data.frame, AnnotatedDataFrame.}
  \item{name}{a character string for the name of an object.}
  \item{varList}{a list of characters with names being covariate names and values
    being short descriptions of covariate names.}
  \item{from}{a character string indicating how an AnnotatedDataFrame object will
    be created. "file" - create from an existing file, "object" - create from an
    existing data frame object, "edit" - create by editing an existing
    AnnotatedDataFrame object, and "new" create a new AnnotatedDataFrame object
    from scratch. NULL or any other values for \code{from} will invoke a widget
    that allows users to select one of the four means from an interface.}
}
\details{
  When import a data.frame or AnnotatedDataFrame object, the object to be
  imported should have been stored in .GlobelEnv. All the objects of
  data.frame or AnnotatedDataFrame will be made available through
  a browser.

  The main widget if \code{importPhenoData} that calls other
  functions/widgets to have the job done.

  Package Biobase is required for importPhenoData but the requirement id
  not forced as it is the only time the package is used. Users have to
  make sure that Biobase is available.
}
\value{
  An AnnotatedDataFrame object.
}
\references{AnnotatedDataFrame class in Biobase}
\author{Jianhua Zhang}
\note{This function is intended for use by function read.phenoData of Biobase}
\examples{
if(interactive()){
    importPhenoData()
}
}
\seealso{\code{\link[Biobase:class.AnnotatedDataFrame]{AnnotatedDataFrame-class}}} 
\keyword{interface}
