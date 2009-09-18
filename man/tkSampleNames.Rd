\name{tkSampleNames}
\alias{tkSampleNames}
\title{Simple interface to associate sample names with files}
\description{
This widget provides an interface to enter names to be associated with
files containing array expression information related to a particular
sample.}
\usage{
tkSampleNames(\dots, filenames = character(0))
}
\arguments{
  \item{\dots}{ the filenames to be associated with a sample name,
    supplied individually and/or as a  character vector}
  \item{filenames}{a character vector of filenames to be associated
    with a sample name.}
}
\details{
  \code{\link[Biobase:class.AnnotatedDataFrame]{AnnotatedDataFrame-class}}
  objects will use sample names 
  as row names for its pData. The colnames of the expression matrices in
  \code{\link[Biobase:class.ExpressionSet]{ExpressionSet-class}} use
  this as well. Many times, each 
  of these columns are obtained from a file. Rather than use the,
  sometimes ugly, filename we can use the sample names that this
  interface associates with each file. 

  The function returns a character matrix intended to be used to create
  sample names in AnnotatedDataFrame and ExpressionSet. However, the
  function can be used independently of the Biobase package.
}
\value{
  A character matrix with the first column the filenames the second
  column the sample names to associate.
}
\author{Majnu John}
\seealso{\code{\link[Biobase:class.AnnotatedDataFrame]{AnnotatedDataFrame-class}}}
\keyword{interface}
