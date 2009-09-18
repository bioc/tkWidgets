\name{tkphenoData}
\alias{tkphenoData}
\title{Simple interface to enter AnnotatedDataFrame}
\description{
This widget provides an interface to create
\code{\link[Biobase:class.AnnotatedDataFrame]{AnnotatedDataFrame-class}}
instances. 
}
\usage{
tkphenoData(sampleNames)
}
\arguments{
  \item{sampleNames}{sampleNames for which we will enter phenotypic data.}
}
\details{
  The function returns a list of character matrices intended to be used
  as the \code{pData} and \code{varLabels} slots of an instance of
  \code{\link[Biobase:class.AnnotatedDataFrame]{AnnotatedDataFrame-class}}.
}
\value{
  A list of two matrices
  \item{pData}{a character matrix containing phenotypic data.}
  \item{varLabels }{a character vector with covariate description.}
}
\author{Majnu John} 
\seealso{\code{\link[Biobase:class.AnnotatedDataFrame]{AnnotatedDataFrame-class}}}
\keyword{interface}
