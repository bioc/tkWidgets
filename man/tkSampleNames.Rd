\name{tkSampleNames}
\alias{tkSampleNames}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Simple interface to associate sample names with files}
\description{
This widget provides an interface to enter names to be associated with
files containing array expression information related to a particular
sample.}

}
\usage{
tkSampleNames(..., filenames = character(0))
}
\arguments{
  \item{\dots}{ the filenames to be associated with a sample name,
    supplied individually and/or as a  character vector}
  \item{filenames}{a character vector of filenames to be associated
    with a sample name.}
}
\details{
  \code{\link[Biobase]{phenoData-class}} objects will use sample names as row
    names for the \code{pData} slot. The colnames of the expression
    matrices in \code{\link[Biobase]{exprSet-class}} use this as well. Many
      times, each of these columns are obtained from a file. Rather than
      use the, sometimes ugly, filename we can use the sample names that
      this interface associates with each file.
      
      The function returns a character matrix intended to be used to
      create sample
      names in phenoData and exprSet. However, the function can be used
      independetly of the Biobase package.
    }
\value{
  A character matrix with the first column the filenames the second
  column the sample names to associate.
}
\author{Majnu John}
\keyword{interface}% __ONLY ONE__ keyword per line
