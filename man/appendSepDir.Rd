\name{appendSepDir}
\alias{appendSepDir}

\title{List File and Directories for a Given Path}
\description{
  Takes a path and returns a vector of string with the
  names of files and directories for the path. The directory names will
  have a system dependent path separator (e. g. / for Unix) appended.
}
\usage{
appendSepDir(path)
}
\arguments{
  \item{path}{\code{path} character string specifying the path whose
    contents are to be listed}
}
\value{
  A character vector containing file and directory names.
}
\author{Jianhua (John) Zhang}
\note{This function is part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through R.}

\seealso{\code{\link{fileBrowser}}, \code{\link{pickFiles}}}

\examples{
# File names and directory names are not differentiated
list.files()

# Put a separater at the end of directory names
appendSepDir(getwd())
}
\keyword{manip}



