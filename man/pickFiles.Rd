\name{pickFiles}
\alias{pickFiles}

\title{Pick Elements From Vector of Strings}
\description{
 Takes a vector of strings and then checks to see if the
  predefined conditions are met for each element.  Elements that meet the
  conditions will be included in the vector returned and the others not.
}
\usage{
pickFiles(fileNames, fun = function(x) TRUE,
          prefix = NULL, suffix = NULL, exclude = .Platform$file.sep)
}
\arguments{
  \item{fileNames}{vector of strings that will be checked.}
  \item{fun}{function to be used to check the strings.  Default is no checking.}
  \item{prefix}{character used to check to see if strings in the vector
    have the prefix.}
  \item{suffix}{character used to check to see if strings in the vector
    have the suffix.}
  \item{exclude}{character string with which strings in the vector
    will be excluded form the checking.  The default is to exclude all
    the directory names and always return them.}
}
\details{
  The function fun will be used only when both prefix and suffix are
  NULL. If a prefix is not NULL, that prefix will be checked. A suffix
  is going to be checked when prefix is NULL.
}
\value{
  Character vector of file names satisfying the conditions.}
}

\author{Jianhua Zhang}
\note{This function is part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through R.}

\seealso{\code{\link{fileBrowser}}, \code{\link{hasPrefix}}, \code{\link{hasSuffix}} }

\examples{
# Return every thing from the current directory
pickFiles(list.files())

# Create a temp file
file.create("myFile")

# Returns subdirectory names and file names with a prefix of "my"
pickFiles(list.files(), prefix = "my")

# create another temp file
file.create("temp.tex")

# Return subdirectory names and file names with a suffix of ".tex"
pickFiles(list.files(), suffix = ".tex")

# clearn up
unlink("myFile")
unlink("temp.tex")
}
\keyword{manip}













