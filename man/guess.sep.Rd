\name{guess.sep}
\alias{guess.sep}
\alias{guess.header}
\alias{find.type}
\alias{charOrNum}
\alias{getRowNames}

\title{Automatically determines whether a data file has a header and
  what delimiter is used to separate data columns}
\description{
  This function reads a few lines from a data text file and determines
  whether a header exists, what the delimiter, and what data type each
  column is for a given file.
}
\usage{
guess.sep(file.name, numLine = 5, seps = "", isFile = TRUE)
guess.header(twoLines, sep)
find.type(file.name, sep, header = FALSE, numLine = 5, isFile = TRUE)
charOrNum(vect)
getRowNames(file.name, sep, header, skip)
}

\arguments{
  \item{file.name}{\code{file.name} a character string for the name of
    the file of interests}
  \item{numLine}{\code{n} an integer or character indicating the total number
    of lines to be read from the file for making the determination}
  \item{seps}{\code{seps} a vector of characters of potential delimiters
    to be used to make the determination. Default delimiters include " ",
    ",", ";", and "\\t". Anything inaddition to the default will have to
    be included in seps}
  \item{twoLines}{\code{twoLines} a vector of character string including
    the first two lines of a file that will be used to determine whether
    the file has a header}
  \item{sep}{\code{sep} a character for the delimiter used to separate
    columns in a file}
  \item{vect}{\code{vect} a vector of character or numeric string}
  \item{header}{\code{header} a boolean indicating whether a file has
    headers}
  \item{isFile}{\code{isFile} a boolean that is TRUE when
    \code{file.name} is a file or FALSE an object}
  \item{skip}{\code{skip} an integer for the number of lines to be
    skiped using \code{\link{read.table}}}
}
\details{
  guess.sep calls \code{\link{guess.sep}} and \code{\link{find.type}} to
  determine the header, delimiter, and column data type of a file.

  \code{\link{charOrNum}} determines which elements of a vector are
  numeric or character.
}
\value{
  This function returns a list containing
  \item{header}{TRUE if there is a header and FALSE otherwise}
  \item{separater}{A character string indicating the delimiter used}
  \item{type}{A vector of character strings that are either character or
    numeric} 
}
\author{Jianhua Zhang}

\seealso{\code{\link{fileWizard}}}

\examples{
# Create a temp file
tempData <- matrix(1:20, ncol = 4)
write.table(tempData, file = "tempData", quote = FALSE, sep =
"\t", row.names = FALSE, col.names = TRUE)

guess.sep("tempData")

unlink("tempData")
}
\keyword{misc}
