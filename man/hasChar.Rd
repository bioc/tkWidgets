\name{hasChar}
\alias{hasChar}
\alias{hasPrefix}
\alias{hasSuffix}

\title{String Prefix and Suffix Checking}
\description{
  These functions return a \emph{function} for determining if a
  given prefix, suffix, or set of characters passed to this function
  exists in a character string passed to the returned function.
}
\usage{
hasChar(toCheck, what = "")
hasPrefix(aPrefix)
hasSuffix(aSuffix)
}
\arguments{
  \item{aPrefix}{character string to be used as the prefix to be checked for}
  \item{aSuffix}{character string to be used as the suffix to be checked for}
  \item{toCheck}{\code{toCheck} a character string to be used to check
    to see if it exists in a character string passed to the returned
    function}
  \item{what}{\code{what} a character string defining whether toCheck
    will be used as a prefix (what = "prefix"), suffix (what =
    "suffix"), or a set of characters (what = "") to check the character
    string passed to the returned function} 
}
\details{
  The prefix (or suffix) is passed to \code{hasPrefix} (or
  \code{hasSuffix}) and then the returned function can be used to check
  for the existence of that prefix (suffix) in a string passed to that
  function.

  hasChar is a more general function that determines the existence of
  prefix, sufix, or a set of a characters in a character string passed
  to the returned function.
}
\value{
  A \bold{function} which itself returns a logical (of length 1)
  indicating if the prefix/suffix is found (\code{\link{TRUE}}) or not.
}

\author{Jianhua (John) Zhang}

\seealso{\code{\link{pickFiles}}}

\examples{
# Function for checking for a prefix "xxx" :
chkPfun <- hasChar("xxx", what = "prefix")
# Function for checking for a suffix ".tex" :
chkSfun <- hasChar(".tex", what = "suffix")

chkPfun("xxx.tex")
chkPfun(".xxx")
chkSfun("xxx.tex")
chkSfun("yyyyy")
chkSfun("yxxx.text")
}
\keyword{manip}
