\name{args2XML}
\alias{args2XML}

\title{Converting the formal arguments to a function and converts into
  an XML format} 
\description{
  This function reads the formal arguments to a given function and
  converts the content into an XML format
}
\usage{
args2XML(fun, xml.name = "", full.names = NULL, priority = NULL)
}

\arguments{
  \item{fun}{\code{fun} name of the function of interests}
  \item{xml.name}{\code{xml.name} a character string for the name of the
    xml file holding the content of the formal arguments to the function}
  \item{full.names}{\code{full.names} a vector of character string with
    full description of each of the formal arguments to the
    function. The order of apperance of each description much correspond
    to the oreder of their appeerance in the formal argument list}
  \item{priority}{\code{priority} a vector of integers or character
    strings indicating the priority of the arguments.}
}
\details{
  Priority values are currently used to determine whether the argument
  will appear on a widget that has entry boxes for modifying the values
  of the arguments. Users of args2XML may not have any concern of the
  priority values
}
\value{
  No value will be returned.
}

\author{Jianhua Zhang}
\note{This function is part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through R}

\seealso{\code{\link{fileWizard}}}

\examples{
fullNames <- c("Full path names", "Pattern to match",
"Visiable file names", "Include path")
args2XML(list.files, "temp.xml", fullNames, c(1, 2, 2, 2)) 
readLines("temp.xml")
unlink("temp.xml")
}
\keyword{misc}

