\name{importWizard}
\alias{importWizard}
\alias{whatDeli}
\alias{numberChar}
\alias{getMoreArgs}

\title{A widget for importing data to R}
\description{
  This function constructs a widget that allows users to inport data
  file to R. The imported data will be returned as an R data frame
}
\usage{
importWizard(filename = "", maxRow = 200)
whatDeli(delimiter)
numberChar(matr)
getMoreArgs()
}

\arguments{
  \item{filename}{\code{filename} a character string for the name of the
    file to be imported. The default is an empty string and users have to
    click a browse button to get the file name through
  \code{\link{fileBrowser}}} 
  \item{maxRow}{\code{maxRow} an integer for the maximum number of rows
    of the data file to be imported to be shown on the widget. The
    default is 200 rows}
}
\details{
  This function mimics the interface of MS Excel and collects arguments
  for the function \code{\link{read.table}}.
  
  Due to performace concern, a maximum number of rows (maxRow) set by
  users will be displayed. Overly long data set may cause slow response
  of the system. 
}
\value{
  A data frame containing the data to be imported will be returned
}

\author{Jianhua Zhang}
\note{This function is part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through R}

\seealso{\code{\link{fileBrowser}}, \code{\link{argsWidget}}}

\examples{
  if(interactive()){
      importWizard()
  }
}
\keyword{interface}

