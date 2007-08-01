\name{dbArgsWidget}
\alias{dbArgsWidget}

\title{Function to build a widget for inputing database arguments}
\description{
  This functions creates a interactive widget to allow users to input
  arguments for database connection for Unix.
}
\usage{
dbArgsWidget()
}

\details{
  Database arguments include database name, user name, password, host
  name, and table name.
}
\value{
  The function returns a list containing the following elements:
  \item{dbname}{a charater string for the name of the database}
  \item{host}{a character string for the name or IP address of the host
    machine}
  \item{user}{a character string for the name of the user}
  \item{password}{a character string for the password}
  \item{tablename}{a character string for the name of the database table}
}
\references{Rdbi}
\author{Jianhua Zhang}

\seealso{\code{\link{argsWidget}}}
\examples{
if(interactive()){
    test <- dbArgsWidget()
}
}
\keyword{interface}

