\name{stdType}
\alias{stdType}
\alias{stdView}
\title{Provide Default Behavior for listSelect Helper Functions}
\description{
  The function \code{\link{listSelect}} takes two functions which define
  how the type information and content of \R objects will be shown on
  the widget created by \code{listSelect}.
  Functions \code{stdType()} and \code{stdView()} provide the default
  behavior.
}
\usage{
stdType(toCheck)
stdView(toView)
}
\arguments{
  \item{toCheck, toView}{arbitrary \R object.}
}
\details{
  These functions can be viewed as exmaples of defining functions for
  the \code{typeFun} and \code{valueFun} arguments of the
  \code{\link{listSelect}} function.
}
\value{
  %% FIXME:
  \code{stdType()} returns a character string describing the type of the
  \R object.

  \code{stdView()}
}
\author{Jianhua Zhang}

\seealso{\code{\link{listSelect}}}

\examples{
stdType(123)
stdType("What am I")

str(mydf <- data.frame(x = 2:8, ch = letters[1:7]))
stdType(mydf)# "list"
stdType(stdType)

if(interactive()) {## stdView() needs UI:
  stdView(1:10)
  stdView(mydf)
 }
}
\keyword{misc}

