\name{argsWidget}
\alias{argsWidget}
\alias{getPWidget}
\alias{formatArg}
\alias{getSymbol}
\alias{funcs2Char}
\alias{getTrueNullNa}
\title{Functions to construct a widget that takes inputs from users}
\description{
   Given a argument list, the functions construct a widget to allow
   users to entry or select values for arguments defined by the names of
   the argument list.
}
\usage{
argsWidget(argsList, defaultNames, inst = "")
getPWidget(argsList, PWEnv, inst = "")
formatArg(toFormat)
getSymbol(args)
funcs2Char(args,funcs)
getTrueNullNa(toFormat)
}

\arguments{
  \item{argsList}{\code{argsList} a list of arguments with or without
    default values. The list can be derived from
    e.g. \code{\link{formals}}}
  \item{PWEnv}{\code{PWEnv} an R environment used object to store data for
    the argument list}
  \item{toFormat}{\code{toFormat} element to be formated by
    \code{\link{formatArg}}}
  \item{args}{\code{args} arguments to be formated}
  \item{funcs}{\code{funcs} a list containing the arguments that are
    functions}
  \item{defaultNames}{\code{defaultNames} a vector of character strings
    of length 2 for two default buttons to be rendered. The first one is
    to end the process and the second to abort the process}
  \item{inst}{\code{inst} a character string for a short instuction that
    will appear on the top of a widget}
}
\details{
  \code{\link{argsWidget}} builds a widget with widget elements to allow
  users to input values for the arguments.

  \code{\link{getPWidget}} instatiates primary widgets that will be used
  to construct the widget for argements.

  \code{\link{formatArg}} formats the value for a given argument so that
  the calue can be displayed by a tcltk widget.

  \code{\link{getSymbol}} filters out functions from the argument list.

  \code{\link{funcs2Char}} converts functions to character
  representations of the functions.

  \code{\link{getTrueNullNa}} converts string "true", "false", "null",
  and "na" to R primitives for these items.
}
\value{
  \code{\link{argsWidget}} returns a list with user input values for
  elements of the argument list passed.

  \code{\link{getPWidget}} returns a list of primary widgets.

  \code{\link{formatArg}} returns a list containing the formated values.

  \code{\link{getSymbol}} returns a list containing arguments that are
  functions.

  \code{\link{funcs2Char}} returns a list containing character
  representations of functions.

   \code{\link{getTrueNullNa}} returns an R object.
}
\references{R tcltk}
\author{Jianhua Zhang}
\note{The functions are part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through R}

\seealso{\code{\link{basicPW-class}}}
\examples{
  if(interactive()){
      argsWidget(list("Entry with default" = "default",
                      "Entry without default" = ""))
  }
}
\keyword{interface}

