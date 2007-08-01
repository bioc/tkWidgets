\name{getLightTW}
\alias{getLightTW}

\title{Function to create a light weight widget showing a text string}
\description{
  Given a text string and coordinations, this function creates a light
  weight tcltk widget with showing the text string passed.
}
\usage{
getLightTW(x, y, text)
}

\arguments{
  \item{x}{\code{x} an interger for the horizontal position for the
    widget to appear}
  \item{y}{\code{y} an integer for the vertical position for the widget
    to appear}
  \item{text}{\code{text} a character string to be show in the widget}
}
\details{
  When the function is invoked, a box containing the text will appear at
  the position specified by x, and y. Click the widget makes it disappear.
}
\value{
  This function does not return any value
}
\author{Jianhua Zhang}

\examples{
if(interactive()){
    getLightTW(200, 200, "Click Me!")
}
}
\keyword{interface}
