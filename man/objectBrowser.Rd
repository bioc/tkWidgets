\name{objectBrowser}
\alias{objectBrowser}

\title{View the Objects in the Workspace}
\description{
  This widget allows uers to view and select objects from the
  workspace. When the \code{End} button is pressed, the selected objects
  will be returned as a list. 
}
\usage{
objectBrowser(env = .GlobalEnv,fun = noAuto, textToShow = "Select object(s)", nSelect = -1)  
}
\arguments{
  \item{fun}{function to test whether certain conditions are met by
    the objects.  Only objects that meet the conditions will be displayed.}
  \item{textToShow}{character with the message to be shown on the
    widget as an instruction.}
  \item{nSelect}{integer indicating the number of objects to select.
    No limitation if \code{nSelect = -1} as per default.}
  \item{env}{\code{env} a default environment object to start object Browser}
}
\details{
  This function will return a list of lists with a "name" and "obj" pair
  for each object selected. The "name" will be the name of the object
  and "obj" will be the value of the object. If the object is a package, a
  description of the contents of the package will be the value. If the
  selected object is a function, a text string of the original code will
  be the value. A function can be passed to impose a filtering mechanisms
  on the objects to be displayed. See function isCharacter for an
  example of writing a filtering function for objectBrowser.

  The buttons and their expected behavior are
  \describe{
    \item{\code{Up}}{Moves one level up along the search list and
      displays the content in the box for object names on the left of the
      widget.}

    \item{\code{Select \>\>}}{When objects in the box for object names
      have been highlighted by clicking or clicking/dragging, this
      button will display the highlighted object names in the box
      for selected objects on the right.}

    \item{\code{Reset}}{Moves back to \code{\link{.GlobalEnv}} which
      is the default starting point of the system.}

    \item{\code{\<\< Remove}}{When object names in the box for selected
      objects have been highlighted by clicking or clicking/dragging and
      this button is pressed, the highlighted object names will be removed
      from the display.}

    \item{\code{Clear}}{Removes all the object names from the box for
      selected objects.}

    \item{\code{Cancel}}{Exits the widget and returns \code{NULL} when
      pressed.}

    \item{\code{End}}{Returns a \code{list} of lists with names of the
      objects in the box for selected objects and their corresponding values
      or \code{NULL} if nothing exists in the box.}
  }
}
\value{
  A \code{\link{list}} of lists with a name and value pair for each object.
}
\author{Jianhua (John) Zhang}
\note{This function is part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through \R.}

\examples{
## The example here is only run interactively since it requires user
## interference which may cause problems if not available:
if(interactive()) {

# Call the function with the isCharacter function.
r <- objectBrowser()
str(r) # show what we've got
}
}
\keyword{interface}

