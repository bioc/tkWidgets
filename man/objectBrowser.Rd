\name{objectBrowser}
\alias{objectBrowser}

\title{A function to view the objects in the work space.}
\description{
  This function provides a widget to view objects and select one that is
  going to be returned. When the "End" button is pressed,the selected
  object will be returned. 
}
\usage{
objectBrowser(fun = function(x) TRUE, textToShow = "Select object(s)", nSelect = -1)
}

\arguments{
  \item{fun}{A function to test whether certain conditions are met by
    the objects. Only objects that meet the conditions will be
    displayed.}
  \item{textToShow}{A character string for the message to be show on the
    widget as an instruction.}
  \item{nSelect}{An integer for the number of object to select. No
    limitation if nSelect = -1}
}

\details{
  This function will return a list of lists with a "name" and "obj" pair
  for each object. The "name" will be the name of the object and "obj"
  will be the value of the object. If the object is a package, a
  description of the contents of the package will be the value. If the
  selected object is a function, a text string of the original code will
  be the value. A function can be passed to impose a filtering mechanisms
  on the objects to be displayed. See function isCharacter for an
  example of writing a filtering function for objectBrowser.

  The buttons and their expected behavior are listed below:

  Up - Moves one level up along the search list and displays the content
  in the box for object names on the left of the widget.

  Select >> - When objects in the box for object names have been
  highlighted by clicking or clicking/draging, clicking "Select >>" will
  display the highlighted object names in the box for selected objects on
  the right. 

  Reset - Moves back to ".GlobalEnv" which is the default starting point
  of the system.

  << Remove - When object names in the box for seleted objects have been
  highlighted by clicking or clicking/draging and the "<< Remove" button
  pressed, the highlighted object names will be removed from the display.

  Clear <- Removes all the object names from the box for selected
  objects.

  Cancel - Exits the widget and returns a NULL when pressed.

  End <- Returns a list of lists with names of the objects in the box
  for selected objects and their corresponding values or NULL if nothing
  exists in the box.
  
}
\value{
  \item{returnList}{A list of lists with a name and value pair for each
    object.} 
}
\references{Tcltk, R News Vol 1/3. A Primer on the R-Tck/Tk Package.}
\author{Jianhua (John) Zhang}
\note{This function is part of the Bioconductor project at Dana-Farber
  Cancer Institute to provide Bioinformatics functionalities through R.}

\seealso{\code{\link{writeObj}}, \code{\link{isCharacter}}}

\examples{
# The example is commented out as it requires user interference which
# may not be desirable for auto build.

# Call the function with the isCharacter function.
#objectBrowser(isCharacter)
  
}
\keyword{interface}

