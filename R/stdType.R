# These functions provide standard outputs that are going to be
# rendered to a widget to indicate the type or value of the R object
# shown on the widget.
#
# Copyright 2002, J. Zhang. All rights reserved
#
stdType <- function(toCheck){
    if(mode(toCheck) == "call")
        return("Formula")
    else
        return(mode(toCheck))
}

stdView <- function(toView){
    toView <- getContent(toView)
    objViewer(toView)
}

