# These functions provide standard outputs that are going to be
# rendered to a widget to indicate the type or value of the R object
# shown on the widget.
#
# Copyright 2002, J. Zhang. All rights reserved
#
##FIXME: please do not do this, a call is not a formula it is a call and
## a formula is a formula -- this is going to cause all sorts of trouble
## you can use inherits(x, "formula") to find out if something is a
## formula

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

