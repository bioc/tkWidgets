# These functions provide standard outputs that are going to be
# rendered to a widget to indicate the type or value of the R object
# shown on the widget.
#
# Copyright 2002, J. Zhang. All rights reserved
#

##what is formular????
##is this really what you want?? shouldn't it always use a class if 
##there is one? 
stdType <- function(toCheck){
    if(inherits(toCheck, "formular")){
        return("formula")
    }else{
        return(mode(toCheck))
    }
}

stdView <- function(toView){
    if(inherits(toView, "formular")){
        toView <- format(toView)
    }
    objViewer(toView)
}

