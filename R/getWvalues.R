# This function prints the values for widgets on a widget created by
# function widgetRender. It takes a list defining the widget and
# returns a named list containing the values for each of the widgets.
#
# Copyright J. Zhang 2002, all rights reserved.
#

getWvalues <- function (W){
    wList <- WwList(W)
    lenW <- length(wList)
    if( is.null(wList) ) return(NULL)
    rList <- vector("list", length = lenW)
    names(rList) <- names(wList)
    for(i in 1:lenW)
        rList[[i]] <- WValue(wList[[i]])
    return(rList)
}
