##FIXME: this description doesn't seem to make sense...
# This function prints the values for all the value containing widgets
# elements on a widget created using the function widgetRender.
#
# Copyright J. Zhang 2002, all rights reserved.
#
##FIXME: why is the argument named x? is it a widget? then please name
## if widget or some such thing, but x is completely incomprehensible

values.Widget <- function (x){
   ##FIXME: what does WwList do?
    wList <- WwList(x)
    returnList <- vector("list", length = length(names(wList)))
    counter <- 1
    for(i in names(wList)) {
        pW <- wList[[i]]
        returnList[[counter]] <- list(Entry = i, Value = WValue(pW))
        counter <- counter + 1
    }
    return(returnList)
}
##FIXME: why not like this?
##it is much more straight forward; I don't see why you want to 
##return a list of lists; just return a named list
getWvalues <- function (W){
    wList <- WwList(W)
    lenW <- length(wList)
    if( is.null(wList) ) return(NULL)  ##not sure here--what is the contract?
    rList <- vector("list", length = lenW)
    names(rList) <- names(wList)
    for(i in 1:lenW) 
        returnList[[i]] <- WValue(wList[[i]])
    return(returnList)
}
