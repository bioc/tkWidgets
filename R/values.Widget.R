# This function prints the values for all the value containing widgets
# elements on a widget created using the function widgetRender.
#
# Copyright J. Zhang 2002, all rights reserved.
#
values.Widget <- function (x){

    wList <- WwList(x)
    returnList <- vector("list", length = length(names(wList)))
    counter <- 1
    for(i in names(wList)) {
        pW <- WListByName(wList, i)
        returnList[[counter]] <- list(Entry = i, Value = WValue(pW))
        counter <- counter + 1
    }
    return(returnList)
}
