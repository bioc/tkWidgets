# This function prints the values for the Name, Value, canEdit, and
# buttonText of all the value containing widget elements on the widget
# created by using the function widgetRender.
#
# Copyright J. Zhang 2002, all rights reserved.
#
print.Widget <- function (x, ...){

    wList <- WwList(x)
    for(i in names(wList)) {
        pW <- wList[[i]]
        print(paste("widget_", i, ":", sep = ""))
        class(pW) <- "pWidget"
        print.pWidget(pW)
    }
    # for now we just the other parameters as a vector. Will do more later.
    if(length(unlist(list(...))) > 0)
        unlist(list(...))
}
