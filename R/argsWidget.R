# This function takes a argument list for an R function and creates a
# widget that allows users to manipulate the values using an interface.
#
# Copyright 2002, J. Zhang, all rights reserved
#

argsWidget <- function(argsList){
    # Sets the working environment
    PWEnv <- new.env(hash = TRUE, parent = NULL)
    # A list of pWidgets
    pWidgets <- list()
    # Figures out the width for lables
    lWidth <- max(nchar(names(argsList)))
    # Defines the widget components
    for(i in names(argsList)){
        tempList <- list()
        label <- label(name = "label", value = i, width = lWidth,
                                                           env = PWEnv)
        eval(substitute(j <- entryBox(name = j, value = argsList[j],
                                 width = 15, env = PWEnv), list(j = i)))
        tempList[["label"]] <- label
        tempList[["entry"]] <- get(i)
        pWidgets[[i]] <- tempList
    }
    # Constructs the interface
    widget <- widget(wTitle = "BioC Arguments Widget", pWidgets,
                     funs = list(), preFun = function() {},
                     postFun = function() {}, env = PWEnv)

    # Returns the input values
    for(i in names(argsList)){
        argsList[i] <- formatArg(value(pWidgets(widget)[[i]][["entry"]]))
    }

    return(argsList)
}

# This function fomats the arguments obtained from a widget
formatArg <- function(arg){
    switch(tolower(arg),
           "true" = return(TRUE),
           "false" = return(FALSE),
           "na" = return(NA),
           "null" = return(NULL))
    options(warn = -1)
    temp <- as.numeric(arg)
    options(warn = 0)
    if(is.na(temp)){
        return(arg)
    }else{
        return(temp)
    }
}
