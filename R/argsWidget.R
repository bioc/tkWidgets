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
        # Creates radio buttons with TRUE and FALSE if the default
        # value for an argument is either a TRUE or FALSE
        if(is.logical(argsList[[i]]) && !is.na(argsList[[i]])){
            eval(substitute(j <- radioButton(name = j,
                     value = c("TRUE" = TRUE,"FALSE" = FALSE),
                     env = PWEnv), list(j = i)))
        }else{
            if(is.primitive(argsList[i])){
                eval(substitute(j <- entryBox(name = j,
                                 value = deparse(substitute(argsList[i])),
                                 width = 15, env = PWEnv), list(j = i)))
            }
            eval(substitute(j <- entryBox(name = j, value = argsList[i],
                                 width = 15, env = PWEnv), list(j = i)))
        }
        label <- label(name = "label", value = i,
                                             width = lWidth, env = PWEnv)
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
        argsList[i] <-
                 formatArg(value(pWidgets(widget)[[i]][["entry"]])[[1]])
    }

    return(argsList)
}

# This function fomats the arguments obtained from a widget
formatArg <- function(toFormat){
    # Turns off any warnings when checking for NULL, NA, and boolean
    options(warn = -1)
    if(any(is.null(toFormat), is.na(toFormat), is.logical(toFormat))){
        options(warn = 0)
        return(toFormat)
    }else{
        options(warn = 0)
        if(toFormat == ""){
            return(toFormat)
        }
        # expression and negative numbers can be "language"
        if(is.language(toFormat)){
            return(formula(toFormat))
        }
        options(warn = -1)
        temp <- as.numeric(toFormat)
        options(warn = 0)
        if(is.na(temp)){
            switch(tolower(toFormat),
                   "t" = ,
                   "true" = return(TRUE),
                   "f" = ,
                   "false" = return(FALSE),
                   "na" = return(NA),
                   "null" = return(NULL),
                   return(toFormat))
        }else{
            return(temp)
        }
    }
}
