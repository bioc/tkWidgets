# This function takes a argument list returned by formals(R function)
# and creates a widget that allows users to manipulate the values
# using an interface.
#
# Copyright 2002, J. Zhang, all rights reserved
#

argsWidget <- function(argsList, defaultNames = c("OK", "Cancel"),
                       inst = ""){

    # Arguments that are functions
    funcs <- getSymbol(argsList)
    # Conver functions to characters
    argsList <- sapply(funcs2Char(argsList, funcs), formatArg)
    # Constructs the interface
    # Sets the working environment
    PWEnv <- new.env(hash = TRUE, parent = NULL)
    pWidgets <- getPWidget(argsList, PWEnv, inst)
    widget <- widget(wTitle = "BioC Arguments Widget", pWidgets,
                     funs = list(), preFun = function() {},
                     postFun = function() {}, env = PWEnv,
                     defaultNames = defaultNames)
    if(!is.null(widget)){
        # Returns the input values
        for(i in names(argsList)){
            if(any(i == names(funcs))){
                argsList[[i]] <-
                    get(wValue(pWidgets(widget)[[i]][["entry"]])[[1]])
            }else{
                argsList[[i]] <-
                    formatArg(wValue(pWidgets(widget)[[i]][["entry"]])[[1]])
            }
        }
        return(argsList)
    }else{
        return(widget)
    }
#    return(argsList)
}
# Creates the primary widget list for building the interface
getPWidget <- function(argsList, PWEnv, inst = ""){
    # Figures out the width for lables
    lWidth <- max(nchar(names(argsList)))
    pWidgets <- list()

    if(inst != ""){
        label <- label(wName = "label", wValue = inst,
                       wWidth = nchar(inst), wEnv = PWEnv)
        tempList <- list()
        tempList[["label"]] <- label
        pWidgets[["inst"]] <- tempList
    }

    for(i in names(argsList)){
        tempList <- list()
        # Creates radio buttons with TRUE and FALSE if the default
        # value for an argument is either a TRUE or FALSE
        if(is.logical(argsList[[i]]) && !is.na(argsList[[i]])){
            eval(substitute(j <- radioButton(wName = j,
                     wValue = c("TRUE" = TRUE,"FALSE" = FALSE),
                     wEnv = PWEnv), list(j = i)))
        }else{
            eval(substitute(j <- entryBox(wName = j,
                                 wValue = argsList[j],
                                 wWidth = 15, wEnv = PWEnv), list(j = i)))

        }
        label <- label(wName = "label", wValue = i,
                                             wWidth = lWidth, wEnv = PWEnv)
        tempList[["label"]] <- label
        tempList[["entry"]] <- get(i)
        pWidgets[[i]] <- tempList
    }
    return(pWidgets)
}

# This function fomats the arguments obtained from a widget
formatArg <- function(toFormat){

    # Turns off any warnings when checking for NULL, NA, and boolean
    options(warn = -1)
    if(is.null(toFormat)){
        options(warn = 0)
        return(toFormat)
    }
    if(is.na(toFormat)){
        options(warn = -1)
        return("NA")
    }
    if(is.logical(toFormat)){
        options(warn = 0)
        return(toFormat)
    }
    if(toFormat == "TRUE"){
        return(TRUE)
    }
    if(toFormat == "FALSE"){
        return(FALSE)
    }
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
        return(getTrueNullNa(toFormat))
    }else{
        return(temp)
    }
}

# All functions are of type "symbol", "builtin", in the list returned
# by formals
getSymbol <- function(args){
    temp <- sapply(args, typeof)
    temp <- args[names(
           temp[temp == "symbol" | temp == "closure" | temp == "builtin"])]
    temp <- temp[temp != ""]
    return(temp)
}

funcs2Char <- function(args,funcs){
    for(i in names(funcs)){
        args[[i]] <- as.character(funcs[i])
    }
    return(args)
}

getTrueNullNa <- function(toFormat){
     switch(tolower(toFormat),
                   "t" = ,
                   "true" = return(TRUE),
                   "f" = ,
                   "false" = return(FALSE),
                   "na" = return(NA),
                   "null" = return(NULL),
                   return(toFormat))
}

