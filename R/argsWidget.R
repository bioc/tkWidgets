# This function takes a argument list returned by formals(R function)
# and creates a widget that allows users to manipulate the values
# using an interface.
#
# Copyright 2002, J. Zhang, all rights reserved
#

argsWidget <- function(argsList){

    # Arguments that are functions
    funcs <- getSymbol(argsList)
    # Conver functions to characters
    argsList <- funcs2Char(argsList, funcs)
    # Constructs the interface
    # Sets the working environment
    PWEnv <- new.env(hash = TRUE, parent = NULL)
    pWidgets <- getPWidget(argsList, PWEnv)
    widget <- widget(wTitle = "BioC Arguments Widget", pWidgets,
                     funs = list(), preFun = function() {},
                     postFun = function() {}, env = PWEnv)

    # Returns the input values
    for(i in names(argsList)){
        if(any(i == names(funcs))){
            argsList[[i]] <- get(value(pWidgets(widget)[[i]][["entry"]])[[1]])
        }else{
            argsList[[i]] <-
                formatArg(value(pWidgets(widget)[[i]][["entry"]])[[1]])
        }
    }

    return(argsList)
}
# Creates the primary widget list for building the interface
getPWidget <- function(argsList, PWEnv){
    # Figures out the width for lables
    lWidth <- max(nchar(names(argsList)))
    pWidgets <- list()
    for(i in names(argsList)){
        tempList <- list()
        # Creates radio buttons with TRUE and FALSE if the default
        # value for an argument is either a TRUE or FALSE
        if(is.logical(argsList[[i]]) && !is.na(argsList[[i]])){
            eval(substitute(j <- radioButton(name = j,
                     value = c("TRUE" = TRUE,"FALSE" = FALSE),
                     env = PWEnv), list(j = i)))
        }else{
            eval(substitute(j <- entryBox(name = j,
                                 value = argsList[j],
                                 width = 15, env = PWEnv), list(j = i)))

        }
        label <- label(name = "label", value = i,
                                             width = lWidth, env = PWEnv)
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
           return(getTrueNullNa(toFormat))
        }else{
            return(temp)
        }
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

