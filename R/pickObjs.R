##FIXME: this description doesn't make sense...
# This function checks a vector of object names for a given
# environment and determins what to be sent for display on the
# objectBrowser GUI based on the default and user specified parameters.
#
# Copyright 2002, J. Zhang. All rights reserved.
#
#FIXME: why not make objNames be a character vector on input?
#FIXME: what is being listed? the try(get(as.character(i)))
#   doesn't look right? 
#why are we mixing packages and environments in here (regardless
# of the user function)?????
#if you want to list the packages and environments then you
#should obey the same rules as everyone else: call this with a
#function that picks those out
pickObjs <- function (objNames, fun = function(x) TRUE){
    whichOnes <- NULL
##FIXME: why is this on.exit here?
    on.exit(options(show.error.messages = TRUE))

    for(i in objNames){
        if(regexpr("package:", as.character(i)) > 0)
            whichOnes <- c(whichOnes, TRUE)
        else{
            if(regexpr("^Autoloads", as.character(i)) > 0)
                whichOnes <- c(whichOnes, FALSE)
            else{
                options(show.error.messages = FALSE)
                tryMe <- try(get(as.character(i)))
                options(show.error.messages = TRUE)

                if(is.environment(tryMe))
                    whichOnes <- c(whichOnes, TRUE)
                else
                    whichOnes <- c(whichOnes, fun(as.character(i)))
            }
        }
    }
    return (objNames[whichOnes])
}


