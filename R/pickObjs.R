# This function checks a vector of object names for a given
# environment and determins what to be sent for desplay on the
# objectBrowser GUI based on the default and user speficied parameters.
#
# Copyright 2002, J. Zhang. All rights reserved.
#
pickObjs <- function (objNames, fun = function(x) TRUE){
    whichOnes <- NULL
    on.exit(options(show.error.messages = TRUE))

    for(i in objNames){
        if(regexpr("package:", i) > 0)
            whichOnes <- c(whichOnes, TRUE)
        else{
            if(regexpr("^Autoloads", i) > 0)
                whichOnes <- c(whichOnes, FALSE)
            else{
                options(show.error.messages = FALSE)
                tryMe <- try(get(i))
                options(show.error.messages = TRUE)

                if(is.environment(tryMe))
                    whichOnes <- c(whichOnes, TRUE)
                else
                    whichOnes <- c(whichOnes, fun(i))
            }
        }
    }
    return (objNames[whichOnes])
}


