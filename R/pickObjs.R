# This function takes a vector of strings for object names is an
# environment specified by the calling function and returns a vactor
# of strings for object names selected by the function passed.
#
# Copyright 2002, J. Zhang. All rights reserved.
#

pickObjs <- function (objNames, fun = noAuto){

    if(is.null(objNames) || is.na(objNames) ||
                      !is.character(objNames)){
        stop("Invalid objNames! Must be character")
    }

    whichOnes <- NULL

    for(i in objNames){
        whichOnes <- c(whichOnes, fun(i))
    }
    return (objNames[whichOnes])
}

noAuto <-  function(x) {
    if(regexpr("^Autoloads", x) > 0 ){
        return(FALSE)
    }else{
        return(TRUE)
    }
}
