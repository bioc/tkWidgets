# This function returns TRUE if a given prefix, suffix, or characters
# matches a character string or FALSE otherwise.
#
# Copyright 2002, J. Zhang. All rights reserved.
#

hasPrefix <- function(aPrefix){
    got <- hasChar(aPrefix, "prefix")
}

hasSuffix <- function(aSuffix){
    got <- hasChar(aSuffix, "suffix")
}

hasChar <- function (toCheck, what = ""){

    if(!is.character(toCheck) || nchar(toCheck)  < 1)
        stop(paste("Bad value:", toCheck))

    function(x){
        if(what == "prefix"){
            pattern <- paste("^", toCheck, sep = "")
        }else if(what == "suffix"){
            pattern <- paste(toCheck, "$", sep = "")
        }else{
            pattern <- toCheck
        }

        if(regexpr(pattern, x) > 0 ){
           return(TRUE)
        }else{
            return(FALSE)
        }
    }
}


