# This function checks to see if a given string assumed to be a file name has
# a predefined prefix, suffix, or characters and returns TRUE if it
# does or FALSE otherwise.
#
# Copyright 2002, J. Zhang. All rights reserved.
#
##why did you get rid of hasSuffix and hasPrefix -- you should not be 
## changing the API -- you just broke everyone's code -- for no real
##reason -- you could have a hasPrefix and hasSuffix that simply call this
##function. You can't really go around changing the API unless there is a
##really good reason....

hasChar <- function (tocheck, what = ""){

    if(!is.character(tocheck) || nchar(tocheck)  < 1)
        stop(paste("Bad value:", tocheck))

    function(x){
        ##didn't you just check this?
        if(!is.character(x) || nchar(x) < 1 )
        stop("Bad string value!")

        if(what == "prefix"){
            pattern <- paste("^", tocheck, sep = "")
        }else if(what == "suffix"){
            pattern <- paste(tocheck, "$", sep = "")
        }else{
            pattern <- tocheck
        }

        if(regexpr(pattern, x) > 0 ){
           return(TRUE)
        }else{
            return(FALSE)
        }
    }
}


