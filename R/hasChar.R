# This function checks to see if a given string assumed to be a file name has
# a predefined prefix, suffix, or characters and returns TRUE if it
# does or FALSE otherwise.
#
# Copyright 2002, J. Zhang. All rights reserved.
#
hasChar <- function (tocheck, what = ""){

    if(!is.character(tocheck) || nchar(tocheck)  < 1)
        stop(paste("Bad value for", tocheck))

    function(x){
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


