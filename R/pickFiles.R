# This function checks a vector of file names from a directory and
# determins what to be sent for desplay on the fileBrowser  based
# on the default and user speficied parameters.
#
# Copyright 2002, J. Zhang. All rights reserved.
#
pickFiles <- function (fileNames, fun = function(x) TRUE,
                       prefix = NULL, suffix = NULL,
                       exclude = .Platform$file.sep){

    unTouched <- fileNames[regexpr(exclude, fileNames) > 0]
    rest <- setdiff(fileNames, unTouched)
    if(length(rest) > 0){
        rest <- rest[sapply(rest, fun)]

        if(!is.null(prefix)){
            tryMe <- hasChar(prefix, "prefix")
            rest <- rest[sapply(rest, tryMe)]
        }
        if(!is.null(suffix)){
            tryMe <- hasChar(suffix, "suffix")
            rest <- sapply(rest, tryMe)
        }
        return(c(unTouched, rest))
    }else{
        return(unTouched)
    }
}


