# This function checks a vector of file names from a directory and
# determins what to be sent for desplay on the fileBrowser  based
# on the default and user speficied parameters.
#
# Copyright 2002, J. Zhang. All rights reserved.
#
pickFiles <- function (fileNames, fun = function(x) TRUE,
                       prefix = NULL, suffix = NULL,
                       exclude = .Platform$file.sep){
    if(is.null(prefix) && is.null(suffix)){
       whichOnes <- sapply(fileNames, fun)
    }else
       if(!is.null(prefix)){
           tryMe <- hasChar(prefix, "prefix")
           whichOnes <- sapply(fileNames, tryMe)
       }else{
           tryMe <- hasSuffix(suffix, "suffix")
           whichOnes <- sapply(fileNames, tryMe)
       }
   return(c(fileNames[regexpr(exclude, fileNames) > 0],
            fileNames[whichOnes]))
}


