# This function checks a vector of file names from a directory and
# determins what to be sent for desplay on the fileBrowser GUI based
# on the default and user speficied parameters.
#
# Copyright 2002, J. Zhang. All rights reserved.
#
pickFiles <- function (fileNames, fun = function(x) TRUE,
                       prefix = NULL, suffix = NULL,
                       exclude = .Platform$file.sep){
    if(is.null(prefix) && is.null(suffix))
       whichOnes <- sapply(fileNames, fun)
    else
       if(!is.null(prefix)){
           tryMe <- hasPrefix(prefix, exclude)
           whichOnes <- sapply(fileNames, tryMe)
       }else{
           tryMe <- hasSuffix(suffix, exclude)
           whichOnes <- sapply(fileNames, tryMe)
       }
   return(fileNames[whichOnes])
}


