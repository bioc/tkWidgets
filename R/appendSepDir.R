# This function takes a path and returns a vector containing names of
# files. A platform file separator will be appended to the end of
# subdirectory names.
#

appendSepDir <- function(path = ".") {
     toCheck <- list.files(path)

     if(length(toCheck) == 0 ) return("")

     isd <- file.info(toCheck)
     toCheck <- paste(toCheck, ifelse(isd$isdir, .Platform$file.sep, ""),
                   sep="")
     toCheck
}
















