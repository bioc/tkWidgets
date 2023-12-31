# This function takes a path and returns a vector containing names of
# files. A platform file separator will be appended to the end of
# subdirectory names.
#

appendSepDir <- function(path = ".") {

     toCheck <- list.files(path)

     if (length(toCheck) == 0) return("")

     isd <- file.info(file.path(path, toCheck))
     if(length(toCheck[isd$isdir]) != 0){
         # Separate dirs and files into two groups for better presentation
         dirs <- paste(toCheck[isd$isdir], .Platform$file.sep, sep = "")
     }else{
         dirs <- NULL
     }
     return(c(dirs, toCheck[!isd$isdir]))
}


















