# This function gets a list of files from a folder passed and applies
# a given function to each of them. If the function can be applied to
# the file, the file is kept or left out otherwise. A vector
# containing all the kept files will be returned.
#
# Copyright 2002, J. Zhang. All rights reserved.
#
pickFiles <- function (funct,  path = ""){
    on.exit(options(show.error.messages = TRUE))

    filesPicked <- NULL
    if(path == "")
        path <- getwd()

    filesGot <- list.files(path)

    for(i in filesGot){
        readLines(paste(path, Platform()$file.sep, i, sep = ""))

        options(show.error.messages = FALSE)
        checkMe <- try(funct(i))
        options(show.error.messages = TRUE)

        if(!inherits(checkMe, "try-error"))
            filesPicked <- c(filesPicked,
                             paste(path, Platform()$file.sep, i, sep = ""))
    }

    return(filesPicked)
}


