# This function reads a few lines from a given file and then guesses
# the separator used to separate columns.
#
# Copyright 2002, J. Zhang, all rights reserved
#

guess.sep <- function(file.name, n = 3, seps = ""){

    if(seps == ""){
        seps <- c(" ", ",", ";", "\t")
    }else{
        seps <- c(" ", ",", ";", "\t", seps)
    }

    toCheck <- readLines(file.name, n = n)

    for(i in seps){
        temp <- length(unlist(strsplit(toCheck[1], i)))
        if(temp > 1){
            for(j in 2:length(toCheck)){
                temp2 <- length(unlist(strsplit(toCheck[j], i)))
                if(temp != temp2){
                    break
                }
            }
            return(i)
        }
    }
    warning("No separator has been detected")
}
