# This function reads a few lines from a given file and then guesses
# if there is a header line, the separator used to separate columns,
# and the data type of each column.
#
# Copyright 2002, J. Zhang, all rights reserved
#

guess.sep <- function(file.name, n = 5, seps = ""){

    separator <- "Not detected"
    header <- "Not detected"

    if(seps == ""){
        seps <- c(" ", ",", ";", "\t")
    }else{
        seps <- c(" ", ",", ";", "\t", seps)
    }

    toCheck <- readLines(file.name, n = n)

    w<-NULL
    for(i in seps) { w[[i]] <- strsplit(x, i)}
    v <- lapply(w, function(x) sapply(x, length))
    good <- function(x) all(x==x[1]) && x[1] > 1
    found <- sapply(v, good)
    sep <- names(found[found])
    if(length(sep) == 1){
        separator <- sep
    }
#    for(i in seps){
#        temp <- length(unlist(strsplit(toCheck[1], i)))
#        if(temp > 1){
#            for(j in 2:length(toCheck)){
#                temp2 <- length(unlist(strsplit(toCheck[j], i)))
#                if(temp == temp2){
#                    separator <- i
#                    break
#                }
#            }
#        }
#    }

    if(separator != "Not detected"){
       header <- guess.header(toCheck[1:2], separator)
    }else{
       header <- guess.header(toCheck[1:2], NULL)
    }
    type <- find.type(toCheck[length(toCheck)], separator)

    return(list(header = header, separator = separator, type = type))
}

guess.header <- function(twoLines, sep){

    on.exit(options(warn = 1))

    if(!is.null(sep)){
        firstLine <- unlist(strsplit(twoLines[1], sep))
        scndLine <- unlist(strsplit(twoLines[2], sep))
    }else{
        firstLine <- twoLines[1]
        scndLine <- twoLines[2]
    }

    options(warn = -1)
    firstLine <- as.numeric(firstLine)
    scndLine <- as.numeric(scndLine)
    options(warn = 1)

    firstLine[!is.na(firstLine)] <- "num"
    scndLine[!is.na(scndLine)] <- "num"

    if(!setequal(firstLine, scndLine)){
        return(TRUE)
    }else{
        if(any(!is.na(firstLine))){
            return(FALSE)
        }
        return("Not detectable")
    }
}

find.type <- function(line, sep){

     line <- unlist(strsplit(line, sep))
     options(warn = -1)
     line <- as.numeric(line)
     options(warn = 1)

     line[is.na(line)] <- "character"
     line[!is.na(line) & line != "character"] <- "numeric"

     return(line)
}
