# This function reads a few lines from a given file and then guesses
# if there is a header line, the separator used to separate columns,
# and the data type of each column.
#
# Copyright 2002, J. Zhang, all rights reserved
#

guess.sep <- function(file.name, n = 5, seps = ""){

    separator <- ""
    header <- FALSE

    if(seps == ""){
        seps <- c(" ", ",", ";", "\t")
    }else{
        seps <- c(" ", ",", ";", "\t", seps)
    }

    toCheck <- readLines(file.name, n = n)

    w<-NULL
    for(i in seps) { w[[i]] <- strsplit(toCheck[2:length(toCheck)], i)}
    v <- lapply(w, function(x) sapply(x, length))
    good <- function(x) all(x==x[1]) && x[1] > 1
    found <- sapply(v, good)
    sep <- names(found[found])
    if(length(sep) == 1){
        separator <- sep
    }

    if(length(unlist(strsplit(toCheck[1], separator)))
                                      == v[[separator]][1] - 1){
        header <- TRUE
    }else{
        header <- guess.header(toCheck[1:2], separator)
    }

    type <- find.type(toCheck[2:length(toCheck)],separator)
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
        return(FALSE)
    }
}

find.type <- function(line, sep){

     types <- NULL
     for(i in line){
         temp <- unlist(strsplit(i, sep))
         options(warn = -1)
         temp <- as.numeric(temp)
         options(warn = 1)

         temp[is.na(temp)] <- "character"
         temp[!is.na(temp) & temp != "character"] <- "numeric"
         types <- rbind(types, temp)
     }
     if(nrow(unique(types)) == 1){
         return(types[1,])
     }else{
         return("Not detected")
     }
}
