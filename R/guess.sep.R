# This function reads a few lines from a given file and then guesses
# if there is a header line, the separator used to separate columns,
# and the data type of each column.
#
# Copyright 2002, J. Zhang, all rights reserved
#

guess.sep <- function(file.name, numLine = 5, seps = "", isFile = TRUE){

    separator <- ""
    header <- FALSE

    if(seps == ""){
        seps <- c(" ", ",", ";", "\t")
    }else{
        seps <- c(" ", ",", ";", "\t", seps)
    }

    if(isFile){
        toCheck <- readLines(file.name, n = numLine)
        for(i in seps[seps != " "]){
            toCheck <- gsub(paste(" *", i, "| *", i, " *|", i, " *",
                                  sep = ""), gsub(" *", "", i), toCheck)
        }
    }else{
        toCheck <- file.name
    }

    w<-NULL

    for(i in seps)
         w[[i]] <- strsplit(toCheck[2:length(toCheck)], i)

    v <- lapply(w, function(x) sapply(x, length))

    good <- function(x) all(x==x[1]) && x[1] > 1
    found <- sapply(v, good)

    sep <- names(found[found])
    if(length(sep) == 1){
        separator <- sep
        if(length(unlist(strsplit(toCheck[1], separator)))
                                              == v[[separator]][1] - 1){
            header <- TRUE
        }else{
            header <- guess.header(toCheck[1:2], separator)
        }

        type <- find.type(file.name, separator, header, numLine)
        return(list(header = header, separator = separator, type = type))
    }else{
        # New line is always the separator
        return(list(header = FALSE, separator = "\n",
                    type = find.type(toCheck[2:length(toCheck)],
                    header = FALSE)))
    }
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

find.type <- function(file.name, sep, header = FALSE, numLine = 5){

    line <- read.table(file.name, sep = sep, header = header, nrows = numLine)

    types <- NULL
    for(i in 1:nrow(line)){
        types <- rbind(types, charOrNum(line[i,]))
    }
    if(nrow(unique(types)) == 1){
        return(types[1,])
    }else{
        return("Not detected")
    }
}

charOrNum <- function(vect){
    options(warn = -1)
    temp <- as.numeric(vect)
    options(warn = 1)

    temp[is.na(temp)] <- "Character"
    temp[!is.na(temp) & temp != "Character"] <- "Numeric"
    return(temp)
}
