# This function reads a few lines from a given file and then guesses
# if there is a header line, the separator used to separate columns,
# and the data type of each column.
#
# Copyright 2002, J. Zhang, all rights reserved
#

guess.sep <- function(file.name, numLine = 5, seps, isFile = TRUE){

    separator <- ""
    header <- FALSE
    sep <- NULL

    if(missing(seps)){
        seps <- c(",", ";", "\t", "\\t", " ")
    }#else{
    #    seps <- c(seps, ",", ";", "\t", " ")
    #}

    if(isFile){
        conn <- safeFileOpen(file.name)
        if(inherits(conn, "connection")){
            toCheck <- readLines(conn, n = numLine)
            close(conn)
        }else{
            stop(paste("Can't read file because", conn))
        }
    }else{
        toCheck <- file.name
    }

    good <- function(x) all(x==x[1]) && x[1] > 1

    for(i in seps){
        w <- strsplit(toCheck[2:length(toCheck)], i)
        v <- sapply(w, length)
        if(good(v)){
            sep <- i
            break
        }
    }
    if(!is.null(sep)){
        separator <- sep
        if(length(unlist(strsplit(toCheck[1], separator)))
                    == length(unlist(strsplit(toCheck[2], separator))) - 1){
            header <- TRUE
            colNames <- gsub("\"", "",
                              unlist(strsplit(toCheck[1], separator)))
            skip <- 0
            rowNames <- getRowNames(file.name, separator, header, skip)
        }else{
            headerNSkip <- guess.header(toCheck[1:2], separator)
            header <- headerNSkip[["header"]]
            skip <- headerNSkip[["skip"]]
            colNames <- headerNSkip[["colNames"]]
            rowNames <- getRowNames(file.name, separator, header, skip)
        }
        type <- find.type(file.name, separator, header, numLine)
        return(list(header = header, separator = separator, skip = skip,
                    col.names = colNames, row.names = rowNames, type = type))
    }else{
        # New line is always the separator
        return(list(header = FALSE, separator = "\n", col.names = NA,
                    row.names = NA, skip = 0,
                    type = find.type(file.name, sep = "\n", header = FALSE)))
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
        return(list(header = TRUE, skip = 1,
            colNames = gsub("\"", "",  unlist(strsplit(twoLines[1], sep)))))
    }else{
        #if(any(!is.na(firstLine))){
        #    return(list(header = FALSE, skip = 0))
        #}
        return(list(header = FALSE, skip = 0, colNames = NA))
    }
}

find.type <- function(file.name, sep, header = FALSE, numLine = 5,
                      isFile = TRUE){
    if(isFile){
        line <- as.matrix(read.table(file.name, sep = sep, header = header,
                                     nrows = numLine, as.is = TRUE))
    }else{
        line <- as.matrix(file.name)
    }

    types <- NULL
    for(i in 1:nrow(line)){
        types <- rbind(types, charOrNum(line[i,]))
    }
    if(nrow(unique(types)) == 1){
        return(types[1,])
    }else{
        return(rep("Character", ncol(types)))
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

getRowNames <- function(file.name, sep, header, skip){
    data <- read.table(file.name, sep = sep, header = header, skip = skip)
    return(rownames(data))
}

