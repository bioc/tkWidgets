# This function provides data import interfaces by mimicing MS Excel's
# Text Wizard.
#
# Copyright 2002, J. Zhang, all rights reserved.

importWizard <- function(filename, maxRow = 200){

    # Creates an environment to make some variables available to all
    # related functions
    workEnv <- new.env(hash = TRUE, parent = NULL)
    # A string to keep track of the current state
    assignCState("state1", env = workEnv)
    # Number of row to be displayed
    assignShowNum(maxRow, env = workEnv)
    # Do this if a file name is given
    if(!missing(filename)){
        setArgsList(filename, workEnv)
    }else{
        assignArgs(list(), workEnv)
        setColInfos(env = workEnv)
    }

    initImportWizard(workEnv)
}

setArgsList <- function(filename, env){
    argsList <- list()

    fileInfo <- guess.sep(file.name = filename, n = 40)
    if(!is.null(fileInfo)){
        temp <- list()
        temp[["file"]] <- filename
        temp[["header"]] <- fileInfo[["header"]]
        temp[["sep"]] <- fileInfo[["separator"]]
        argsList[["state1"]] <- temp
        assignArgs(argsList, env)
        setColInfos(fileInfo[["type"]], env)
    }
}

assignArgs <- function(value, env){
    assign("argsList", value, env = env)
}
getArgs <- function(env){
    get("argsList", env = env)
}

assignType <- function(value, env){
    assign("columnType", value, env)
}
getType <- function(env){
    get("columnType", env)
}

assignShowNum <- function(value, env){
    assign("showNum", value, env)
}
getShowNum <- function(env){
    get("showNum", env)
}
# Keeps track with state is at
assignCState <- function(value, env){
    assign("currentState", value, env)
}
getCState <- function(env){
    get("currentState", env)
}
# A ID to keep the tkwin id for a state frame
assignSID <- function(value, env){
    assign("stateID", value, env)
}
getSID <- function(env){
    get("stateID", env)
}
# A list of colInfo objects that keep column name, type, and drop info
assignColInfo <- function(value, env){
    assign("colInfos", value, env)
}
getColInfo <- function(env){
    get("colInfos", env)
}
# Creates colInfo objects and sets the value of 'colInfos' list
setColInfos <- function(types, env){
    if(missing(types)){
        assignColInfo(list(), env)
    }else{
        temp <- list()
        for(i in 1:length(types)){
            temp[[i]] <- colInfo("", types[i], FALSE)
        }
        assignColInfo(temp, env)
    }
}
# This function initializes the interface for importWizard by creating
# a widget with an empty top canvas and bottom frame filled with four buttons
initImportWizard <- function(env){
    # A list to be returned that contains an argument list and data
    # imported using read.table
    dataList <- NULL
    on.exit(end())
    # Destroy the window
    end <- function(){
         tkdestroy(top)
    }
    nextState <- function(){
        changeState(canvas, env, TRUE)
    }
    preState <- function(){
        changeState(canvas, env, FALSE)
    }
    finishClicked <- function(){
        dataList <<- finish(env)
        end()
    }

    ## Set up the interface
    top <- tktoplevel()
    tktitle(top) <- "BioC Data Import Wizard"
    # Set the empty canvas that will be filled later
    canvas <- getTopCan(top, env)
    stateID <- tkcreate(canvas, "window", 0, 0, anchor = "nw",
                                    window = getState1Frame(canvas, env))
    assignSID(stateID, env)

    ## The bottom frame contains the buttons that allow user to
    ## navigate the importingprocess
    butFrame <- tkframe(top)
    canBut <- tkbutton(butFrame, text = "Cancel", width = 8,
                       command = end)
    backBut <- tkbutton(butFrame, text = "< Back", width = 8,
                        state = "disabled", command = preState)
    nextBut <- tkbutton(butFrame, text = "Next >", width = 8,
                        command = nextState)
    endBut <- tkbutton(butFrame, text = "Finish", width = 8,
                       command = finishClicked)
    tkpack(canBut, backBut, nextBut, endBut, side = "left")
    tkpack(butFrame, pady = 10)

    tkwait.window(top)
    return(dataList)
}

getTopCan <- function(base, env){
    WIDTH <- 730
    HEIGHT <- 400
    ## The canvas has widgets for the relevant
    ## arguments and preview of the original data. The content various
    ## depending on the state
    canvas <- makeViewer(base, vWidth = WIDTH, vHeight = HEIGHT,
                           vScroll = FALSE, hScroll = FALSE,
                           what = "canvas", side = "top")
    return(canvas)
}

changeState <- function(canvas, env, forward = TRUE){
    setNewState(env, forward)
    if(forward){
        addArgs(env)
    }else{
        dropArgs(env)
    }
    tkdelete(canvas, getSID(env))
    stateID <- tkcreate(canvas, "window", 0, 0, anchor = "nw",
                                    window = getAFrame(canvas, env))
    assignSID(stateID, env)
}
# Sets the strig for the new state (next or previous)
setNewState <- function(env, forward = TRUE){
    if(forward){
        switch(getCState(env),
               "state1" = assignCState("state2", env),
               "state2" = assignCState("state3", env))
    }else{
        switch(getCState(env),
               "state2" = assignCState("state1", env),
               "state3" = assignCState("state2", env))
    }
}
# Add a new state arguments list to argsList
addArgs <- function(env){
    temp <- getArgs(env)
    if(length(temp) == 1){
        temp[["state2"]] <- temp[[length(temp)]]
        assignArgs(temp, env)
    }else{
        temp[["state3"]] <- temp[[length(temp)]]
        assignArgs(temp, env)
    }
}
# Drop a state arguments list from argsList
dropArgs <- function(env){
    temp <- getArgs(env)
    if(length(temp) > 1){
        temp <- temp[-length(temp)]
        assignArgs(temp, env)
    }
}
getAFrame <- function(base, env){
    switch(getCState(env),
           "state1" = return(getState1Frame(base, env)),
           "state2" = return(getState2Frame(base, env)),
           "state3" = return(getState3Frame(base, env)))
}

finish <- function(env){
    switch(getCState(env),
           "state1" = args <- getArgs(env)[["state1"]],
           "state2" = args <- getArgs(env)[["state2"]],
           "state3" = args <- getArgs(env)[["state3"]])
    print(args)
    dataFile <- do.call("read.table", args)
    colInfos <- getColInfo(env)
    colNames <- NULL
    for(i in 1:length(colInfos)) {
        if(drop(colInfos[[i]])){
            dataFile <- dataFile[, -i]
        }
        switch(type(colInfos[[i]]),
               "Character" = dataFile[, i] <- as.character(dataFile[, i]),
               "Numeric" = dataFile[, i] <- as.numeric(dataFile[, i]))
        colNames <- c(colNames, name(colInfos[[i]]))
    }
    names(dataFile) <- colNames
    return(list(args = args, data = dataFile))
}
# Gets the frame containing the interface for the top frame of
# importWizard for state1
getState1Frame <- function(base, env){
    # A frame containing the interface that will be returned
    frame <- tkframe(base)
    # The bottom frame contains a list box showing the data. The
    # bottom frame is set first to make the list box available for
    # updating by the top frame
    bottomFrame <- tkframe(frame)
    dataViewer <- setState1BFrame(bottomFrame, env)
    # The mid frame contains the delimiter and number line
    # information.
    midFrame <- tkframe(frame)
    delims <- setState1MFrame(midFrame, env)
    # The top frame contains a entry box and a browse button that
    # allows for browing directories for a file name
    topFrame <- tkframe(frame)
    setState1TFrame(topFrame, dataViewer, delims, env)
    tkpack(topFrame, pady = 5)
    tkpack(midFrame)
    # Pack the bottom frame last
    tkpack(bottomFrame, pady = 5)
    return(frame)
}

setState1BFrame <- function(frame, env){
    # A list box to show the original data
    viewFrame <- tkframe(frame)
    dataViewer <- makeViewer(viewFrame, vWidth = 99, vHeight = 17,
                            vScroll = TRUE, hScroll = TRUE,
                            what = "list", side = "top")
    tkpack(viewFrame, anchor = "w", pady = 10)
    return(dataViewer)
}

setState1TFrame <- function(frame, viewer, delims, env){
    # Populate the entry box for file name when the brose button is
    # clicked
    browse <- function(){
        filename <- fileBrowser(nSelect = 1)
        writeList(nameEntry, filename, clear = TRUE)
        setArgsList(filename, env)
        showData4State1(viewer, env)
        if(length(getArgs(env)[["state1"]][["sep"]]) != 0){
            tkselect(delims[["delimit"]])
        }
    }
    if(!is.null(getArgs(env)[["state1"]][["file"]])){
        showData4State1(viewer, env)
        if(length(getArgs(env)[["state1"]][["sep"]]) != 0){
            tkselect(delims[["delimit"]])
        }
    }
    # Frame to hole the widgets
    nameFrame <- tkframe(frame)
    label1 <- tklabel(nameFrame, text = "File name: ")
    tkpack(label1, side = "left")
    # An entry box to hold the result of fileBrowser
    nameEntry <- tkentry(nameFrame, width = 77)
    tkpack(nameEntry, side = "left")
    # A button to envoke fileBrowser
    browseBut <- tkbutton(nameFrame, width = 8, text = "Browse",
                          command = browse)
    tkpack(browseBut)
    tkpack(nameFrame)
}

setState1MFrame <- function(frame, env){
    # Executed when values in start at row list box is clicked
    startClicked <- function(){
        setSkip(startList, env)
    }
    leftPan <- tkframe(frame)
    delimit <- tclVar()
    delimitRadio <- tkradiobutton(leftPan, text = paste("Delimited",
                                  " - Files are separated by a character",
                                  " such as a comma or tab", sep =""),
                                  value = "delim", variable = delimit,
                                  width = 71, anchor = "nw")
    tkpack(delimitRadio, anchor = "w")
    fixedRadio <- tkradiobutton(leftPan, text = paste("Fixed",
                                " width - Fields are aligned in columns",
                                " with spaces between fields", sep = ""),
                                value = "fixed", variable = delimit,
                                width = 71, anchor = "nw")
    tkpack(fixedRadio, anchor = "w")
    tkpack(leftPan, side = "left", anchor = "w")
    rightPan <- tkframe(frame)
    paraLabel2 <- tklabel(rightPan, text = "Start import at row:")
    tkpack(paraLabel2, side = "left", anchor = "ne")
    startFrame <- tkframe(rightPan)
    startList <- makeViewer(startFrame, vWidth = 2, vHeight = 1,
                            what  = "list", side = "top")
    tkconfigure(startList, selectmode = "single")
    tkbind(startList, "<B1-ButtonRelease>", startClicked)
    writeList(startList, 1:99, clear = TRUE)
    tkpack(startFrame, anchor = "w", side = "left")
    tkpack(rightPan, side = "left", padx = 7)
    return(list(delimit = delimitRadio, fixed = fixedRadio))
}

# Sets the value for skip when user selects line to start in
# state1
setSkip <- function(widget, env, state = "state1"){
    temp <- getArgs(env)
    temp[[state]][["skip"]] <-
                    as.numeric(tkget(widget, tkcurselection(widget))) - 1
    assignArgs(temp, env)
}

getState2Frame <- function(base, env, state = "state2"){
    temp <- getArgs(env)
    temp[[state]] <- temp
    frame <- tkframe(base)
    label1 <- tklabel(frame, text = paste("File:",
                                 getArgs(env)[[state]][["file"]]))
    tkpack(label1, pady = 5, padx = 5)
    midFrame <- tkframe(frame)
    setState2MFrame(midFrame, env)
    tkpack(midFrame, pady = 5, padx = 5)
    bottomFrame <- tkframe(frame)
    setState2BFrame(bottomFrame, env)
    tkpack(bottomFrame)
    return(frame)
}
# Sets the state2 mid frame
setState2MFrame <- function(frame,env){
    leftFrame <- tkframe(frame)
    setSepRadios(leftFrame, env)
    rightFrame <- tkframe(frame)
    setQuoteList(rightFrame, env)
    tkpack(leftFrame, side = "left", anchor = "w")
    tkpack(rightFrame, side = "left")
}
# Sets the radio buttons for separators for state2 mid frame
setSepRadios <- function(frame, env, state = "state2"){
    labelFrame <- tkframe(frame)
    label <- tklabel(labelFrame, text = "File Delimiter:")
    tkpack(label, side = "left", anchor = "nw")
    tkpack(labelFrame, side = "left")
    sepFrame <- tkframe(frame)
    sepVar <- tclVar()
    sepButs <- list()
    sepButs[["tab"]] <- tkradiobutton(sepFrame, text = "Tab",
                              variable = sepVar, width = 9,
                              value = "\t", anchor = "nw")
    sepButs[["semi"]] <- tkradiobutton(sepFrame, text = "Semicolon",
                               variable = sepVar, width = 9,
                               value = ";", anchor = "nw")
    sepButs[["comma"]] <- tkradiobutton(sepFrame, text = "Comma",
                              variable = sepVar, value = ",",
                              width = 9, anchor = "nw")
    tkgrid(sepButs[["tab"]], sepButs[["semi"]], sepButs[["comma"]])
    sepButs[["space"]] <- tkradiobutton(sepFrame, text = "Space",
                              variable = sepVar, value = "\"\"",
                              width = 9, anchor = "nw")
    sepButs[["other"]] <- tkradiobutton(sepFrame, text = "Other:",
                              variable = sepVar, value = "other",
                              width = 9, anchor = "nw")
    otherEntry <- tkentry(sepFrame, width = 11)
    tkgrid(sepButs[["space"]], sepButs[["other"]], otherEntry)
    tkpack(sepFrame, side = "left", anchor = "ne")
    tkselect(sepButs[[whatDeli(getArgs(env)[[state]][["sep"]])]])
}
# Sets the list box for quotes for state2 mid frame
setQuoteList <- function(frame, env){
    quoteSelected <- function(){
        setQuote(quoteList, env)
    }
    label1 <- tklabel(frame, text = "     Quote:")
    tkpack(label1, side = "left", anchor = "ne")
    quoteFrame <- tkframe(frame)
    quoteList <- makeViewer(quoteFrame, vWidth = 8, vHeight = 1,
                            what  = "list", side = "top")
    tkconfigure(quoteList, selectmode = "extended")
    tkbind(quoteList, "<B1-ButtonRelease>", setQuote)
    writeList(quoteList, c("\"", "'"), clear = TRUE)
    tkpack(quoteFrame, anchor = "w")
}

# Sets the value for quote when user selects quote in state2
setQuote <- function(listBox, env, state = "state2"){
    quotes <- ""
    selIndex <- unlist(strsplit(tkcurselection(listBox), " "))
    for(i in selIndex){
        quotes <- paste(quotes, tkget(listBox, i), sep = "")
    }
    temp <- getArgs(env)
    temp[[state]][["quote"]] <- quote
    assignArgs(temp, env)
}
# Sets the canvas holding the preview of data for state2
setState2BFrame <- function(frame, env){
    viewFrame <- tkframe(frame)
    dataView2 <- makeViewer(viewFrame, vWidth = 700, vHeight = 280,
                           vScroll = TRUE, hScroll = TRUE,
                           what = "canvas", side = "top")
    tkpack(viewFrame, anchor = "w", padx = 5, pady = 5)
    showData4State2(dataView2, env)

}
# Populates the data preview list of state2
showData4State2 <- function(canvas, env, state = "state2"){
    # Only show the number of rows defined
    temp <- getArgs(env)[[state]]
    temp[["nrow"]] <- getShowNum(env)
    dataFile <- as.matrix(do.call("read.table", temp))
    tempFrame <- tkframe(canvas)
    for(i in 1:ncol(dataFile)){
        tempList <- tklistbox(tempFrame, width = 0, height = 0,
                              background = "white")
        tkinsert(tempList, "end", dataFile[,i])
        tkpack(tempList, side = "left")
    }
    tkcreate(canvas, "window", 0, 0, anchor = "nw", window = tempFrame)
}
# Gets the frame containing the interface for the top frame of
# importWizard for state3
getState3Frame <- function(base, env){
    # A frame containing the interface that will be returned
    frame <- tkframe(base)
    label1 <- tklabel(frame, text = paste("File:",
                             getArgs(env)[["state3"]][["file"]]))
    tkpack(label1, pady = 5)
    topFrame <- tkframe(frame)
    setState3TFrame(topFrame, env)
    tkpack(topFrame, anchor = "nw")
    bottomFrame <- tkframe(frame)
    setState3BFrame(bottomFrame, env)
    tkpack(bottomFrame, padx = 5, pady = 5)
    return(frame)
}
# Creates the left bottom portion of state3 frame
setState3TFrame <- function(frame, env){
    # More arguments for read.table
    moreClicked <- function(){
        moreArgs(env)
    }
    label <- tklabel(frame, text = paste("Editable column names are",
                            "shown in the first entry box on top of data",
                            "columns.\nEditable data type of columns is",
                            " shown in the entry box following coloumn",
                            "names\nClick the check box on top of a column",
                            " to drop the column.\nClik 'More Args' button",
                            " for more arguments.", sep = ""),
                       width = 80, height = 4, justify = "left")
    tkpack(label, anchor = "w", pady = 5, side = "left", padx = 5)
    moreBut <- tkbutton(frame, text = "More Args...", width = 10,
                                                 command = moreClicked)
    tkpack(moreBut, side = "left", padx = 5, pady = 15)
}
# Read the other arguments from a widget for read.table arguments
moreArgs <- function(env){
    temp <- getArgs(env)
    moreArgs <- getMoreArgs()
    for(i in names(moreArgs)){
        temp[[getCState(env)]][[i]] <- moreArgs[[i]]
    }
    assignArgs(temp, env)
}

# Creates the right bottom portion of state3 frame
setState3BFrame <- function(frame, env){
    typeEntry <- list()
    dropCheck <- list()
    nameEntry <- list()
    # Lists to keep the command associated with the radio buttons of
    # entry boxex
    dropCMD <- list()
    nameCMD <- list()
    typeCMD <- list()
    colList <- list()
    rCanv <-  makeViewer(frame, vWidth = 700, vHeight = 280,
                       vScroll = TRUE, hScroll = TRUE,
                       what = "canvas", side = "top")
    tempFrame <- tkframe(rCanv)
    argsList <- getArgs(env)[["state3"]]
    argsList[["nrow"]] <- getShowNum(env)
    dataFile <- do.call("read.table", argsList)
    # Finds the data type for columns
    colInfos <- getColInfo(env)
    # Finds the maximum number of characters for each column
    columnLength <- numberChar(dataFile)
    for(i in 1:ncol(dataFile)){
        colFrame <- tkframe(tempFrame)
        colWidth <- max(columnLength[i], nchar(name(colInfos[[i]])),
                                            nchar(type(colInfos[[i]])))
        dropCMD[[i]] <- function(){}
        body <- list(as.name("{"),
                     substitute(eval(dropColumn(j, env)), list(j = i)))
        body(dropCMD[[i]]) <- as.call(body)
        var <- tclVar()
        dropCheck[[i]] <- tkcheckbutton(colFrame, text = "Drop",
                                   variable = var, command = dropCMD[[i]])
        tkpack(dropCheck[[i]], side = "top")
        nameEntry[[i]] <- tkentry(colFrame, width = colWidth)
        writeList(nameEntry[[i]], colnames(dataFile)[1])
        nameCMD[[i]] <- function(){}
        body <- list(as.name("{"), substitute(eval(setColName(j,
                                           nameEntry, env)), list(j = i)))
        body(nameCMD[[i]]) <- as.call(body)
        tkbind(nameEntry[[i]], "<KeyPress>", nameCMD[[i]])
        tkpack(nameEntry[[i]], side = "top")
        typeEntry[[i]] <- tkentry(colFrame, width = colWidth)
        writeList(typeEntry[[i]], type(colInfos[[i]]))
        typeCMD[[i]] <- function(){}
        body <- list(as.name("{"), substitute(eval(setColType(j,
                                      typeEntry[[i]], env)), list(j = i)))
        body(typeCMD[[i]]) <- as.call(body)
        tkbind(typeEntry[[i]], "<KeyPress>", typeCMD[[i]])
        tkpack(typeEntry[[i]], side = "top")
        colList[[i]] <- tklistbox(colFrame, width = (colWidth),
                                   height = 0, background = "white")
        tkinsert(colList[[i]], "end", dataFile[,i])
        tkpack(colList[[i]], side = "top")
        tkpack(colFrame, side = "left")
    }
    tkcreate(rCanv, "window", 0, 0, anchor = "nw", window = tempFrame)
}
# Set the value of slot 'drop' of a colInfo object
dropColumn <- function(index, env){
    colInfos <- getColInfo(env)
    if(drop(colInfos[[index]])){
        colInfos[[index]] <- drop(colInfos[[index]]) <- FALSE
    }else{
        colInfos[[index]] <- drop(colInfos[[index]]) <- TRUE
    }
    assignColInfo(colInfos, env)
}
# Set the value of slot (column) 'name' of a colInfo object
setColName <- function(index, entryBox, env){
    colInfos <- getColInfo(env)
    entry <- as.character(tkget(entryBox))
    colInfos[[index]] <- name(colInfos[[index]]) <- entry
    assignColInfo(colInfos, env)
}
# Set the value of slot 'type' of a colInfo object
setColType <- function(index, entryBox, env){
    colInfos <- getColInfo(env)
    entry <- as.character(tkget(entryBox))
    colInfos[[index]] <- type(colInfos[[index]]) <- entry
    assignColInfo(colInfos, env)
}

showData4State1 <- function(widget, env){
     dataFile <- readLines(getArgs(env)[["state1"]][["file"]])
     writeList(widget, paste(1:getShowNum(env), ": ",
                             dataFile[1:getShowNum(env)], sep = ""), TRUE)
}

showData4State3 <- function(){
}


whatDeli <- function(delimiter){
    switch(delimiter,
           "\t" = return("tab"),
           ";" = return("semi"),
           " " = return("space"),
           "," = return("comma"),
           stop("Unknown delimiter"))
}

numberChar <- function(matr){
    nchars <- NULL
    for(i in 1:ncol(matr)){
        nchars <- c(nchars, max(nchar(matr[,i])))
    }
    return(nchars)
}

# This function generates a widget using widgetTools to collect all
# the arguments for read.table that are not yet collected by importWizard
getMoreArgs <- function(){
    args <- formals(read.table)
    args <- args[setdiff(names(args),
                         c("file", "header", "sep", "skip","quote"))]

    # Argument fill has to be defined using the value of
    # blank.lines.skip.
    args[["fill"]] <- !args[["blank.lines.skip"]]
    return(argsWidget(args))
}

