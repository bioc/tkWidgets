# This function provides data import interfaces by mimicing MS Excel's
# Text Wizard. read.table will be used to import the data.
#
# filename - an optional character string for the name of the file to
# be imported
# maxRow - an integer for the maximum number of rows to be
# displayed. Large row numbers may slow down the machine
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

    if(!missing(filename)){
        # Do this if a file name is given
        setArgsList(filename, workEnv)
    }else{
        # Otherwise, assign an empty list to argsList and colInfo
        assignArgs(list(), workEnv)
        setColInfos(env = workEnv)
    }
    # Initializes the interface
    initImportWizard(workEnv)
}
# Using function guess.sep to figure out the the header, sep, and data
# type of a file and sets the argument list and colInfo
setArgsList <- function(filename, env){
    argsList <- list()

    fileInfo <- guess.sep(file.name = filename, n = 40)
    temp <- list()
    temp[["file"]] <- filename
    temp[["header"]] <- fileInfo[["header"]]
    temp[["sep"]] <- fileInfo[["separator"]]
    argsList[["state1"]] <- temp
    assignArgs(argsList, env)
    setColInfos(fileInfo[["type"]], env)
}
# Set and get methods for argument list
assignArgs <- function(value, env){
    assign("argsList", value, env = env)
}
getArgs <- function(env){
    get("argsList", env = env)
}
# Set and get methods for number to show in the interface
assignShowNum <- function(value, env){
    assign("showNum", value, env)
}
getShowNum <- function(env){
    get("showNum", env)
}
# Set and get methods for current state to keep track of the state
assignCState <- function(value, env){
    assign("currentState", value, env)
}
getCState <- function(env){
    get("currentState", env)
}
# Set and get methods for colInfo that is a list of colInfo objects to
# keep column name, type, and drop info
assignColInfo <- function(value, env){
    assign("colInfos", value, env)
}
getColInfo <- function(env){
    get("colInfos", env)
}
# Creates colInfo objects and sets the value of 'colInfos' list
setColInfos <- function(types, env){
#    initColInfo()
    if(missing(types)){
        assignColInfo(list(), env)
    }else{
        temp <- list()
        for(i in 1:length(types)){
            # name = "", type = types[i], drop = FALSE
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
    # A variable to keep the frame that is currently displayed
    currentFrame <- NULL

    on.exit(end())
    # Destroy the window
    end <- function(){
        tkdestroy(top)
    }
    nextState <- function(){
        tempFrame <- changeState(canvas, backBut, nextBut, env, TRUE)
        tkdestroy(currentFrame)
        tkpack(tempFrame, fill = "both", expand = TRUE)
        currentFrame <<- tempFrame
    }
    preState <- function(){
        tempFrame<- changeState(canvas, backBut, nextBut, env, FALSE)
        tkdestroy(currentFrame)
        tkpack(tempFrame, fill = "both", expand = TRUE)
        currentFrame <<- tempFrame
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
    # Sets current frame to state1 now
    currentFrame <- getAFrame(canvas, env)
    tkpack(currentFrame, fill = "both", expand = TRUE)
#    tkcreate(canvas, "window", 0, 0, anchor = "nw", window = currentFrame)
    ## The bottom frame contains the buttons that allow users to
    ## navigate the importing process
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
    tkpack(butFrame, pady = 10, fill = "y", expand = TRUE)

    tkwait.window(top)
    return(dataList)
}
# Creates a top frame of an empty canvas
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
# Changes the state and thus the interface
changeState <- function(canvas, backBut, nextBut, env, forward = TRUE){
    # Sets the current state
    setNewState(env, backBut, nextBut, forward)
    if(forward){
        addArgs(env)
    }else{
        dropArgs(env)
    }
    return(getAFrame(canvas, env))
#    tkcreate(canvas, "window", 0, 0, anchor = "nw", window = newFrame)
}
# Sets the string for the new state (next or previous) and
# actviates/inactivates buttons depending on the state
setNewState <- function(env, backBut, nextBut, forward = TRUE){
    if(forward){
        if(getCState(env) == "state1"){
            assignCState("state2", env)
            tkconfigure(backBut, state = "normal")
        }else{
            assignCState("state3", env)
            tkconfigure(nextBut, state = "disabled")
        }
    }else{
        if(getCState(env) == "state2"){
            assignCState("state1", env)
            tkconfigure(nextBut, state = "normal")
            tkconfigure(backBut, state = "disabled")
        }else{
            assignCState("state2", env)
            tkconfigure(nextBut, state = "normal")
        }
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
# Drop a state arguments list from argsList when the back button is clicked
dropArgs <- function(env){
    temp <- getArgs(env)
    if(length(temp) > 1){
        temp <- temp[-length(temp)]
        assignArgs(temp, env)
    }
}
# Gets a frame based on which state is of interest
getAFrame <- function(base, env){
    switch(getCState(env),
           "state1" = return(getState1Frame(base, env)),
           "state2" = return(getState2Frame(base, env)),
           "state3" = return(getState3Frame(base, env)))
}
# The importing process ends. Return a list with argument list and
# data read using read.table as elements
finish <- function(env){
    switch(getCState(env),
           "state1" = args <- getArgs(env)[["state1"]],
           "state2" = args <- getArgs(env)[["state2"]],
           "state3" = args <- getArgs(env)[["state3"]])
    dataName <- getName4Data(args[["file"]])
    if(is.null(args$sep)){
        dataFile <- readLines(args$file)
    }else{
        dataFile <- do.call("read.table", args)
        colInfos <- getColInfo(env)
        colNames <- NULL
        colToDrop <- NULL
        for(i in 1:length(colInfos)){
            if(drop(colInfos[[i]])){
                colToDrop <- c(colToDrop, i)
            }else{
                switch(type(colInfos[[i]]),
                  "Character" = dataFile[, i] <- as.character(dataFile[, i]),
                  "Numeric" = dataFile[, i] <- as.numeric(dataFile[, i]))
                colNames <- c(colNames, name(colInfos[[i]]))
            }
        }
        # Drop the columns
        if(!is.null(colToDrop)){
            dataFile <- dataFile[, -colToDrop]
        }
        # In case there is only one column left
        if(is.null(ncol(dataFile))){
            dataFile <- data.frame(matrix(dataFile, ncol = 1))
            names(dataFile) <- colNames
        }else{
            names(dataFile) <- colNames
        }
        if(!is.null(dataName)){
            assign(dataName, dataFile, env = .GlobalEnv)
        }
    }
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
    tkpack(topFrame, pady = 5, padx = 5, fill = "both", expand = TRUE)
    tkpack(midFrame, padx = 5, fill = "both", expand = TRUE)
    # Pack the bottom frame last
    tkpack(bottomFrame, pady = 5, padx = 5, fill = "both", expand = TRUE)
    return(frame)
}
# Sets the botton frame for state1
setState1BFrame <- function(frame, env){
    # A list box to show the original data
    viewFrame <- tkframe(frame)
    dataViewer <- makeViewer(viewFrame, vWidth = 50, vHeight = 10,
                            vScroll = TRUE, hScroll = TRUE,
                            what = "list", side = "top")
    tkpack(viewFrame, anchor = "w", pady = 10, fill = "both",
                                                      expand = TRUE)
    return(dataViewer)
}
# Sets the top frame for state1
setState1TFrame <- function(frame, viewer, delims, env){
    # Populate the entry box for file name when the brose button is
    # clicked
    browse <- function(){
        filename <- tclvalue(tkgetOpenFile())
        writeList(nameEntry, filename, clear = TRUE)
        setArgsList(filename, env)
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
    nameEntry <- tkentry(nameFrame, width = 20)
    # If a file name is given, fill the widget with data
    if(!is.null(getArgs(env)[["state1"]][["file"]])){
        writeList(nameEntry, getArgs(env)[["state1"]][["file"]], clear = TRUE)
        showData4State1(viewer, env)
        if(length(getArgs(env)[["state1"]][["sep"]]) != 0){
            tkselect(delims[["delimit"]])
        }
    }
    tkpack(nameEntry, side = "left", fill = "x", expand = TRUE)
    # A button to envoke fileBrowser
    browseBut <- tkbutton(nameFrame, width = 8, text = "Browse",
                          command = browse)
    tkpack(browseBut, side = "left", fill = "x")
    tkpack(nameFrame, fill = "both", expand = TRUE)
}
# Show the data read in using readLines for state1
showData4State1 <- function(widget, env){
     dataFile <- readLines(getArgs(env)[["state1"]][["file"]])
     # Preventing the header to be shown
     if(getArgs(env)[["state1"]][["header"]]){
         dataFile <- dataFile[2:length(dataFile)]
     }
     # determines how many lines to show
     if(length(dataFile) > getShowNum(env)){
         writeList(widget, paste(1:getShowNum(env), ": ",
                             dataFile[1:getShowNum(env)], sep = ""), TRUE)
     }else{
         writeList(widget, paste(1:length(dataFile), ": ",
                                                dataFile, sep = ""), TRUE)
     }
}
# Sets the mid frame for state1
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
                                  anchor = "nw")
    tkpack(delimitRadio, anchor = "w", expand = TRUE, fill = "x")
    fixedRadio <- tkradiobutton(leftPan, text = paste("Fixed",
                                " width - Fields are aligned in columns",
                                " with spaces between fields", sep = ""),
                                value = "fixed", variable = delimit,
                                anchor = "nw")
    tkpack(fixedRadio, anchor = "w", expand = TRUE, fill = "x")
    tkpack(leftPan, side = "left", anchor = "w", fill = "x", expand = TRUE)
    rightPan <- tkframe(frame)
    paraLabel2 <- tklabel(rightPan, text = "Start import at row:")
    tkpack(paraLabel2, side = "left", anchor = "ne")
    startFrame <- tkframe(rightPan)
    startList <- makeViewer(startFrame, vWidth = 2, vHeight = 1,
                            what  = "list", side = "top")
    tkconfigure(startList, selectmode = "single")
    tkbind(startList, "<B1-ButtonRelease>", startClicked)
    writeList(startList, 1:99, clear = TRUE)
    tkpack(startFrame, anchor = "w", side = "left",
                                          fill = "x", expand = TRUE)
    tkpack(rightPan, side = "left", padx = 7, expand = TRUE, fill = "x")
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
# Gets a frame for state2
getState2Frame <- function(base, env, state = "state2"){
    temp <- getArgs(env)
    temp[[state]] <- temp
    frame <- tkframe(base)
    # Shows the name of the file
    label1 <- tklabel(frame, text = paste("File:",
                      getArgs(env)[[state]][["file"]]),
                      font = "Helvetica 11 bold")
    tkpack(label1, pady = 5, padx = 5)
    midFrame <- tkframe(frame)
    setState2MFrame(midFrame, env)
    tkpack(midFrame, pady = 5, padx = 5, fill = "x", expand = TRUE)
    bottomFrame <- tkframe(frame)
    setState2BFrame(bottomFrame, env)
    tkpack(bottomFrame, fill = "both", expand = TRUE)
    return(frame)
}
# Sets the state2 mid frame containing radio buttons for delimiters
# and a list box for quote selection
setState2MFrame <- function(frame,env){
    # Radio buttons for delimiters
    leftFrame <- tkframe(frame)
    setSepRadios(leftFrame, env)
    # A list for quote selecttion (" or/and ')
    rightFrame <- tkframe(frame)
    setQuoteList(rightFrame, env)
    tkpack(leftFrame, side = "left", anchor = "w", fill = "x",
           expand  = TRUE)
    tkpack(rightFrame, side = "left", fill = "x", expand = TRUE)
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
    sepButFun <- function(){
        if(tclvalue(sepVar) != "other"){
            temp <- getArgs(env)
            temp[[state]][["sep"]] <- tclvalue(sepVar)
            assignArgs(temp, env)
            tkconfigure(otherEntry, state = "disabled")
        }else{
            tkconfigure(otherEntry, state = "normal")
        }
    }
    sepEntered <- function(){
        tkselect(sepButs[["other"]])
        temp <- getArgs(env)
        print(paste("sep = ", tkget(otherEntry)))
        temp[[state]][["sep"]] <- as.character(tkget(otherEntry))
        assignArgs(temp, env)
    }
    sepButs[["tab"]] <- tkradiobutton(sepFrame, text = "Tab",
                              variable = sepVar, width = 9,
                              value = "\t", anchor = "nw",
                                      command = sepButFun)
    sepButs[["semi"]] <- tkradiobutton(sepFrame, text = "Semicolon",
                               variable = sepVar, width = 9,
                               value = ";", anchor = "nw",
                                       command = sepButFun)
    sepButs[["comma"]] <- tkradiobutton(sepFrame, text = "Comma",
                              variable = sepVar, value = ",",
                              width = 9, anchor = "nw",
                                        command = sepButFun)
    # Puts the buttons in two rows. First row now
    tkgrid(sepButs[["tab"]], sepButs[["semi"]], sepButs[["comma"]])
    sepButs[["space"]] <- tkradiobutton(sepFrame, text = "Space",
                              variable = sepVar, value = "\"\"",
                              width = 9, anchor = "nw",
                                        command = sepButFun)
    sepButs[["other"]] <- tkradiobutton(sepFrame, text = "Other:",
                              variable = sepVar, value = "other",
                              width = 9, anchor = "nw",
                                        command = sepButFun)
    otherEntry <- tkentry(sepFrame, width = 11, state = "disabled")
    tkbind(otherEntry, "<KeyRelease>", sepEntered)
    # Second row with an entry box for delimiters other those given here
    tkgrid(sepButs[["space"]], sepButs[["other"]], otherEntry)
    tkpack(sepFrame, side = "left", anchor = "ne", fill = "x",
           expand = TRUE)
    if(!is.null(getArgs(env)[[state]][["sep"]])){
        tkselect(sepButs[[whatDeli(getArgs(env)[[state]][["sep"]])]])
    }
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
    tkbind(quoteList, "<B1-ButtonRelease>", quoteSelected)
    writeList(quoteList, c("\"", "'"), clear = TRUE)
    tkpack(quoteFrame, anchor = "w", fill = "x", expand = TRUE)
}

# Sets the value for quote when user selects quote in the list for
# quotes in state2
setQuote <- function(listBox, env, state = "state2"){
    quotes <- ""
    # Quote can be multiple (" and/or ')
    selIndex <- unlist(strsplit(tkcurselection(listBox), " "))
    for(i in selIndex){
        quotes <- paste(quotes, tkget(listBox, i), sep = "")
    }
    temp <- getArgs(env)
    temp[[state]][["quote"]] <- quotes
    assignArgs(temp, env)
}
# Sets the canvas holding the preview of data for state2
setState2BFrame <- function(frame, env){
    viewFrame <- tkframe(frame)
    dataView2 <- makeViewer(viewFrame, vScroll = TRUE, hScroll = TRUE,
                           what = "canvas", side = "top")
    tkpack(viewFrame, anchor = "w", padx = 5, pady = 5, fill = "both",
           expand = TRUE)
    showData4State2(dataView2, env)

}
# Populates the data preview list of state2
showData4State2 <- function(canvas, env, state = "state2"){
    # Only show the number of rows defined
    temp <- getArgs(env)[[state]]
    tempFrame <- tkframe(canvas)
    # If there is no sep, just read the lines
    if(is.null(getArgs(env)[[state]][["sep"]])){
        dataFile <- readLines(temp$file)
        if(length(dataFile) > getShowNum(env)){
            # Cut to right size of file is longer than maxRow
            dataFile <- dataFile[1:getShowNum(env)]
        }
        tempList <- tklistbox(tempFrame, height = 0, width = 0,
                                  background = "white")
        tkinsert(tempList, "end", dataFile)
        tkpack(tempList, side = "left", fill = "both", expand = TRUE)
    }else{
        dataFile <- as.matrix(do.call("read.table", temp))
        if(nrow(dataFile) > getShowNum(env)){
            # Cut to right size of file is longer than maxRow
            dataFile <- dataFile[1:getShowNum(env),]
        }
        for(i in 1:ncol(dataFile)){
            tempList <- tklistbox(tempFrame, height = 0, width = 0,
                                  background = "white")
            tkinsert(tempList, "end", dataFile[,i])
            tkpack(tempList, side = "left", fill = "both", expand = TRUE)
        }
    }
    tkcreate(canvas, "window", 0, 0, anchor = "nw", window = tempFrame)
}

# Gets the frame containing the interface for the top frame of
# importWizard for state3
getState3Frame <- function(base, env){
    # A frame containing the interface that will be returned
    frame <- tkframe(base)
    label1 <- tklabel(frame, text = paste("File:",
                             getArgs(env)[["state3"]][["file"]]),
                      font ="Helvetica 11 bold")
    tkpack(label1, pady = 5)
    topFrame <- tkframe(frame)
    if(!is.null(getArgs(env)[["state3"]][["sep"]])){
        setState3TFrame(topFrame, env)
        tkpack(topFrame, anchor = "nw", fill = "x", expand = TRUE)
    }
    bottomFrame <- tkframe(frame)
    setState3BFrame(bottomFrame, env)
    tkpack(bottomFrame, padx = 5, pady = 5, fill = "both", expand = TRUE)
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
    # Creat a canvas to hold the other widget elements
    rCanv <-  makeViewer(frame, vWidth = 700, vHeight = 280,
                       vScroll = TRUE, hScroll = TRUE,
                       what = "canvas", side = "top")
    tempFrame <- tkframe(rCanv)
    argsList <- getArgs(env)[["state3"]]
    if(is.null(argsList[["sep"]])){
        dataFile <- readLines(argsList[["file"]])
        # Cut to right size of file if longer than maxRow
        if(length(dataFile) > getShowNum(env)){
            dataFile <- dataFile[1:getShowNum(env)]
        }
        writeCol4Lines(tempFrame, dataFile)
    }else{
        dataFile <- do.call("read.table", argsList)
        # Cut to right size of file if longer than maxRow
        if(nrow(dataFile) > getShowNum(env)){
            dataFile <- dataFile[1:getShowNum(env),]
        }
        # Finds the data type for columns
        colInfos <- getColInfo(env)
        # Finds the maximum number of characters for each column
        columnLength <- numberChar(dataFile)
        writeCol4Matrix(tempFrame, dataFile, colInfos,
                        columnLength, env)
    }
    tkcreate(rCanv, "window", 0, 0, anchor = "nw", window = tempFrame)
}
# Create a group of list boxes with entry boxes and a radio button on
# top to allow for user inputs.
writeCol4Matrix <- function(tempFrame, dataFile, colInfos,
                            columnLength, env){
    typeEntry <- list()
    dropCheck <- list()
    nameEntry <- list()
    # Lists to keep the command associated with the radio buttons of
    # entry boxex
    dropCMD <- list()
    nameCMD <- list()
    typeCMD <- list()
    colList <- list()
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
        tkpack(dropCheck[[i]], side = "top", fill = "x", expand = TRUE)
        nameEntry[[i]] <- tkentry(colFrame, width = colWidth)
        writeList(nameEntry[[i]], colnames(dataFile)[i])
        # Also updates the value of colInfos
        temp <- colInfos[[i]]
        name(temp) <- colnames(dataFile)[i]
        colInfos[[i]] <- temp
        nameCMD[[i]] <- function(){}
        body <- list(as.name("{"), substitute(eval(setColName(j,
                                   nameEntry[[j]], env)), list(j = i)))
        body(nameCMD[[i]]) <- as.call(body)
            tkbind(nameEntry[[i]], "<KeyRelease>", nameCMD[[i]])
        tkpack(nameEntry[[i]], side = "top", fill = "x", expand = TRUE)
        typeEntry[[i]] <- tkentry(colFrame, width = colWidth)
        writeList(typeEntry[[i]], type(colInfos[[i]]))
        typeCMD[[i]] <- function(){}
        body <- list(as.name("{"), substitute(eval(setColType(j,
                                      typeEntry[[j]], env)), list(j = i)))
        body(typeCMD[[i]]) <- as.call(body)
        tkbind(typeEntry[[i]], "<KeyRelease>", typeCMD[[i]])
        tkpack(typeEntry[[i]], side = "top", fill = "x", expand = TRUE)
        colList[[i]] <- tklistbox(colFrame, width = (colWidth),
                                  height = 0, background = "white")
        tkinsert(colList[[i]], "end", dataFile[,i])
        tkpack(colList[[i]], side = "top", fill = "x", expand = TRUE)
        tkpack(colFrame, side = "left", fill = "both", expand = TRUE)
    }
    # Sets values for colInfo object
    assignColInfo(colInfos, env)
}
# Create a list box with data written in
writeCol4Lines <- function(tempFrame, dataCol){
    colFrame <- tkframe(tempFrame)
    colList <- tklistbox(colFrame, width = 0, height = 0,
                                         background = "white")
    tkinsert(colList, "end", dataCol)
    tkpack(colList, side = "top", fill = "x", expand = TRUE)
    tkpack(colFrame, side = "left", fill = "both", expand = TRUE)
}

# Set the value of slot 'drop' of a colInfo object
dropColumn <- function(index, env){
    colInfos <- getColInfo(env)
    temp <- colInfos[[index]]
    if(drop(colInfos[[index]])){
        temp <- colInfos[[index]]
        drop(temp) <- FALSE
    }else{
        drop(temp) <- TRUE
    }
    colInfos[[index]] <- temp
    assignColInfo(colInfos, env)
}
# Set the value of slot (column) 'name' of a colInfo object
setColName <- function(index, entryBox, env){
    colInfos <- getColInfo(env)
    entry <- as.character(tkget(entryBox))
    temp <- colInfos[[index]]
    name(temp) <- entry
    colInfos[[index]] <- temp
    assignColInfo(colInfos, env)
}
# Set the value of slot 'type' of a colInfo object
setColType <- function(index, entryBox, env){
    colInfos <- getColInfo(env)
    entry <- as.character(tkget(entryBox))
    temp <- colInfos[[index]]
    type(temp) <- entry
    colInfos[[index]] <- temp
    assignColInfo(colInfos, env)
}
# Gets the word representation of delimiters
whatDeli <- function(delimiter){
    switch(delimiter,
           "\t" = return("tab"),
           ";" = return("semi"),
           " " = return("space"),
           "," = return("comma"),
           stop("Unknown delimiter"))
}
# Given a matrix of characters, figures out the maximum number of
#characters for each column
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
# This function provides the interface for uers to decide whether to
# save the imported data in the global environment
getName4Data <- function(filename){
    # Gets ride of the separaters
    temp <- gsub(paste("^.*", .Platform$file.sep,
                                  "(.*)", sep = ""), "\\1", filename)
    # Gets ride of the extensions
    temp <- strsplit(temp, "\\.")[[1]][1]
    var <- tclVar(temp)
    # Destroy the window
    end <- function(){
        tkdestroy(top)
    }
    # Save the data
    save <- function(){
        temp <<- tclvalue(var)
        end()
    }
    # Do not save the data
    noSave <- function(){
        temp <<- NULL
        end()
    }
    ## Set up the interface
    top <- tktoplevel()
    nameFrame <- tkframe(top)
    tktitle(top) <- "BioC Data Import Wizard"
    inst <- tklabel(nameFrame, text = paste("Save data in .GlobalEnv as:"))
    tkpack(inst, side = "left")
    nameEntry <- tkentry(nameFrame, width = 10, textvariable = var)
    tkpack(nameEntry, side = "left")
    tkpack(nameFrame, side = "top", pady = 5, padx = 5)
    butFrame <- tkframe(top)
    noSaveBut <- tkbutton(butFrame, text = "Do'nt Save", width = 10,
                       command = noSave)
    saveBut <- tkbutton(butFrame, text = "Save", width = 10,
                        command = save)
    tkpack(noSaveBut, saveBut, side = "left")
    tkpack(butFrame, side = "top", pady = 5, padx = 5)

    tkwait.window(top)

    return(temp)
}
