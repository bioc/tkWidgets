# This function provides data import interfaces by mimicing MS Excel's
# Text Wizard. read.table will be used to import the data.
#
# filename - an optional character string for the name of the file to
# be imported
# maxRow - an integer for the maximum number of rows to be
# displayed. Large row numbers may slow down the machine
#
# Copyright 2002, J. Zhang, all rights reserved.

importWizard <- function(filename, maxRow = 400){

    # Creates an environment to make some variables available to all
    # related functions
    workEnv <- new.env(hash = TRUE, parent = NULL)
    # A string to keep track of the current state
    assignCState("state1", env = workEnv)
    # Number of row to be displayed
    assignShowNum(maxRow, env = workEnv)

    if(!missing(filename)){
        # Do this if a file name is given
        argsSet <- setArgsList(filename, workEnv)
    }else{
        # Otherwise, assign an empty list to argsList and colInfo
        assignArgs(list(), workEnv)
        setColInfos(env = workEnv)
        argsSet <- TRUE
    }
    if(argsSet){
        # Initializes the interface
        initImportWizard(workEnv)
    }
}
# Using function guess.sep to figure out the the header, sep, and data
# type of a file and sets the argument list and colInfo
setArgsList <- function(filename, env, isFile = TRUE, init = TRUE){
    options(show.error.messages = FALSE)
    fileInfo <- try(guess.sep(file.name = filename, numLine = 40,
                              isFile = isFile))
    options(show.error.messages = TRUE)
    if(inherits(fileInfo, "try-error")){
        tkmessageBox(title = "Incorrect File Name",
                 message = paste("An error message:\n\n", fileInfo,
                           "\nwas generated while reading file",
                           filename, "."), icon = "error", type = "ok")
        return(FALSE)
    }else{
        if(init){
            argsList <- list()
            temp <- formals("read.table")
            temp[["file"]] <- filename
            temp[["header"]] <- fileInfo[["header"]]
            temp[["sep"]] <- fileInfo[["separator"]]
            temp[["quote"]] <- ""
            # Reassign fill with the value of blank.lines.skip
            temp[["fill"]] <- !temp[["blank.lines.skip"]]
            argsList[["state1"]] <- as.list(temp)
        }else{
            argsList <- getArgs(env)
            argsList[["state1"]][["header"]] <- fileInfo[["header"]]
            argsList[["state1"]][["sep"]] <- fileInfo[["separator"]]
        }
        assignArgs(argsList, env)
        setColInfos(fileInfo[["type"]], env)
        if(isFile){
            assignLineData(readLines(filename, n = getShowNum(env)), env)
        }else{
            assignLineData(filename, env)
        }
        return(TRUE)
    }
}
# Set the temp data read as lines with a maxmun number
assignLineData <- function(lineData, env){
    env[["lineData"]] <<- lineData
    #assign("lineData", lineData, env = env)
}
# Get the temp data stroed as lines with a maxmun number
getLineData <- function(env){
    env$lineData
}
# Set and get methods for argument list
assignArgs <- function(value, env){
    env[["argsList"]] <<- value
    #assign("argsList", value, env = env)
}
getArgs <- function(env){
    env$argsList
}
# Set and get methods for number to show in the interface
assignShowNum <- function(value, env){
    env[["showNum"]] <<- value
    #assign("showNum", value, env)
}
getShowNum <- function(env){
    env$showNum
}
# Set and get methods for current state to keep track of the state
assignCState <- function(value, env){
    env[["currentState"]] <<- value
    #assign("currentState", value, env)
}
getCState <- function(env){
    env$currentState
}
# Set and get methods for colInfo that is a list of colInfo objects to
# keep column name, type, and drop info
assignColInfo <- function(value, env){
    env[["colInfos"]] <<- value
    #assign("colInfos", value, env)
}
getColInfo <- function(env){
    env$colInfos
}
# Creates colInfo objects and sets the value of 'colInfos' list
setColInfos <- function(types, env){
#    initColInfo()
    if(missing(types)){
        assignColInfo(list(), env)
    }else{
        temp <- list()
        for(i in 1:length(types)){
            # name = "", type = types[i], dropOrNot = FALSE
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
        args <- getArgs(env)
        if(is.null(args[["state1"]][["file"]])){
            tkmessageBox(title = "Import Error",
                         message = "I do not know what file to import!",
                         icon = "error",
                         type = "ok")
        }else{
            tempFrame <- changeState(canvas, backBut, nextBut, env, TRUE,
                                 endBut, viewBut)
            tkdestroy(currentFrame)
            tkpack(tempFrame, fill = "both", expand = TRUE)
            currentFrame <<- tempFrame
        }
    }
    preState <- function(){
        tempFrame<- changeState(canvas, backBut, nextBut, env, FALSE,
                                endBut, viewBut)
        tkdestroy(currentFrame)
        tkpack(tempFrame, fill = "both", expand = TRUE)
        currentFrame <<- tempFrame
    }
    redraw <- function(){
        tempFrame <- getAFrame(canvas, env)
        tkdestroy(currentFrame)
        tkpack(tempFrame, fill = "both", expand = TRUE)
        currentFrame <<- tempFrame
    }
    finishClicked <- function(){
        dataList <<- finish(env)
        if(!is.null(dataList)){
            end()
        }
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
    viewBut <- tkbutton(butFrame, text = "View", width = 8,
                        state = "disabled", command = redraw)
    canBut <- tkbutton(butFrame, text = "Cancel", width = 8,
                       command = end)
    backBut <- tkbutton(butFrame, text = "< Back", width = 8,
                        state = "disabled", command = preState)
    nextBut <- tkbutton(butFrame, text = "Next >", width = 8,
                        command = nextState)
    endBut <- tkbutton(butFrame, text = "Finish", width = 8,
                       state = "disabled", command = finishClicked)
    tkpack(canBut, backBut, nextBut, viewBut, endBut, side = "left")
    tkpack(butFrame, pady = 10, fill = "y", expand = TRUE)

    args <- getArgs(env)
    tkwait.window(top)
    return(invisible(dataList))
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
changeState <- function(canvas, backBut, nextBut, env, forward = TRUE,
                        endBut, viewBut){
    # Sets the current state
    setNewState(env, backBut, nextBut, forward, endBut, viewBut)
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
setNewState <- function(env, backBut, nextBut, forward = TRUE,
                        endBut, viewBut){
    if(forward){
        if(getCState(env) == "state1"){
            assignCState("state2", env)
            tkconfigure(backBut, state = "normal")
            tkconfigure(viewBut, state = "normal")
        }else{
            assignCState("state3", env)
            tkconfigure(nextBut, state = "disabled")
            tkconfigure(endBut, state = "normal")
            tkconfigure(viewBut, state = "disabled")
        }
    }else{
        if(getCState(env) == "state2"){
            assignCState("state1", env)
            tkconfigure(nextBut, state = "normal")
            tkconfigure(backBut, state = "disabled")
            tkconfigure(viewBut, state = "disabled")
        }else{
            assignCState("state2", env)
            tkconfigure(nextBut, state = "normal")
            tkconfigure(endBut, state = "disabled")
            tkconfigure(viewBut, state = "normal")
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
    args <- getArgs(env)[["state3"]]
    if(is.null(args[["quote"]])){
        args[["quote"]] <- "\"'"
    }
    dataName <- getName4Data(args[["file"]])
    options(show.error.messages = FALSE)
    dataFile <- try(do.call("read.table", args))
    options(show.error.messages = TRUE)
    if(inherits(dataFile, "try-error")){
        tkmessageBox(title = "Import Error",
                     message = paste("An error message:\n\n", dataFile,
                     "\nwas generated while reading file ",
                     args[["file"]], "."), icon = "error", type = "ok")
        return(NULL)
    }else{
        colInfos <- getColInfo(env)
        colNames <- NULL
        colToDrop <- NULL
        for(i in 1:length(colInfos)){
            if(dropOrNot(colInfos[[i]])){
                colToDrop <- c(colToDrop, i)
            }else{
                switch(colType(colInfos[[i]]),
                   "Character" = dataFile[, i] <- as.character(dataFile[, i]),
                   "Numeric" = dataFile[, i] <- as.numeric(dataFile[, i]))
                colNames <- c(colNames, colName(colInfos[[i]]))
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
            .GlobalEnv[[dataName]] <<- dataFile
            #assign(dataName, dataFile, env = .GlobalEnv)
        }
        return(list(args = args, data = dataFile))
    }
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
    delims <- setState1MFrame(midFrame, env, dataViewer)
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
    fName <- tclVar()
    # Populate the entry box for file name when the brose button is
    # clicked
    browse <- function(){
        filename <- tclvalue(tkgetOpenFile())
        writeList(nameEntry, filename, clear = TRUE)
        argsSet <- setArgsList(filename, env)
        if(argsSet){
            showData4State1(viewer, env)
            if(!is.null(getArgs(env)[["state1"]][["sep"]])){
                tkselect(delims[["delimit"]])
            }
        }
    }
    # Get the file
    getFile <- function(){
        argsSet <- setArgsList(tclvalue(fName), env)
        if(argsSet){
            showData4State1(viewer, env)
            if(!is.null(getArgs(env)[["state1"]][["sep"]])){
                tkselect(delims[["delimit"]])
            }
        }
    }

    # Frame to hole the widgets
    nameFrame <- tkframe(frame)
    label1 <- tklabel(nameFrame, text = "File name: ")
    tkpack(label1, side = "left")
    # An entry box to hold the result of fileBrowser
    nameEntry <- tkentry(nameFrame, width = 20, textvariable = fName)
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
    browseBut <- tkbutton(nameFrame, width = 6, text = "Browse",
                          command = browse)
    getBut <- tkbutton(nameFrame, width = 6, text = "Get",
                       command = getFile)
    tkpack(browseBut, side = "left", fill = "x")
    tkpack(getBut, side = "left", fill = "x")
    tkpack(nameFrame, fill = "both", expand = TRUE)
}
# Show the data read in using readLines for state1
showData4State1 <- function(widget, env){
     skip <- getArgs(env)[["state1"]][["skip"]]
     if(!is.null(skip)){
         dataFile <- getLineData(env)
         showNum <- getShowNum(env)
         if(length(dataFile) > showNum){
             dataFile <- dataFile[(skip + 1):showNum]
         }else{
             dataFile <- dataFile[(skip + 1):length(dataFile)]
         }
     }else{
         dataFile <- getLineData(env)
     }
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
setState1MFrame <- function(frame, env, dataViewer){
    # Executed when values in start at row list box is clicked
    startClicked <- function(){
        setSkip(startList, env)
        args <- getArgs(env)
        skip <- as.numeric(args[["state1"]][["skip"]])
        assignShowNum((getShowNum(env) + skip), env)
        dataFile <- getLineData(env)
        showNum <- getShowNum(env)
        if(length(dataFile) > showNum){
            dataFile <- dataFile[(skip + 1):showNum]
        }else{
            dataFile <- dataFile[(skip + 1):length(dataFile)]
        }
        setArgsList(dataFile, env, FALSE, FALSE)
#        showData4State1(dataViewer, env)

    }
    leftPan <- tkframe(frame)
    delimit <- tclVar()
    delimitRadio <- tkradiobutton(leftPan, text = paste("Delimited",
                                  " - Files are separated by a character",
                                  " such as a comma, tab ...", sep =""),
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
getState2Frame <- function(base, env, state = "state2", reset = FALSE){
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
setState2MFrame <- function(frame, env){
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
    sepButs[["space"]] <- tkradiobutton(sepFrame, text = "Space",
                              variable = sepVar, value = "\"\"",
                              width = 9, anchor = "nw",
                                        command = sepButFun)
    # Puts the buttons in two rows. First row now
    tkgrid(sepButs[["tab"]], sepButs[["semi"]], sepButs[["comma"]],
           sepButs[["space"]])
    sepButs[["newline"]] <- tkradiobutton(sepFrame, text = "Newline",
                              variable = sepVar, value = "\n",
                              width = 9, anchor = "nw",
                                        command = sepButFun)
    sepButs[["other"]] <- tkradiobutton(sepFrame, text = "Other:",
                              variable = sepVar, value = "other",
                              width = 9, anchor = "nw",
                                        command = sepButFun)
    otherEntry <- tkentry(sepFrame, width = 11, state = "disabled")
    tkbind(otherEntry, "<KeyRelease>", sepEntered)
    # Second row with an entry box for delimiters other those given here
    tkgrid(sepButs[["newline"]], sepButs[["other"]], otherEntry)
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
    selIndex <- unlist(strsplit(as.character(tkcurselection(listBox)), " "))
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
    writeColList <- function(i){
        tempList <- tklistbox(tempFrame, height = 0, width = 0,
                              background = "white")
        writeList(tempList, dataFile[,i])
        #tkinsert(tempList, "end", dataFile[,i])
        tkpack(tempList, side = "left", fill = "both", expand = TRUE)
    }

    # Only show the number of rows defined
    temp <- getArgs(env)[[state]]
    tempFrame <- tkframe(canvas)
    # Puts n in temporaly
    temp[["nrows"]] = getShowNum(env)
    dataFile <- do.call("read.table", temp)
    # For data without a separater
    if(is.null(ncol(dataFile))){
        writeColList(1)
    }else{
        for(i in 1:ncol(dataFile)){
            writeColList(i)
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
                            " shown in the first entry box on top of data",
                            " columns.\nEditable data type of columns is",
                            " shown in the entry box following coloumn",
                            " names\nClick the check box on top of a column",
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
    argsList[["nrows"]] <- getShowNum(env)
    dataFile <- do.call("read.table", argsList)
    # Cut to right size of file if longer than maxRow
#    if(nrow(dataFile) > getShowNum(env)){
#        dataFile <- dataFile[1:getShowNum(env),]
#    }
    # Finds the data type for columns
    colInfos <- getColInfo(env)
    writeCol4Matrix(tempFrame, dataFile, colInfos, env)

    tkcreate(rCanv, "window", 0, 0, anchor = "nw", window = tempFrame)
}
# Create a group of list boxes with entry boxes and a radio button on
# top to allow for user inputs.
writeCol4Matrix <- function(tempFrame, dataFile, colInfos, env){
    writeDataCol <- function(i, data){
        colFrame <- tkframe(tempFrame)
        dropCMD[[i]] <- function(){}
        body <- list(as.name("{"),
                     substitute(eval(dropColumn(j, env)), list(j = i)))
        body(dropCMD[[i]]) <- as.call(body)
        var <- tclVar()
        dropCheck[[i]] <- tkcheckbutton(colFrame, text = "Drop",
                                   variable = var, command = dropCMD[[i]])
        tkpack(dropCheck[[i]], side = "top", fill = "x", expand = TRUE)
        nameEntry[[i]] <- tkentry(colFrame, width = 0)
        # Also updates the value of colInfos
        temp <- colInfos[[i]]
        if(!is.null(colnames(data))){
            writeList(nameEntry[[i]], colnames(data))
            colName(temp) <- colnames(data)
        }else{
            writeList(nameEntry[[i]], paste("V", i, sep = ""))
            colName(temp) <- paste("V", i, sep = "")
        }
        colInfos[[i]] <<- temp
        nameCMD[[i]] <- function(){}
        body <- list(as.name("{"), substitute(eval(setColName(j,
                                   nameEntry[[j]], env)), list(j = i)))
        body(nameCMD[[i]]) <- as.call(body)
            tkbind(nameEntry[[i]], "<KeyRelease>", nameCMD[[i]])
        tkpack(nameEntry[[i]], side = "top", fill = "x", expand = TRUE)
        typeEntry[[i]] <- tkentry(colFrame, width = 0)
        writeList(typeEntry[[i]], colType(colInfos[[i]]))
        typeCMD[[i]] <- function(){}
        body <- list(as.name("{"), substitute(eval(setColType(j,
                                      typeEntry[[j]], env)), list(j = i)))
        body(typeCMD[[i]]) <- as.call(body)
        tkbind(typeEntry[[i]], "<KeyRelease>", typeCMD[[i]])
        tkpack(typeEntry[[i]], side = "top", fill = "x", expand = TRUE)
        colList[[i]] <- tklistbox(colFrame, width = 0,
                                  height = 0, background = "white")
        writeList(colList[[i]], data)
#        tkinsert(colList[[i]], "end", data)
        tkpack(colList[[i]], side = "top", fill = "x", expand = TRUE)
        tkpack(colFrame, side = "left", fill = "both", expand = TRUE)
    }
    typeEntry <- list()
    dropCheck <- list()
    nameEntry <- list()
    # Lists to keep the command associated with the radio buttons of
    # entry boxex
    dropCMD <- list()
    nameCMD <- list()
    typeCMD <- list()
    colList <- list()
    if(is.null(ncol(dataFile))){
        writeDataCol(1, dataFile)
    }else{
        for(i in 1:ncol(dataFile)){
            writeDataCol(i, dataFile[,i])
        }
    }
    # Sets values for colInfo object
    assignColInfo(colInfos, env)
}

# Set the value of slot 'drop' of a colInfo object
dropColumn <- function(index, env){
    colInfos <- getColInfo(env)
    temp <- colInfos[[index]]
    if(dropOrNot(colInfos[[index]])){
        temp <- colInfos[[index]]
        dropOrNot(temp) <- FALSE
    }else{
        dropOrNot(temp) <- TRUE
    }
    colInfos[[index]] <- temp
    assignColInfo(colInfos, env)
}
# Set the value of slot (column) 'name' of a colInfo object
setColName <- function(index, entryBox, env){
    colInfos <- getColInfo(env)
    entry <- as.character(tkget(entryBox))
    temp <- colInfos[[index]]
    colName(temp) <- entry
    colInfos[[index]] <- temp
    assignColInfo(colInfos, env)
}
# Set the value of slot 'type' of a colInfo object
setColType <- function(index, entryBox, env){
    colInfos <- getColInfo(env)
    entry <- as.character(tkget(entryBox))
    temp <- colInfos[[index]]
    colType(temp) <- entry
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
           "\n" = return("newline"),
           stop("Unknown delimiter"))
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
