# This function provides data import interfaces by mimicing MS Excel's
# Text Wizard.
#
# Copyright 2002, J. Zhang, all rights reserved.

importWizard <- function(filename = ""){

    on.exit(end)

    WIDTH <- 730
    F1HEIGHT <- 40
    F2HEIGHT <- 300
    F3HEIGHT <- 50
    XMARGIN <- 5
    YMARGIN <- 5
    # A list of frames each renders a state specific interface
    stateFrame <- list()
    # Keeps track of the current state
    state <- "state1"
    # A tk id that will be associated with a frame for a given state
    stateID <- NULL
    # Three argument lists to keep the state specific argument information
    args1 <- list()
    args2 <- list()
    args3 <- list()
    # A vector to keep the data type guessed by funtion guess.sep
    columnType <- NULL
    # Keeps track which of the data coloumns is on focus for state 3
    currentCol <- NULL
    #  A list for data columns of state 3
    colList <- list()
    # A list of buttons for data folumn types of state3
    typeButs <- list()

    # Destroy the window
    end <- function(){
         tkdestroy(top)
    }
    # Populate the entry box for file name when the brose button is
    # clicked
    browse <- function(){
        args1[["file"]] <<- fileBrowser(nSelect = 1)
        writeList(nameEntry, args1[["file"]], clear = TRUE)
        showData(state, dataView1, args1)
        fileInfo <- guess.sep(file.name = args1[["file"]], n = 40)
        if(!is.null(fileInfo)){
            args1[["header"]] <<- fileInfo[["header"]]
            args1[["sep"]] <<- fileInfo[["separator"]]
            columnType <<- fileInfo[["type"]]
        }
        if(length(args1[["sep"]]) != 0){
            tkselect(delimitRadio)
        }
    }
    # Moves to the next state of the three available states when the
    # next button is clicked
    nextState <- function(){
        if(state == "state1"){
            # Retains the state of state1 and initializes the state of state2
            args2 <<- args1
            # Figures out the starting line to import
            options(show.error.messages = FALSE)
            beginAt <- try(as.numeric(tkget(startList,
                                            tkcurselection(statList))))
            options(show.error.messages = TRUE)
            if(inherits(beginAt, "try-error")){
                args2[["skip"]] <<- 0
            }else{
                args2[["skip"]] <<- beginAt - 1
            }
            tkdelete(midCanv, stateID)
            stateID <<- tkcreate(midCanv, "window", XMARGIN, YMARGIN,
                            anchor = "nw", window = stateFrame[["state2"]])
            state <<- "state2"
            if(!is.null(args2[["sep"]])){
                tkselect(checkButs[[whatDeli(args2[["sep"]])]])
            }
            tkconfigure(backBut, state = "normal")
            showData(state, dataView2, args2)
        }else if(state == "state2"){
            args3 <- args2
            # Figures out whether there are quoptes
            options(show.error.messages = FALSE)
            quote <- try(as.character(tkget(quoteList,
                                            tkcurselection(quoteList))))
            options(show.error.messages = TRUE)
            if(inherits(quote, "try-error")){
                args3[["quote"]] <<- quote
            }
            tkdelete(midCanv, stateID)
            stateID <<- tkcreate(midCanv, "window", XMARGIN, YMARGIN,
                          anchor = "nw", window = stateFrame[["state3"]])
            state <<- "state3"
            tkconfigure(nextBut, state = "disabled")
            showData(state, dataView3, args2)
        }
    }
    # Populates the list box for data file
    showData <- function(state, widget, argsList, dataType){
        if(state == "state1"){
            dataFile <- readLines(argsList[["file"]])
            writeList(widget, paste(1:length(dataFile), ": ",
                                        dataFile, sep = ""), TRUE)
        }else if(state == "state2"){
            # Set a limit of 200 rows for better performance and
            # presentation
            argsList[["nrows"]] <- 200
            dataFile <- as.matrix(do.call("read.table", argsList))
            tempFrame <- tkframe(widget)
            for(i in 1:ncol(dataFile)){
                tempList <- tklistbox(tempFrame, width = 0, height = 0,
                                      background = "white")
                tkinsert(tempList, "end", dataFile[,i])
                tkpack(tempList, side = "left")
            }
            tkcreate(widget, "window", 0, 0, anchor = "nw",
                                         window = tempFrame)
        }else{
            argsList[["nrows"]] <- 200
            dataFile <- as.matrix(do.call("read.table", argsList))
            # Figures out the maximum number of characters for each column
            colLength <- numberChar(dataFile)
            # Puts buttons on top of each data column for data type
            tempFrame <- tkframe(widget)
            cmds <- list()
            for(i in 1:ncol(dataFile)){
                colFrame <- tkframe(tempFrame)
                colWidth <- max(colLength[i], nchar(columnType[i]))
                cmds[[i]] <- function(){}
                body <- list(as.name("{"),
                           substitute(eval(typeButFun(j)), list(j = i)))
                body(cmds[[i]]) <- as.call(body)
                typeButs[[i]] <<- tkbutton(colFrame, text = columnType[i],
                                     width = colWidth, command = cmds[[i]])
                tkpack(typeButs[[i]], side = "top")
                colList[[i]] <<- tklistbox(colFrame, width = (colWidth + 3),
                                          height = 0, background = "white")
                tkinsert(colList[[i]], "end", dataFile[,i])
                tkpack(colList[[i]], side = "top")
                tkpack(colFrame, side = "left")
            }
            tkcreate(widget, "window", 0, 0, anchor = "nw",
                                             window = tempFrame)
        }
    }
    # Function for buttons on top of data column lists for state 3
    typeButFun <- function(index){
        if(!is.null(currentCol)){
            bNfGroundColor(colList[[currentCol]], TRUE)
        }
        currentCol <<- index
        bNfGroundColor(colList[[index]], FALSE)
    }
    # Moves one step back when the back button is clicked
    back <- function(){
        if(state == "state2"){
            tkdelete(midCanv, stateID)
            stateID <<- tkcreate(midCanv, "window", XMARGIN, YMARGIN,
                            anchor = "nw", window = stateFrame[["state1"]])
            state <<- "state1"
            tkconfigure(backBut, state = "disabled")
        }else if(state == "state3"){
            tkdelete(midCanv, stateID)
            stateID <<- tkcreate(midCanv, "window", XMARGIN, YMARGIN,
                           anchor = "nw", window = stateFrame[["state2"]])
            state <<- "state2"
        }
    }
    # Closes the top window then the cancel button is clicked
    cancel <- function(){
        end()
    }
    # This function is invoked when the Advanced button of state3 is
    # clicked.
    moreArgs <- function(){
        moreArgs <- getMoreArgs()
    }
    # Ends the process and returns a data frame containing the
    # imported data
    finish <- function(){

    }
    ## Set up the interface
    top <- tktoplevel()
    tktitle(top) <- "BioC Data Import Wizard"
    ## The top canvars has an entry box for a file name
    topCanv <- makeViewer(top, vWidth = WIDTH, vHeight = F1HEIGHT,
                          vScroll = FALSE, hScroll = FALSE,
                          what = "canvas", side = "top")
    nameFrame <- tkframe(topCanv)
    label1 <- tklabel(nameFrame, text = "File name: ")
    tkpack(label1, side = "left")
    # An entry box to hold the result of fileBrowser
    nameEntry <- tkentry(nameFrame, width = 77)
    tkpack(nameEntry, side = "left")
    # A button to envoke fileBrowser
    browseBut <- tkbutton(nameFrame, width = 8, text = "Browse",
                          command = browse)
    tkpack(browseBut)
    tkcreate(topCanv, "window", XMARGIN, YMARGIN, anchor = "nw",
                                                   window = nameFrame)
    ## The middle canvas has widgets for the relevant
    ## arguments and preview of the original data. The content various
    ## depending on the state
    midCanv <- makeViewer(top, vWidth = WIDTH, vHeight = F2HEIGHT,
                           vScroll = FALSE, hScroll = FALSE,
                           what = "canvas", side = "top")
    ###################################################################
    ## The following code defines the interface for state 1 when the ##
    ## widget is first initiated                                     ##
    ###################################################################
    fileType <- c("Text", "Genepix", "Spotfire", "Dchip")
    stateFrame[["state1"]] <- tkframe(midCanv)
    paraLabel1 <- tklabel(stateFrame[["state1"]], text =
               "Choose the file type that best describes your data")
    tkpack(paraLabel1, anchor = "w")
    midFrame <- tkframe(stateFrame[["state1"]])
    leftPan <- tkframe(midFrame)
    delimit <- tclVar()
    delimitRadio <- tkradiobutton(leftPan, text = paste("Delimited",
                                  " - Files are separated by a character",
                                  " such as a comma or tab", sep =""),
                                  value = 1, variable = delimit,
                                  width = 71, anchor = "nw")
    tkpack(delimitRadio, anchor = "w")
    fixedRadio <- tkradiobutton(leftPan, text = paste("Fixed",
                                " width - Fields are aligned in columns",
                                " with spaces between fields", sep = ""),
                                value = 2, variable = delimit,
                                width = 71, anchor = "nw")
    tkpack(fixedRadio, anchor = "w")
    tkpack(leftPan, side = "left", anchor = "w")
    rightPan <- tkframe(midFrame)
    paraLabel2 <- tklabel(rightPan, text = "Start import at row:")
    tkpack(paraLabel2, side = "left", anchor = "ne")
    startFrame <- tkframe(rightPan)
    startList <- makeViewer(startFrame, vWidth = 2, vHeight = 1,
                            what  = "list", side = "top")
    writeList(startList, 1:99, clear = TRUE)
#    tkselect(startList, 1)
    tkpack(startFrame, anchor = "w", side = "left")
    tkpack(rightPan, side = "left", padx = 7)
    tkpack(midFrame, anchor = "w")
    # A list box to show the original data
    viewFrame <- tkframe(stateFrame[["state1"]])
    dataView1 <- makeViewer(viewFrame, vWidth = 99, vHeight = 11,
                           vScroll = TRUE, hScroll = TRUE,
                           what = "list", side = "top")
    tkpack(viewFrame, anchor = "w", pady = 10)
    # State 1 is endered now
    stateID <- tkcreate(midCanv, "window", XMARGIN, YMARGIN,
                        anchor = "nw", window = stateFrame[["state1"]])
    ##################################################################
    ## The following code defines the interface of state 2 when the ##
    ## next button of interface state 1 is clicked                  ##
    ##################################################################
    stateFrame[["state2"]] <- tkframe(midCanv)
    paraLabel21 <- tklabel(stateFrame[["state2"]], text = "Delimiters")
    tkpack(paraLabel21, anchor = "w")
    midFrame <- tkframe(stateFrame[["state2"]])
    leftFrame <- tkframe(midFrame)
    tab <- tclVar()
    semicolon <- tclVar()
    comma <- tclVar()
    space <- tclVar()
    other <- tclVar()
    checkButs <- list()
    checkButs[["tab"]] <- tkcheckbutton(leftFrame, text = "Tab",
                              variable = tab, width = 9, onvalue = "TRUE",
                              offvalue = "FALSE", anchor = "nw")
    checkButs[["semi"]] <- tkcheckbutton(leftFrame, text = "Semicolon",
                               variable = semicolon,
                               width = 9, onvalue = "TRUE",
                               offvalue = "FALSE", anchor = "nw")
    checkButs[["comma"]] <- tkcheckbutton(leftFrame, text = "Comma",
                              variable = comma,
                              width = 9, onvalue = "TRUE",
                              offvalue = "FALSE", anchor = "nw")
    tkgrid(checkButs[["tab"]], checkButs[["semi"]], checkButs[["comma"]])
    checkButs[["space"]] <- tkcheckbutton(leftFrame, text = "Space",
                              variable = space,
                              width = 9, onvalue = "TRUE",
                              offvalue = "FALSE", anchor = "nw")
    checkButs[["other"]] <- tkcheckbutton(leftFrame, text = "Other:",
                              variable = other,
                              width = 9, onvalue = "TRUE",
                              offvalue = "FALSE", anchor = "nw")
    otherEntry <- tkentry(leftFrame, width = 11)
    tkgrid(checkButs[["space"]], checkButs[["other"]], otherEntry)
    tkpack(leftFrame, side = "left", anchor = "w")
    rightFrame <- tkframe(midFrame)
    paraLabel22 <- tklabel(rightFrame, text = "        Quote:")
    tkpack(paraLabel22, side = "left", anchor = "ne")
    quoteFrame <- tkframe(rightFrame)
    quoteList <- makeViewer(quoteFrame, vWidth = 8, vHeight = 1,
                            what  = "list", side = "top")
    writeList(quoteList, c("\"", "'"), clear = TRUE)
    tkpack(quoteFrame, anchor = "w")
    tkpack(rightFrame, side = "left", padx = 5)
    tkpack(midFrame, anchor = "w")
    # A text box to show the original data
    viewFrame <- tkframe(stateFrame[["state2"]])
    dataView2 <- makeViewer(viewFrame, vWidth = 700, vHeight = 192,
                           vScroll = TRUE, hScroll = TRUE,
                           what = "canvas", side = "top")
    tkpack(viewFrame, anchor = "w", pady = 10)
    ###################################################################
    ## The following code defines the interface for state 3 when the ##
    ## next button of interface state 2 is clicked                   ##
    ###################################################################
    stateFrame[["state3"]] <- tkframe(midCanv)
    label31 <- tklabel(stateFrame[["state3"]],
                           text = "Column data format")
    tkpack(label31, anchor = "w")
    midFrame <- tkframe(stateFrame[["state3"]])
    leftFrame <- tkframe(midFrame)
    dataType <- tclVar()
    textRCmd <- function(){
        tkconfigure(typeButs[[currentCol]], text = "Character")
    }
    textRadio <- tkradiobutton(leftFrame, text = "Character",
                               value = 1, variable = dataType,
                               width = 13, anchor = "nw",
                               command = textRCmd)
    tkpack(textRadio, anchor = "w")
    numRCmd <- function(){
        tkconfigure(typeButs[[currentCol]], text = "Numeric")
    }
    numRadio <- tkradiobutton(leftFrame, text = "Numeric",
                              value = 2, variable = dataType,
                              width = 13, anchor = "nw",
                              command = numRCmd)
    tkpack(numRadio, anchor = "w")
#    dateRadio <- tkradiobutton(leftFrame, text = "Date",
#                                  value = 3, variable = dataType,
#                                  width = 13, anchor = "nw")
#    tkpack(dateRadio, anchor = "w")
    skipRCmd <- function(){
        tkconfigure(typeButs[[currentCol]], text = "Skip")
    }
    skipRadio <- tkradiobutton(leftFrame, text = "Drop column",
                               value = 4, variable = dataType,
                               width = 13, anchor = "nw",
                               command = skipRCmd)
    tkpack(skipRadio, anchor = "w")
    tkpack(leftFrame, side = "left", anchor = "w")
    rightFrame <- tkframe(midFrame)
    label32 <- tklabel(rightFrame, text = paste("Use the radio buttons",
                                   "on the left to define the data",
                                   "type for each \ncolumn.", "Drop column",
                                   "prevents a column from being",
                                   "imported."),
                       width = 80, height = 2, justify = "left")
    tkpack(label32, anchor = "w")
    advanceBut <- tkbutton(rightFrame, width = 10, text = "Advanced...",
                           command = moreArgs)
    tkpack(advanceBut, side = "top", pady = 5)
    tkpack(rightFrame, side = "left", pady = 5)
    tkpack(midFrame, anchor = "w")
    # A canvas to show the original data
    viewFrame <- tkframe(stateFrame[["state3"]])
    dataView3 <- makeViewer(viewFrame, vWidth = 700, vHeight = 150,
                           vScroll = TRUE, hScroll = TRUE,
                           what = "canvas", side = "top")
    tkpack(viewFrame, anchor = "w", pady = 10)
    ## The bottom canvas contains the buttons that allow user to
    ## navigate the importingprocess
    butFrame <- tkframe(top)
    canBut <- tkbutton(butFrame, text = "Cancel", width = 8,
                       command = cancel)
    backBut <- tkbutton(butFrame, text = "< Back", width = 8,
                        state = "disabled", command = back)
    nextBut <- tkbutton(butFrame, text = "Next >", width = 8,
                        command = nextState)
    endBut <- tkbutton(butFrame, text = "Finish", width = 8,
                       command = finish)
    tkpack(canBut, backBut, nextBut, endBut, side = "left")
    tkpack(butFrame, pady = 10)

    tkwait.window(top)
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

bNfGroundColor <- function(widget, bWhite = FALSE){
    if(!bWhite){
        tkconfigure(widget, background = "black")
        tkconfigure(widget, foreground = "white")
    }else{
        tkconfigure(widget, background = "white")
        tkconfigure(widget, foreground = "black")
    }
}
# This function generates a widget using widgetTools to collect all
# the arguments for read.table that are not yet collected by importWizard
getMoreArgs <- function(){
    args <- formals(read.table)
    args <- setdiff(args, c("file", "header", "sep", "skip", "quote"))
    # Argument fill has to be defined using the value of
    # blank.lines.skip.
    args[["fill"]] <- !args[["blank.lines.skip"]]
}
