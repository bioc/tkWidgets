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
    dataFile <- NULL

    # Destroy the window
    end <- function(){
         tkdestroy(top)
    }
    # Populate the entry box for file name when the brose button is
    # clicked
    browse <- function(){
        args1[["file"]] <<- fileBrowser(nSelect = 1)
        writeList(nameEntry, args1[["file"]], clear = TRUE)
        dataFile <<- readLines(args1[["file"]])
        showData(state, dataView1, dataFile)
    }
    # Moves to the next state of the three available states when the
    # next button is clicked
    nextState <- function(){
        if(state == "state1"){
            tkdelete(midCanv, stateID)
            stateID <<- tkcreate(midCanv, "window", XMARGIN, YMARGIN,
                            anchor = "nw", window = stateFrame[["state2"]])
            state <<- "state2"
            tkconfigure(backBut, state = "normal")
        }else if(state == "state2"){
            tkdelete(midCanv, stateID)
            stateID <<- tkcreate(midCanv, "window", XMARGIN, YMARGIN,
                          anchor = "nw", window = stateFrame[["state3"]])
            state <<- "state3"
            tkconfigure(nextBut, state = "disabled")
        }else{
        }
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
    layer1 <- tkframe(midFrame)
    delimit <- tclVar(1)
    delimitRadio <- tkradiobutton(layer1, text = paste("Delimited",
                                  " - Files are separated by a character",
                                  " such as a comma or tab", sep =""),
                                  value = 1, variable = delimit,
                                  width = 71, anchor = "nw")
    tkpack(delimitRadio, anchor = "w")
    fixedRadio <- tkradiobutton(layer1, text = paste("Fixed",
                                " width - Fields are aligned in columns",
                                " with spaces between fields", sep = ""),
                                value = 2, variable = delimit,
                                width = 71, anchor = "nw")
    tkpack(fixedRadio, anchor = "w")
    tkpack(layer1, side = "top", anchor = "w")
    layer2 <- tkframe(midFrame)
    paraLabel2 <- tklabel(layer2, text = "Start import at row:")
    tkpack(paraLabel2, side = "left", anchor = "ne")
    startFrame <- tkframe(layer2)
    startList <- makeViewer(startFrame, vWidth = 2, vHeight = 1,
                            what  = "list", side = "top")
    writeList(startList, 1:99, clear = TRUE)
    tkpack(startFrame, anchor = "w", side = "left")
    paraLabel3 <- tklabel(layer2, text = "     File origin:")
    tkpack(paraLabel3, side = "left", anchor = "ne")
    originFrame <- tkframe(layer2)
    originList <- makeViewer(originFrame, vWidth = 8, vHeight = 1,
                            what  = "list", side = "top")
    tkpack(originFrame, side = "left")
    writeList(originList, fileType, clear = TRUE)
    tkpack(layer2, side = "top", pady = 5)
    tkpack(midFrame, anchor = "w")
    # A text box to show the original data
    viewFrame <- tkframe(stateFrame[["state1"]])
    dataView1 <- makeViewer(viewFrame, vWidth = 99, vHeight = 10,
                           vScroll = TRUE, hScroll = TRUE,
                           what = "text", side = "top")
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
    tabCheck <- tkcheckbutton(leftFrame, text = "Tab", variable = tab,
                              width = 9, onvalue = "TRUE",
                              offvalue = "FALSE", anchor = "nw")
    semiCheck <- tkcheckbutton(leftFrame, text = "Semicolon",
                               variable = semicolon,
                               width = 9, onvalue = "TRUE",
                               offvalue = "FALSE", anchor = "nw")
    commaCheck <- tkcheckbutton(leftFrame, text = "Comma", variable = comma,
                              width = 9, onvalue = "TRUE",
                              offvalue = "FALSE", anchor = "nw")
    tkgrid(tabCheck, semiCheck, commaCheck)
    spaceCheck <- tkcheckbutton(leftFrame, text = "Space", variable = space,
                              width = 9, onvalue = "TRUE",
                              offvalue = "FALSE", anchor = "nw")
    otherCheck <- tkcheckbutton(leftFrame, text = "Other:", variable = other,
                              width = 9, onvalue = "TRUE",
                              offvalue = "FALSE", anchor = "nw")
    otherEntry <- tkentry(leftFrame, width = 11)
    tkgrid(spaceCheck, otherCheck, otherEntry)
    tkpack(leftFrame, side = "left", anchor = "w")
    rightFrame <- tkframe(midFrame)
    paraLabel22 <- tklabel(rightFrame, text = "        Text qualifier:")
    tkpack(paraLabel22, side = "left", anchor = "ne")
    qualifFrame <- tkframe(rightFrame)
    qualifList <- makeViewer(qualifFrame, vWidth = 8, vHeight = 1,
                            what  = "list", side = "top")
    writeList(qualifList, c("\"", "'"), clear = TRUE)
    tkpack(qualifFrame, anchor = "w")
    tkpack(rightFrame, side = "left", padx = 5)
    tkpack(midFrame, anchor = "w")
    # A text box to show the original data
    viewFrame <- tkframe(stateFrame[["state2"]])
    dataView2 <- makeViewer(viewFrame, vWidth = 99, vHeight = 14,
                           vScroll = TRUE, hScroll = TRUE,
                           what = "text", side = "top")
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
    dataType <- tclVar(1)
    textRadio <- tkradiobutton(leftFrame, text = "Text",
                                  value = 1, variable = dataType,
                                  width = 13, anchor = "nw")
    tkpack(textRadio, anchor = "w")
    numRadio <- tkradiobutton(leftFrame, text = "Numeric",
                                  value = 2, variable = dataType,
                                  width = 13, anchor = "nw")
    tkpack(numRadio, anchor = "w")
    dateRadio <- tkradiobutton(leftFrame, text = "Date",
                                  value = 3, variable = dataType,
                                  width = 13, anchor = "nw")
    tkpack(dateRadio, anchor = "w")
    skipRadio <- tkradiobutton(leftFrame, text = "Drop column",
                                  value = 4, variable = dataType,
                                  width = 13, anchor = "nw")
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
    advanceBut <- tkbutton(rightFrame, width = 10, text = "Advanced")
    tkpack(advanceBut, side = "top", pady = 5)
    tkpack(rightFrame, side = "left", pady = 5)
    tkpack(midFrame, anchor = "w")
    # A text box to show the original data
    viewFrame <- tkframe(stateFrame[["state3"]])
    dataView3 <- makeViewer(viewFrame, vWidth = 99, vHeight = 10,
                           vScroll = TRUE, hScroll = TRUE,
                           what = "text", side = "top")
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
    endBut <- tkbutton(butFrame, text = "Finish", width = 8)
    tkpack(canBut, backBut, nextBut, endBut, side = "left")
    tkpack(butFrame, pady = 10)

    tkwait.window(top)
}

# Populates the list box for data file
showData <- function(state, listWidget, datafile, argsList){
    if(state == "state1"){
        aFrame <- tkframe(listWidget)
        for(i in 1:length(datafile)){
            tempFrame <- tkframe(aFrame)
            tkpack(tkbutton(tempFrame, width = 1, text = i), side = "left")
            tkpack(tklabel(tempFrame, text = datafile[i]), side = "left")
            tkpack(tempFrame, side = "top", anchor = "w")
        }
        tkwindow.create(listWidget, "end", window = aFrame)
    }else if(state == "state2"){
    }else{
    }
}
