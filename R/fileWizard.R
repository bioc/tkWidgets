# This function reads a defined file and allows users to perform the
# functions related to impoting a file into R
#
# Copyright 2002, J. Zhang, all rights reserved.

fileWizard <- function(filename = ""){
    require(tcltk) || stop("tcltk is not available.")

    BOLD12 <- "Helvetica 12 bold"
    NORMAL11 <- "Helvetica 11"
    DELIMITER <- c("Tab", "Semicolon", "comma", "Space", "Other")
    NUMS <- c(1:20)

    type <- tclVar()
    type <- "delimited"
    sep <- "\t"
    skip <- 0

    end <- function(){
         tkdestroy(top)
    }

    popLists <- function(){
        for(i in DELIMITER)
            tkinsert(sepList, "end", i)
        tkinsert(skipList, "end", "0")
        for(i in NUMS){
            tkinsert(widthList, "end", i)
            tkinsert(skipList, "end", i)
        }
    }

    init <- function(){
        if(!is.null(filename) || !is.na(filename) || filename != ""){
            tkinsert(nameEntry, "0", filename)
            tkdelete(fileText, 0, "end")
            tkinsert(fileText, "end", readLines(filename))
        }
    }

    brows <- function(){
        filename <<- tkcmd("tk_getOpenFile")
        init()
    }

    deliSelected <- function(){
        tkconfigure(widthList, state = "disabled")
        tkconfigure(sepList, state = "normal")
    }

    fixSelected <- function(){
        tkconfigure(sepList, state = "disabled")
        tkconfigure(widthList, state = "normal")
    }

    sepSel <- function(){
        switch(tclvalue(tkget(sepList, (tkcurselection(sepList)))),
               "Tab" = sep <<- "\t",
               "Semicolon" = sep <<- ";",
               "comma" = sep <<- ",",
               "Space" = sep <<- " ",
               sep <<- tclvalue(tkget(sepList,(tkcurselection(sepList)))))
    }

    skipSel <- function(){
        skip <<- as.character(tclvalue(tkget(skipList,
                                             tkcurselection(skipList))))
    }

    view <- function(){
        if(type == "delimited"){
            tkdelete(fileText, 0, "end")
            tkinsert(fileText, "end",
                     read.table(filename, sep = sep, header = FALSE,
                                skip = skip))
        }else if(tclvalue(type) == "fixed"){
        }
    }

    top <- tktoplevel()
    tktitle(top) <- "BioC Text Import Wizard"

    nameFrame <- tkframe(top)
    nameLabel <- tklabel(nameFrame, text = "File name:", font = NORMAL11)
    nameEntry <- tkentry(nameFrame, width = 60)
    browsBut <- tkbutton(nameFrame, text = "Brows", command = brows)
    tkpack(nameLabel, side = "left", padx = 2, pady = 4)
    tkpack(nameEntry, side = "left", padx = 2, pady = 4)
    tkpack(browsBut, side = "left", padx = 2, pady = 4)
    tkgrid(nameFrame, columnspan = 2, sticky = "w")

    typeFrame <- tkframe(top)
    typeLabel <- tklabel(typeFrame, text = "Original data type:",
                        font = NORMAL11)
    radioFrame <- tkframe(typeFrame)
    deliBut <- tkradiobutton(radioFrame, text = "Delimited",
                             variable = type, value = "delimited",
                             font = NORMAL11, command = deliSelected)
    fixedBut <- tkradiobutton(radioFrame, text = "Fixed width",
                              variable = type, value = "fixed",
                              font = NORMAL11, command = fixSelected)
    tkpack(deliBut, fixedBut, side = "top", anchor = "w")
    sepFrame <- tkframe(typeFrame)
    sepLabel <- tklabel(sepFrame, text = "Delimiter:", font = NORMAL11)
    sepList <- tklistbox(sepFrame, selectmode = "browse",
                         width = 10, height = 1)
    tkbind(sepList, "<B1-ButtonRelease>", sepSel)
    tkgrid(sepLabel, sepList, padx = 2)
    widthLabel <- tklabel(sepFrame, text = "Width:", font = NORMAL11)
    widthList <- tklistbox(sepFrame, selectmode = "browse",
                           width = 10, height = 1)
    tkgrid(widthLabel, widthList, padx = 2)
    tkgrid.configure(sepLabel, widthLabel, sticky = "e")
    tkgrid.configure(sepList, widthList, sticky = "w")
    tkpack(sepFrame)
    tkgrid(typeLabel, radioFrame, sepFrame, sticky = "nw", pady = 4,
           padx = 2)
    skipFrame <- tkframe(top)
    skipLabel <- tklabel(skipFrame, text = "Start at row:",
                         font = NORMAL11)
    skipList <- tklistbox(skipFrame, selectmode = "browse",
                          height = 1, width = 10)
    tkbind(skipList, "<B1-ButtonRelease>", skipSel)
    tkgrid(skipLabel, skipList, pady = 4, sticky = "nw")

    tkgrid(typeFrame, skipFrame, sticky = "nw")

    fileFrame <- tkframe(top)
    fileLabel <- paste("Preview of file:", filename)
    previewLabel <- tklabel(fileFrame, text = fileLabel,
                            font = NORMAL11)
    tkgrid(previewLabel, pady = 4, padx = 2, sticky = "nw")
    textFrame <- tkframe(fileFrame)
    fileText <- tklistbox(textFrame, height = 10)
    vscr <- tkscrollbar(textFrame,
                        command = function(...) tkyview(fileText, ...))
    hscr <- tkscrollbar(textFrame, orient = "horizontal",
                       command = function(...) tkxview(fileText, ...))
    tkconfigure(fileText,
                yscrollcommand = function(...) tkset(vscr,...),
                xscrollcommand = function(...) tkset(hscr, ...))
    tkpack(vscr, side = "right", fill = "y")
    tkpack(hscr, side = "bottom", fill = "x")
    tkpack(fileText, fill = "both", expand = TRUE)
    tkgrid(textFrame)
    tkgrid(fileFrame, columnspan = 2, padx = 2)

    butFrame <- tkframe(top)
    cancelBut <- tkbutton(butFrame, text = "Cancel", width = 10,
                          command = end)
    backBut <- tkbutton(butFrame, text = "< Back", width = 10,
                        command = init)
    viewBut <- tkbutton(butFrame, text = "View", width = 10,
                        command = view)
    finishBut <- tkbutton(butFrame, text = "Finish", width = 10,
                          command = view)
    tkpack(cancelBut, backBut, viewBut, finishBut, side = "left",
           padx = 2)
    tkgrid(butFrame, columnspan = 2, pady = 4)

    init()
    popLists()
}












