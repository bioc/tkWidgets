# This function provides preliminary visual data inport.
#
# Copyright 2002, J. Zhang, all rights reserved.

fileWizard <- function(filename = "", fun = read.table, file = "file",
                       basic = c("header", "sep")){
    on.exit(end)

    BOLD12 <- "Helvetica 12 bold"
    NORMAL11 <- "Helvetica 11"
    BOXW <- 15

    args <- formals(fun)
    rest <- setdiff(names(args), c(file, basic))
    boxes <- vector("list")
    dList <- vector("list")
    fileRead <- NULL

    # Destroy the window
    end <- function(){
         tkdestroy(top)
    }
    # Write toPop to a given entry box
#    writeEntry <- function(name, toPop){
#        tkdelete(name, "0", "end")
#        tkinsert(name, "end", toPop)
#    }
    # Initialize the list box if fileWizard is called with a file name
    init <- function(){
        if(!is.null(filename) && !is.na(filename) && filename != ""){
            tkinsert(nameEntry, "0", filename)
            writeList(fileText, readLines(filename))
            makeGuess(filename)
        }else{
            writeList(boxes[["header"]], "")
            writeList(boxes[["sep"]], "")
            tkconfigure(finishBut, state = "disabled")
        }
    }
    # Browse directories for a file
    brows <- function(){
        filename <<- tclvalue(tkcmd("tk_getOpenFile"))
        args$file <<- filename
        writeList(fileText, readLines(filename))
        writeList(nameEntry, args$file)
        makeGuess(args$file)
        tkconfigure(finishBut, state = "normal")
    }
    # Take a guess at the delimiter and header of a file
    makeGuess <- function(filename){
        headerNSep <- guess.sep(filename)
        args$header <<- headerNSep$header
        args$sep <<- headerNSep$separator
        if(is.element("header", basic)){
            writeList(boxes[["header"]], ifelse(args$header, "TRUE", "FALSE"))
        }
        if(is.element("sep", basic)){
            writeList(boxes[["sep"]], .sep2Entry(args$sep))
        }
    }
    # View the file after setting some of the arguments. Willl be
    # functioning later.
    view <- function(){
        fileRead <- read.table(file = args$file, head = args$header,
                               sep = args$sep, as.is = TRUE)

        fileRead <- as.matrix(fileRead)
        tkdelete(fileText, 0, "end")

        tkinsert(fileText, "end", fileRead[1:nrow(fileRead),])
    }
    # Read the file in before ending
    finish <- function(){
        for(i in names(boxes)){
            args[[i]] <<- .entry2Arg(tclvalue(tkget(boxes[[i]])))
        }
        fileRead <<- read.table(file = args$file, head = args$header,
                               sep = args$sep, as.is = TRUE)
        end()
    }
    # Not for this release yet
    showMore <- function(){
        writeList(restList, rest)
    }
    # Not for this release yet
    update <- function(){
        cat("workds")
    }
    # Not for this release yet
    argSelected <- function(){
        selectedArg <<-
            tclvalue(tkget(restList,(tkcurselection(restList))))
        writeList(restEntry, args[[selectedArg]])
        tkconfigure(restUp, state = "normal")
    }
    # Set up the interface
    top <- tktoplevel()
    tktitle(top) <- "BioC Data Import Wizard"
    # The top frame that has an entry box for a file name
    nameFrame <- tkframe(top)
    nameLabel <- tklabel(nameFrame, text = "File name:", font = NORMAL11)
    nameEntry <- tkentry(nameFrame, width = 60)
    browsBut <- tkbutton(nameFrame, text = "Browse", command = brows)
    tkpack(nameLabel, side = "left", padx = 2, pady = 4)
    tkpack(nameEntry, side = "left", padx = 2, pady = 4)
    tkpack(browsBut, side = "left", padx = 2, pady = 4)
    tkgrid(nameFrame, columnspan = 2, sticky = "w")
    # Put the entry boxes for basic arguments
    argFrame <- tkframe(top)
    i <- 1
    while(i <= length(basic)){
        if(i != length(basic)){
            frame1 <- tkframe(argFrame)
            boxes[[basic[i]]] <- tkentry(frame1, width = BOXW)
            tempLabel <- tklabel(frame1, text = paste(basic[i], ": ",
                            sep = ""), font = NORMAL11)
            tkgrid(tempLabel, boxes[[basic[i]]])
            frame2 <- tkframe(argFrame)
            boxes[[basic[i + 1]]] <- tkentry(frame2, width = BOXW)
            tempLabel <- tklabel(frame2, text = paste(basic[i + 1],
                                   ": ", sep = ""),
                                 font =NORMAL11)
            tkgrid(tempLabel, boxes[[basic[i + 1]]])
            tkgrid(frame1, frame2, padx = 10)
            tkgrid.configure(frame1, sticky = "w")
            tkgrid.configure(frame2, sticky = "e")
            writeList(boxes[[basic[i]]], args[[basic[i]]])
            writeList(boxes[[basic[i + 1]]], args[[basic[i + 1]]])
        }else{
            # Write the one that is left if the total is an odd number
            frame1 <- tkframe(argFrame)
            boxes[[basic[i]]] <- tkentry(frame1, width = BOXW)
            tempLabel <- tklabel(frame1, text = paste(basic[i], ": ",
                            sep = ""), font = NORMAL11)
            tkgrid(tempLabel, boxes[[basic[i]]])
            tkgrid.configure(tempLabel, sticky = "w")
            tkgrid.configure(boxes[[basic[i]]], sticky = "e")
            frame2 <- tkframe(argFrame)
            frame2 <- tkframe(argFrame)
            tkgrid(frame1, frame2, padx = 10)
            writeList(boxes[[basic[i]]], args[[basic[i]]])
        }
        i <- (i + 2)
    }
    tkgrid(argFrame, columnspan = 2)
    # Put in the rest of the arguments. Not for this release
#    restFrame <- tkframe(top)
#    bFrame <- tkframe(restFrame)
#    restButton <- tkbutton(bFrame, text = "More", width = 10,
#                           command = showMore)
#    tkpack(restButton)
#    restUp <- tkbutton(bFrame, text = "Update", width = 10,
#                       command = update)
#    tkpack(restUp)
#    tkpack(bFrame, side = "left", padx = 10)
#    listFrame <- tkframe(restFrame)
#    restList <- makeViewer(listFrame, vHeight = 2, vWidth = 20,
#                           vScroll = TRUE)
#    tkpack(listFrame, side = "left")
#    tkbind(restList, "<B1-ButtonRelease>", argSelected)
#    dFrame <- tkframe(restFrame)
#    restLabel <- tklabel(dFrame, text = "Default Value:")
#    tkpack(restLabel)
#    restEntry <- tkentry(dFrame, width = 20)
#    tkpack(restEntry)
#    tkpack(dFrame, side = "left", padx = 10)
#    tkgrid(restFrame, columnspan = 2, pady = 10)
#    tkconfigure(restUp, state = "disabled")
    # Put the window for viewing a file
    fileFrame <- tkframe(top)
    fileLabel <- paste("Preview of file:", filename)
    previewLabel <- tklabel(fileFrame, text = fileLabel,
                            font = NORMAL11)
    tkgrid(previewLabel, pady = 4, padx = 2, sticky = "nw")
    textFrame <- tkframe(fileFrame)
    fileText <- makeViewer(textFrame, vWidth = 20, vHeight = 15,
                           vScroll = TRUE, hScroll = TRUE,
                           what = "list")
    tkgrid(textFrame)
    tkgrid(fileFrame, columnspan = 2, padx = 2)
    # Put the buttons for different functions
    butFrame <- tkframe(top)
    cancelBut <- tkbutton(butFrame, text = "Cancel", width = 10,
                          command = end)
#    backBut <- tkbutton(butFrame, text = "< Back", width = 10,
#                        command = init)
#    viewBut <- tkbutton(butFrame, text = "View", width = 10,
#                        command = view)
    finishBut <- tkbutton(butFrame, text = "Finish", width = 10,
                          command = finish)
    tkpack(cancelBut, finishBut, side = "left",
           padx = 2)
    tkgrid(butFrame, columnspan = 2, pady = 4)

    init()

    tkwait.window(top)

    return(fileRead)
}
# Format a delimiter so that it can be displayed in an entry box
.sep2Entry <- function(sep){
    switch(sep,
           "\t" = return("\\t"),
           " " = return("\" \""),
           "\n" = return("\\n"),
           return(sep))
}

.entry2Arg <- function(entry){
    switch(entry,
           "T" = ,
           "TRUE" = ,
           "True" = ,
           "true" = return(TRUE),
           "F" = ,
           "FALSE" = ,
           "False" = ,
           "false" = return(FALSE),
           "\\t" = return("\t"),
           "\"\"" = return(""),
           "\" \"" = return(" "),
           return(entry))
}

