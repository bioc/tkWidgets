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

    end <- function(){
         tkdestroy(top)
    }

    writeEntry <- function(name, toPop){
        tkdelete(name, "0", "end")
        tkinsert(name, "end", toPop)
    }

    init <- function(){
        if(!is.null(filename) && !is.na(filename) && filename != ""){
            tkinsert(nameEntry, "0", filename)
            writeEntry(fileText, readLines(filename))
            makeGuess(filename)
        }
    }

    brows <- function(){
        filename <<- tclvalue(tkcmd("tk_getOpenFile"))
        args$file <<- filename
        writeEntry(fileText, readLines(args$file))
        writeEntry(nameEntry, args$file)
        makeGuess(args$file)
    }

    makeGuess <- function(filename){
        headerNSep <- guess.sep(filename)
        args$header <<- headerNSep$header
        args$sep <<- headerNSep$separator
        if(is.element("header", basic)){
            writeEntry(boxes[["header"]], args$header)
        }
        if(is.element("sep", basic)){
            writeEntry(boxes[["sep"]], .sep2Entry(args$sep))
        }
    }

    view <- function(){
        fileRead <- read.table(file = args$file, head = args$header,
                               sep = args$sep, as.is = TRUE)

        tkdelete(fileText, 0, "end")
        doFormat <- function(line){
            temp <- paste(line, sep = "", collapse = "      ")
            tkinsert(fileText, "end", temp)
        }
        lapply(fileRead, doFormat)
    }

    showMore <- function(){
        writeEntry(restList, rest)
    }

    update <- function(){
        cat("workds")
    }

    argSelected <- function(){
        selectedArg <<-
            tclvalue(tkget(restList,(tkcurselection(restList))))
        writeEntry(restEntry, args[[selectedArg]])
        tkconfigure(restUp, state = "normal")
    }

    top <- tktoplevel()
    tktitle(top) <- "BioC Data Import Wizard"

    nameFrame <- tkframe(top)
    nameLabel <- tklabel(nameFrame, text = "File name:", font = NORMAL11)
    nameEntry <- tkentry(nameFrame, width = 60)
    browsBut <- tkbutton(nameFrame, text = "Browse", command = brows)
    tkpack(nameLabel, side = "left", padx = 2, pady = 4)
    tkpack(nameEntry, side = "left", padx = 2, pady = 4)
    tkpack(browsBut, side = "left", padx = 2, pady = 4)
    tkgrid(nameFrame, columnspan = 2, sticky = "w")

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
            writeEntry(boxes[[basic[i]]], args[[basic[i]]])
            writeEntry(boxes[[basic[i + 1]]], args[[basic[i + 1]]])
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
            writeEntry(boxes[[basic[i]]], args[[basic[i]]])
        }
        i <- (i + 2)
    }
    tkgrid(argFrame, columnspan = 2)
    # Put in the rest of the arguments
    restFrame <- tkframe(top)
    bFrame <- tkframe(restFrame)
    restButton <- tkbutton(bFrame, text = "More", width = 10,
                           command = showMore)
    tkpack(restButton)
    restUp <- tkbutton(bFrame, text = "Update", width = 10,
                       command = update)
    tkpack(restUp)
    tkpack(bFrame, side = "left", padx = 10)
    listFrame <- tkframe(restFrame)
    restList <- makeViewer(listFrame, vHeight = 2, vWidth = 20,
                           vScroll = TRUE)
    tkpack(listFrame, side = "left")
    tkbind(restList, "<B1-ButtonRelease>", argSelected)
    dFrame <- tkframe(restFrame)
    restLabel <- tklabel(dFrame, text = "Default Value:")
    tkpack(restLabel)
    restEntry <- tkentry(dFrame, width = 20)
    tkpack(restEntry)
    tkpack(dFrame, side = "left", padx = 10)
    tkgrid(restFrame, columnspan = 2, pady = 10)
    tkconfigure(restUp, state = "disabled")

    fileFrame <- tkframe(top)
    fileLabel <- paste("Preview of file:", filename)
    previewLabel <- tklabel(fileFrame, text = fileLabel,
                            font = NORMAL11)
    tkgrid(previewLabel, pady = 4, padx = 2, sticky = "nw")
    textFrame <- tkframe(fileFrame)
    fileText <- makeViewer(textFrame, vWidth = 70, vHeight = 15,
                           vScroll = TRUE, hScroll = TRUE)
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

    tkwait.window(top)
}

.sep2Entry <- function(sep){
    switch(sep,
           "\t" = return("\\t"),
           " " = return("\" \""),
           "\n" = return("\\n"),
           return(sep))
}





