# This function reads a defined file and allows users to perform the
# functions related to impoting a file into R
#
# Copyright 2002, J. Zhang, all rights reserved.

fileWizard <- function(filename = "", fun = "read.table"){

    BOLD12 <- "Helvetica 12 bold"
    NORMAL11 <- "Helvetica 11"
    BOXW <- 20

    args <- formals(fun)
    boxNames <- names(args)
    boxNames <- boxNames[boxNames != "file"]
    boxes <- vector(length = length(args))

    end <- function(){
         tkdestroy(top)
    }

    writeEntry <- function(name, toPop){
        tkdelete(name, "0", "end")
        tkinsert(name, "end", toPop)
    }

    init <- function(){
        if(!is.null(filename) || !is.na(filename) || filename != ""){
            tkinsert(nameEntry, "0", filename)
            tkdelete(fileText, 0, "end")
            tkinsert(fileText, "end", readLines(filename))
        }
    }

    brows <- function(){
        filename <<- tclvalue(tkcmd("tk_getOpenFile"))
        args$file <<- filename
        init()
    }

    view <- function(){
        tkdelete(fileText, 0, "end")
        text <- do.call(fun, (paste(names(args), "=", args, sep = ",",
                                 collapse = "")))
        for (i in 1:nrow(text)){
            tkinsert(fileText, "end", paste(text[1,], sep = "\t"))
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

    argFrame <- tkframe(top)
    i <- 1
    while(i <= length(boxNames)){
        if(i != length(boxNames)){
            frame1 <- tkframe(argFrame)
            boxes[boxNames[i]] <- tkentry(frame1, width = BOXW)
            tkpack(tklabel(frame1, text = paste(boxNames[i], ": ",
                            sep = ""), font = NORMAL11), side = "left")
            tkpack(boxes[boxNames[i]], side = "left")
            frame2 <- tkframe(argFrame)
            boxes[boxNames[i + 1]] <- tkentry(frame2, width = BOXW)
            tkpack(tklabel(frame2, text = paste(boxNames[i + 1],
                                   ": ", sep = ""), font =NORMAL11),
                   side = "left")
            tkpack(boxes[boxNames[i + 1]], side = "left")
            tkgrid(frame1, frame2)
            writeEntry(boxes[boxNames[i]], args[[boxNames[i]]])
            writeEntry(boxes[boxNames[i + 1]], args[[boxNames[i + 1]]])
        }else{
            frame1 <- tkframe(argFrame)
            boxes[boxNames[i]] <- tkentry(frame1, width = BOXW)
            tkpack(tklabel(frame1, text = paste(boxNames[i], ": ",
                            sep = ""), font = NORMAL11), side = "left")
            tkpack(boxes[boxNames[i]], side = "left")
            frame2 <- tkframe(argFrame)
            tkgrid(frame1, frame2)
            writeEntry(boxes[boxNames[i]], args[[boxNames[i]]])
        }
        i <- (i + 2)
    }
    tkgrid(argFrame, columnspan = 2, sticky = "nw")

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
}












