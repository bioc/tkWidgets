# This function reads a defined file and allows users to perform the
# functions related to impoting a file into R
#
# Copyright 2002, J. Zhang, all rights reserved.

fileWizard <- function(filename = "", fun = read.table, file = "file",
                       basic = c("header", "sep")){

    BOLD12 <- "Helvetica 12 bold"
    NORMAL11 <- "Helvetica 11"
    BOXW <- 20

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
        if(!is.null(filename) || !is.na(filename) || filename != ""){
            tkinsert(nameEntry, "0", filename)
            writeEntry(fileText, readLines(filename))
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
    while(i <= length(basic)){
        if(i != length(basic)){
            frame1 <- tkframe(argFrame)
            boxes[basic[i]] <<- tkentry(frame1, width = BOXW)
            tkpack(tklabel(frame1, text = paste(basic[i], ": ",
                            sep = ""), font = NORMAL11), side = "left")
            tkpack(boxes[basic[i]], side = "left")
            frame2 <- tkframe(argFrame)
            boxes[[basic[i + 1]]] <<- tkentry(frame2, width = BOXW)
            tkpack(tklabel(frame2, text = paste(basic[i + 1],
                                   ": ", sep = ""), font =NORMAL11),
                   side = "left")
            tkpack(boxes[[basic[i + 1]]], side = "left")
            tkgrid(frame1, frame2)
            writeEntry(boxes[[basic[i]]], args[[basic[i]]])
            writeEntry(boxes[[basic[i + 1]]], args[[basic[i + 1]]])
        }else{
            frame1 <- tkframe(argFrame)
            boxes[[basic[i]]] <<- tkentry(frame1, width = BOXW)
            tkpack(tklabel(frame1, text = paste(basic[i], ": ",
                            sep = ""), font = NORMAL11), side = "left")
            tkpack(boxes[[basic[i]]], side = "left")
            frame2 <- tkframe(argFrame)
            tkgrid(frame1, frame2)
            writeEntry(boxes[[basic[i]]], args[[basic[i]]])
        }
        i <- (i + 2)
    }
    tkgrid(argFrame, columnspan = 2, sticky = "nw")
    # Put in the rest of the arguments
    restFrame <- tkframe(top)
    restButton <- tkbutton(restFrame, text = "More", width = 8,
                           command = showMore)
    tkpack(restButton, side = "left")
    listFrame <- tkframe(restFrame)
    restList <- makeViewer(listFrame, vHeight = 1, vWidth = 10,
                           vScroll = TRUE)
    tkpack(listFrame, side = "left")
    tkbind(restList, "<B1-ButtonRelease>", argSelected)
    restLabel <- tklabel(restFrame, text = "Default:")
    tkpack(restLabel, side = "left")
    restEntry <- tkentry(restFrame, width = 15)
    tkpack(restEntry, side = "left")
    restUp <- tkbutton(restFrame, text = "Update", width = 8,
                       command = update)
    tkpack(restUp, side = "left")
    tkgrid(restFrame, columnspan = 2)
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
}












