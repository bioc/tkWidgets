# This function provides the interface to browse files under a
# given path.
#

fileBrowser <- function (path = "", testFun = function(x) TRUE,
                         prefix = NULL, suffix = NULL,
                         textToShow = "Choose file(s) to be read in."){

    require(tcltk) || stop("tcl/tk library not available")

    LABELFONT <- "Helvetica 12"
    BUTWIDTH <- 8
    CANWIDTH <- 500
    CANHEIGHT <- 400
    BOXHEIGHT <- 16
    BOXWIDTH <- 29
    OFFSET <- 10
    LABELHEIGHT <- 20

    currentNode <- NULL
    nodes <- NULL
    fileSelected <- NULL
    currentDir <- NULL
    currentFile <- NULL
    selIndex <- NULL
    fileIndex <- NULL
    nextY <- NULL

    end <- function(){
        tkdestroy(base)
    }

    inList <- function(){
       if(tkcurselection(listView) != ""){
            selectedObj <<- tkget(listView,(tkcurselection(listView)))
            if(regexpr(Platform()$file.sep, selectedObj) >= 1){
	        path <<- paste(path, Platform()$file.sep,
                               gsub(Platform()$file.sep, "\\", selectedObj),
                               sep = "")
                doPath()
                writeDir(listView,
                         pickFiles(dirsNFiles(path), testFun,
                                   prefix, suffix))
                writeCap(path)
                if(currentNode >= 2)
                    tkconfigure(upBut, state = "normal")
            }
        }
    }

    selInDir <- function (){
        fileIndex <<- NULL
        if(tkcurselection(listView) != ""){
            tkconfigure(selectBut, state = "normal")
            selIndex <<- unlist(strsplit(tkcurselection(listView), " "))
        }
    }

    selInSelection <- function(){
        selIndex <<- NULL
        if(tkcurselection(selectView) != ""){
            fileIndex <<-
                 unlist(strsplit(tkcurselection(selectView), " "))
            tkconfigure(remBut, state = "normal")
        }else
            tkconfigure(remBut, state = "disabled")
    }

    selectAFile <- function(){
        if(length(selIndex) > 0){
            for(i in selIndex){
                selObj <- tkget(listView, i)
                if(regexpr(".*/", selObj) == -1){
                    fileSelected <<- c(fileSelected,
                                    paste(path, Platform()$file.sep,
                                          selObj, sep = ""))
                }
            }
            fileSelected <<- unique(fileSelected)
            writeToView(selectView, fileSelected)
            selIndex <<- NULL
        }
    }

    clearSelection <- function(){
        fileSelected <<- NULL
        writeToView(selectView, NULL)
    }

    dropSelection <- function(){
        toRemove <- NULL
        if(length(fileIndex) > 0){
            for(i in fileIndex)
                toRemove <- c(toRemove, -(as.numeric(i) + 1))

            fileSelected <<- fileSelected[toRemove]
            writeToView(selectView, fileSelected)
            fileIndex <<- NULL
         }
    }

    doPath <- function(){
	nodes <<- unlist(strsplit(path, Platform()$file.sep))
        currentNode <<- length(nodes)
    }

    fileUP <- function (){
        if(currentNode > 2){
	    path <<- paste(nodes[1:(currentNode - 1)],
			  sep = "", collapse = Platform()$file.sep)
            writeDir(listView, pickFiles(dirsNFiles(path), testFun,
                                         prefix, suffix))
            writeCap(path)
            currentNode <<- currentNode - 1
            if(currentNode == 2)
   	        tkconfigure(upBut, state = "disabled")
        }
    }

    up <- function(){

        if(currentNode > 2){
	    path <<- paste(nodes[1:(currentNode - 1)],
			  sep = "", collapse = Platform()$file.sep)
            writeDir(listView, pickFiles(dirsNFiles(path), testFun,
                                         prefix, suffix))
            writeCap(path)
            currentNode <<- currentNode - 1
            if(currentNode == 2)
   	        tkconfigure(upBut, state = "disabled")
        }
    }

    writeToView <- function(aView, toWrite){
  	tkdelete(aView, 0, "end")
        if(!is.null(toWrite))
            tkinsert(aView, 0,  gsub(".*/(.*)", "\\1", toWrite))
        if(length(toWrite) > 0)
            tkconfigure(clearBut, state = "normal")
        else{
            tkconfigure(clearBut, state = "disabled")
            tkconfigure(remBut, state = "disabled")
        }
    }

    writeCap <- function(toWrite)
	tkconfigure(caption, text = toWrite)

    if(path == "")
        path <- getwd()
    doPath()

    base <- tktoplevel()
    tktitle(base) <- paste("File Browser")

    canvas <- tkcanvas(base, relief = "raised", width = CANWIDTH,
                       height = CANHEIGHT)
    tkpack(canvas, side = "top", fill = "both")

    topFrame <- tkframe(canvas)
    dir <- tklabel(topFrame, text = "Directory: ", font = LABELFONT)
    caption <- tkbutton(topFrame, text = path, width = 50)
    instruct <- tklabel(topFrame, text = textToShow, font = "Helvetica 11")
    tkgrid(dir, caption)
    tkgrid(instruct, columnspan = 2)
    tkcreate(canvas, "window", (CANWIDTH/2), (2 * OFFSET + LABELHEIGHT/2),
	     anchor = "center", window = topFrame)

    leftFrame <- tkframe(canvas)
    dirLabel <- tklabel(leftFrame, text = "Files in directory",
                        font = "Helvetica 10")
    tkgrid(dirLabel, columnspan = 2)
    viewFrame <- tkframe(leftFrame)
    listView <- makeView(viewFrame, vWidth = BOXWIDTH,
                         vHeight = BOXHEIGHT)
    tkgrid(viewFrame, columnspan = 2)
    upBut <- tkbutton(leftFrame, text = "Up", width = BUTWIDTH,
		      command = up)
    selectBut <- tkbutton(leftFrame, text = "Select >>", width = BUTWIDTH,
		      state = "disabled", command = selectAFile)
    tkgrid(upBut, selectBut)
    tkgrid.configure(upBut, sticky = "e")
    tkgrid.configure(selectBut, sticky = "w")
    tkconfigure(listView, selectmode = "extended")
    tkbind(listView, "<Double-Button-1>", inList)
    tkbind(listView, "<B1-ButtonRelease>", selInDir)
    writeDir(listView, pickFiles(dirsNFiles(path), testFun,
                                 prefix, suffix))
    tkcreate(canvas, "window", OFFSET, (LABELHEIGHT + 2 * OFFSET + 10),
	     anchor = "nw", window = leftFrame)

    rightFrame <- tkframe(canvas)
    selLabel <- tklabel(rightFrame, text = "Files selected",
                        font = "Helvetica 10")
    tkgrid(selLabel, columnspan = 2)
    sViewFrame <- tkframe(rightFrame)
    selectView <- makeView(sViewFrame, vWidth = BOXWIDTH,
                           vHeight = BOXHEIGHT)
    tkgrid(sViewFrame, columnspan = 2)
    tkconfigure(selectView, selectmode = "extended")
    tkbind(selectView, "<B1-ButtonRelease>", selInSelection)
    remBut <- tkbutton(rightFrame, text = "<< Remove", width = BUTWIDTH,
		      state = "disabled", command = dropSelection)
    clearBut <- tkbutton(rightFrame, text = "Clear", width = BUTWIDTH,
		      state = "disabled", command = clearSelection)
    tkgrid(remBut, clearBut)
    tkgrid.configure(remBut, sticky = "e")
    tkgrid.configure(clearBut, sticky = "w")
    tkcreate(canvas, "window", (CANWIDTH/2 + OFFSET),
             (LABELHEIGHT + 2 * OFFSET + 10),
	     anchor = "nw", window = rightFrame)

    botFrame <- tkframe(canvas)
    endBut <- tkbutton(botFrame, text = "End", width = BUTWIDTH,
		       command = end)
    tkpack(endBut)
    tkcreate(canvas, "window", CANWIDTH/2, (CANHEIGHT - OFFSET - LABELHEIGHT),
	     anchor = "center", window = botFrame)

    tkwait.window(base)

    return(fileSelected)
}















