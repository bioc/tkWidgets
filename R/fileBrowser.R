# This function provides the interface to browse files under a
# given path.
#

fileBrowser <- function (path = "", testFun = function(x) TRUE,
                         prefix = NULL, suffix = NULL){

    require(tcltk) || stop("tcl/tk library not available")

    LABELFONT <- "Helvetica 12"
    BUTWIDTH <- 8

    currentNode <- NULL
    nodes <- NULL
    fileSelected <- ""

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
                         pickFiles(list.files(path), testFun,
                                   prefix, suffix), path)
                writeCap(path)
                if(currentNode >= 2)
                    tkconfigure(upBut, state = "normal")
                fileSelected <<- ""
            }
        }
    }

    selFile <- function (){
        if(tkcurselection(listView) != ""){
            selectedObj <<- tkget(listView, tkcurselection(listView))
            fileSelected <<- tkget(listView,
                                       tkcurselection(listView))
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
            writeDir(listView, pickFiles(list.files(path), testFun,
                                         prefix, suffix), path)
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
            writeDir(listView, pickFiles(list.files(path), testFun,
                                         prefix, suffix), path)
            writeCap(path)
            currentNode <<- currentNode - 1
            if(currentNode == 2)
   	        tkconfigure(upBut, state = "disabled")
        }
    }

    writeToView <- function(toWrite){
  	tkdelete(listView, 0, "end")
        tkinsert(listView, 0, toWrite)
    }

    writeCap <- function(toWrite)
	tkconfigure(caption, text = paste("Current:", toWrite))

    if(path == "")
        path <- getwd()
    doPath()

    base <- tktoplevel()
    tktitle(base) <- paste("File Browser")

    caption <- tklabel(base, text = paste("Current:", path),
                       font = LABELFONT, width = 60)
    tkpack(caption, side = "top")

    listFrame <- tkframe(base, height = 40)
    listView <- makeView(listFrame)
    tkpack(listFrame, fill = "x", expand = TRUE)

    tkbind(listView, "<Double-Button-1>", inList)
    tkbind(listView, "<B1-ButtonRelease>", selFile)
    writeDir(listView, pickFiles(list.files(path), testFun,
                                 prefix, suffix), path)

    butFrame <- tkframe(base)
    upBut <- tkbutton(butFrame, text = "Up", width = BUTWIDTH,
		      command = up)

    endBut <- tkbutton(butFrame, text = "End", width = BUTWIDTH,
		       command = end)
    tkpack(upBut, endBut, side = "left")
    tkpack(butFrame)

    tkwait.window(base)

    return(paste(path, Platform()$file.sep, fileSelected,
                 sep = ""))
}















