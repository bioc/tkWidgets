# This function provides the interface to browse files under a
# given path.
#

fileBrowser <- function (path = ""){

    require(tcltk) || stop("tcl/tk library not available")
    LABELFONT <- "Helvetica 10"
    BUTWIDTH <- 8

    currentNode <- NULL
    nodes <- NULL
    fileSelected <- ""

    end <- function(){
        tkdestroy(base)
    }

    inList <- function(){
        if(tkcurselection(listView) != ""){
            item <- tkget(listView, tkcurselection(listView))
            if(regexpr(Platform()$file.sep, item) >= 1){
	        path <<- paste(path, Platform()$file.sep,
                           gsub(Platform()$file.sep, "\\", item),
                           sep = "")
                doPath()
                writeDir(listView, list.files(path), path)
	        writeCap(path)
                if(currentNode >= 2)
                    tkconfigure(upBut, state = "normal")
            }else
	        fileSelected <<- item
        }
    }

    doPath <- function(){
	nodes <<- unlist(strsplit(path, Platform()$file.sep))
        currentNode <<- length(nodes)
    }

    up <- function(){
	if(currentNode > 2){
	    path <<- paste(nodes[1:(currentNode - 1)],
			  sep = "", collapse = Platform()$file.sep)
            writeDir(listView, list.files(path), path)
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
	tkconfigure(caption, text = toWrite)

    if(path == "")
        path <- getwd()
    doPath()

    base <- tktoplevel()
    tktitle(base) <- paste("File Browser")

    caption <- tklabel(base, text = path,font = LABELFONT,
                       width = 60)
    tkpack(caption, side = "top")

    listFrame <- tkframe(base, height = 40)
    listView <- makeView(listFrame)
    tkpack(listFrame, fill = "x", expand = TRUE)

    tkbind(listView, "<B1-ButtonRelease>", inList)
    writeDir(listView, list.files(path), path)

    butFrame <- tkframe(base)
    upBut <- tkbutton(butFrame, text = "Up", width = BUTWIDTH,
		      command = up)

    endBut <- tkbutton(butFrame, text = "End", width = BUTWIDTH,
		       command = end)
    tkpack(upBut, endBut, side = "left")
    tkpack(butFrame)

    tkwait.window(base)

    return(invisible(paste(path,  Platform()$file.sep,
                           fileSelected, sep = "")))
}















