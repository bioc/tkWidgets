# This function provides the interface to browse files under a 
# given path.
#

fileBrowser <- function (path = ""){

    require(tcltk) || stop("tcl/tk library not available")
    LABELFONT <- "Helvetica 10"
    WIDTH <- 440
    HEIGHT <- 300
    OFFSET <- 20
    BUTWIDTH <- 8

    currentNode <- NULL
    nodes <- NULL
    fileSelected <- NULL
    
    end <- function(){
        tkdestroy(base)
    }

    inList <- function(){
        item <- tkget(listView, tkcurselection(listView))
        if(regexpr("^/", item) == 1){
	    path <<- paste(path, item, sep = "")
            doPath()
            writeDir(listView, list.files(path), path)
	    writeCap(path)
	    if(currentNode == length(nodes))
   	        tkconfigure(downBut, state = "disabled")         
        }else
	    fileSelected <- item
    }

    doPath <- function(){
	nodes <<- unlist(strsplit(path, "/"))
        currentNode <<- length(nodes)
    }

    up <- function(){
	if(currentNode > 2){
	    path <<- paste(nodes[1:(currentNode - 1)],
			  sep = "", collapse = "/")
            writeDir(listView, list.files(path), path)
            writeCap(path)
            currentNode <<- currentNode - 1
            if(currentNode == 2)
   	        tkconfigure(upBut, state = "disabled")
	    if(currentNode == (length(nodes) - 1))
		tkconfigure(downBut, state = "normal")
        }
    }

    down <- function(){
	if(currentNode < length(nodes)){
	    path <<- paste(nodes[1:(currentNode + 1)],
			  sep = "", collapse = "/")
            writeDir(listView, list.files(path), path)
            writeCap(path)
            currentNode <<- currentNode + 1
	    if(currentNode == length(nodes))
   	        tkconfigure(downBut, state = "disabled")
	    if(currentNode == 3)
		tkconfigure(upBut, state = "normal")
        }
    }

    writeToView <- function(toWrite){
  	tkdelete(listView, 0, "end")
        tkinsert(listView, 0, toWrite)
    }

    writeCap <- function(toWrite){
	tkconfigure(caption, text = paste("Current Location:",
                                 toWrite))
    }

    if(path == "")
        path <- getwd()
    doPath()

    base <- tktoplevel()
    tktitle(base) <- paste("File Browser")

    canvas <- tkcanvas(base,width = WIDTH, height = HEIGHT)
    tkpack(canvas)

    capFrame <- tkframe(canvas)
    caption <- tklabel(capFrame, text = paste("Current Location:", 
		       path),font = LABELFONT)
    tkpack(caption)
    tkcreate(canvas, "window", (WIDTH/2), OFFSET, 
 	     anchor = "center", window = capFrame)

    viewFrame <- tkframe(canvas)
    listView <- makeView(viewFrame, 25, 15) 
    tkcreate(canvas, "window", 2*OFFSET, 2*OFFSET, anchor = "nw", 
	     window = viewFrame)
    tkbind(listView, "<B1-ButtonRelease>", inList)
    writeDir(listView, list.files(path), path)

    upFrame <- tkframe(canvas)
    upBut <- tkbutton(upFrame, text = "Up", width = BUTWIDTH,
		      command = up)
    tkpack(upBut) 
    tkcreate(canvas, "window", (WIDTH - 85), (HEIGHT - 100), 
	     anchor = "center", window = upFrame)

    downFrame <- tkframe(canvas)
    downBut <- tkbutton(downFrame, text = "Down", width = BUTWIDTH,
			command = down, state = "disabled")
    tkpack(downBut) 
    tkcreate(canvas, "window", (WIDTH - 85), (HEIGHT - 65), 
	     anchor = "center", window = downFrame)

    endFrame <- tkframe(canvas)
    endBut <- tkbutton(endFrame, text = "End", width = BUTWIDTH,
		       command = end)
    tkpack(endBut) 
    tkcreate(canvas, "window", (WIDTH - 85), (HEIGHT - 30), 
	     anchor = "center", window = endFrame)    
}















