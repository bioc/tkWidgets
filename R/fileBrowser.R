# This function provides the interface to browse files in the system
# and returns all or a given number of selected file names.
#

fileBrowser <- function (path = "", testFun = function(x) TRUE,
                         prefix = NULL, suffix = NULL,
                         textToShow = "Select file(s)", nSelect = -1)
{
    on.exit(end())

    LABELFONT1 <- "Helvetica 12 bold"
    LABELFONT2 <- "Helvetica 11"
    BUTWIDTH <- 8
    CANWIDTH <- 500
    CANHEIGHT <- 420
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

    # Close the window. Warn users if the number of selected files is wrong
    end <- function(){
        if(nSelect == -1){
            tkdestroy(base)
        }else{
            if(nSelect != length(fileSelected)){
                tkmessageBox(title = "Wrong number", message =
                       paste("You can only select", nSelect, "file(s)"),
                       icon = "warning", type = "ok")
            }else{
                tkdestroy(base)
            }
        }
    }
    # When a user double clicks a file name in the list box for file names,
    # remembers the file name and path for that name and write the
    # contents to the list box if the name is a directory.
    inList <- function(){
        selectedObj <- as.character(tkget(listView,(tkcurselection(listView))))
        if(regexpr(.Platform$file.sep, selectedObj) >= 1){
            if(currentNode == 1)
                path <<- ""
	    path <<- paste(path, .Platform$file.sep,
                           gsub(.Platform$file.sep, "\\", selectedObj),
                           sep = "")
            doPath()
            writeDir(listView,
                     pickFiles(appendSepDir(path), testFun,
                               prefix, suffix))
            writeCap(path)
            if(currentNode >= 1)
                tkconfigure(upBut, state = "normal")
            tkconfigure(selectBut, state = "disabled")
        }
    }
    # Remember the index of the file name when a user single clicked a
    # file name in the box for file names.
    selInDir <- function (){
        fileIndex <<- NULL
        tkconfigure(selectBut, state = "normal")
        selIndex <<- unlist(strsplit(tkcurselection(listView), " "))
    }
    # Remember the index of the file name when a user single clicked a
    # file name in the box for selected file names.
    selInSelection <- function(){
        selIndex <<- NULL
        if(as.character(tkcurselection(selectView)) != ""){
            fileIndex <<-
                 unlist(strsplit(tkcurselection(selectView), " "))
            tkconfigure(remBut, state = "normal")
        }else
            tkconfigure(remBut, state = "disabled")
    }
    # Write a selected file name to the list box for selected files
    # when a user clicked the select button after clicking a file name
    #in the box for file names.
    selectAFile <- function(){
        if(length(selIndex) > 0){
            for(i in selIndex){
                selObj <- as.character(tkget(listView, i))
                    fileSelected <<- c(fileSelected,
                                    paste(path, .Platform$file.sep,
                                          selObj, sep = ""))
            }
            fileSelected <<- unique(fileSelected)
            writeToView(selectView, fileSelected)
            selIndex <<- NULL
        }
    }
    # Clear the selected file names
    clearSelection <- function(){
        fileSelected <<- NULL
        writeToView(selectView, NULL)
    }
    # Remove a clicked file name from the box for selected file names
    # after a user clicked the remove button.
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
    # Remember the path the browser has traveled.
    doPath <- function(){
	nodes <<- unlist(strsplit(path, .Platform$file.sep))
        currentNode <<- length(nodes)
    }

#    fileUP <- function (){
#        if(currentNode > 2)
#	    path <<- paste(nodes[1:(currentNode - 1)],
#			  sep = "", collapse = .Platform$file.sep)
#        if(currentNode == 2)
#            path <<- .Platform$file.sep
#        writeDir(listView, pickFiles(appendSepDir(path), testFun,
#                                         prefix, suffix))
#        writeCap(path)
#        currentNode <<- currentNode - 1
#        if(currentNode == 1)
#   	    tkconfigure(upBut, state = "disabled")
#    }
    # Move the browser one level up the directory path
    up <- function(){
        tkconfigure(selectBut, state = "disabled")
        if(currentNode > 2)
	    path <<- paste(nodes[1:(currentNode - 1)],
			  sep = "", collapse = .Platform$file.sep)
        if(currentNode == 2)
            path <<- .Platform$file.sep
        writeDir(listView, pickFiles(appendSepDir(path), testFun,
                                         prefix, suffix))
        writeCap(path)
        currentNode <<- currentNode - 1
        if(currentNode == 1)
   	    tkconfigure(upBut, state = "disabled")
    }
    # Write the content of toWrite to a list given box
    writeToView <- function(aView, toWrite){
  	tkdelete(aView, 0, "end")
        if(!is.null(toWrite)){
            for (i in toWrite){
                if(substr(i, nchar(i), (nchar(i) + 1))
                      == .Platform$file.sep)
                    i <- substr(i, 0, (nchar(i) - 1 ))
                tkinsert(aView, "end",
                     gsub(paste(".*", .Platform$file.sep, "(.*)",
                                sep = ""), "\\1", i))
            }
        }
        if(length(toWrite) > 0)
            tkconfigure(clearBut, state = "normal")
        else{
            tkconfigure(clearBut, state = "disabled")
            tkconfigure(remBut, state = "disabled")
        }
    }
    # Refresh the path shown on the widget
    writeCap <- function(toWrite)
	tkconfigure(caption, text = toWrite)

    if(path == "")
        path <- getwd()
    doPath()


    ## Set up the interface
    base <- tktoplevel()
    tktitle(base) <- paste("File Browser")
    # Writes the directory name
    topFrame <- tkframe(base)
    instruct <- tklabel(topFrame, text = textToShow, font = LABELFONT1)
    dir <- tklabel(topFrame, text = "Directory: ", font = LABELFONT2)
    caption <- tklabel(topFrame, text = path, font = LABELFONT2)
    tkgrid(instruct, columnspan = 2)
    tkgrid(dir, caption)
    tkgrid(topFrame, columnspan = 2, padx = 10)
    # Put the list box for file names in a directory and the
    # associated buttons
    leftFrame <- tkframe(base)
    dirLabel <- tklabel(leftFrame, text = "Files in directory",
                        font = "Helvetica 11")
    tkgrid(dirLabel, columnspan = 2)
    dirLFrame <- tkframe(leftFrame)

    listView <- makeViewer(dirLFrame, vWidth = BOXWIDTH,
                         vHeight = BOXHEIGHT)
    tkgrid(dirLFrame, columnspan = 2)
    upBut <- tkbutton(leftFrame, text = "Up", width = BUTWIDTH,
		      command = up)
    selectBut <- tkbutton(leftFrame, text = "Select >>", width = BUTWIDTH,
		      state = "disabled", command = selectAFile)
    tkgrid(upBut, selectBut)
    tkgrid.configure(upBut, sticky = "e")
    tkgrid.configure(selectBut, sticky = "w")
    tkconfigure(listView, selectmode = "extended", font = LABELFONT2)
    tkbind(listView, "<Double-Button-1>", inList)
    tkbind(listView, "<B1-ButtonRelease>", selInDir)
    writeDir(listView, pickFiles(appendSepDir(path), testFun,
                                 prefix, suffix))
    # Put the list box for selected file names and the associated buttons
    rightFrame <- tkframe(base)
    selLabel <- tklabel(rightFrame, text = "Files selected",
                        font = "Helvetica 11")
    tkgrid(selLabel, columnspan = 2)
    selLFrame <- tkframe(rightFrame)
    selectView <- makeViewer(selLFrame, vWidth = BOXWIDTH,
                           vHeight = BOXHEIGHT)
    tkgrid(selLFrame, columnspan = 2)
    tkconfigure(selectView, selectmode = "extended", font = LABELFONT2)
    tkbind(selectView, "<B1-ButtonRelease>", selInSelection)
    remBut <- tkbutton(rightFrame, text = "<< Remove", width = BUTWIDTH,
		      state = "disabled", command = dropSelection)
    clearBut <- tkbutton(rightFrame, text = "Clear", width = BUTWIDTH,
		      state = "disabled", command = clearSelection)
    tkgrid(remBut, clearBut)
    tkgrid.configure(remBut, sticky = "e")
    tkgrid.configure(clearBut, sticky = "w")

    tkgrid(leftFrame, rightFrame)
    # Put the end button
    endBut <- tkbutton(base, text = "Finish", width = BUTWIDTH,
		       command = end)
    tkgrid(endBut, columnspan = 2)

    tkwait.window(base)

    return(fileSelected)
}















