# This function provides the interface to browse files under a
# given path.
#

fileBrowser <- function (path = ""){

    require(tcltk) || stop("tcl/tk library not available")
    require(Biobase) || stop("Biobase library not available")

    LABELFONT <- "Helvetica 10"
    BUTWIDTH <- 8

    currentNode <- NULL
    nodes <- NULL
    objLevel <- "Top Level"
    fileOrObj <- "file"
    readMode <- "readTable"
    fileSelected <- ""
    obj <- NULL
    selectedObj <- NULL

    end <- function(){
        tkdestroy(base)
    }

    viewFile <- function(){
        writeCap(path)
        writeDir(listView, list.files(path), path)
        fileOrObj <<- "file"

        if(currentNode >= 2)
            tkconfigure(upBut, state = "normal")
        else
            tkconfigure(upBut, state = "disabled")
    }

    viewObj <- function(){
        writeCap(paste("Object View:", objLevel[length(objLevel)]))
        if(length(objLevel) == 1)
            writeObj(listView, search())
        else
            writeObj(listView,
                     ls(env = get(objLevel[length(objLevel)])))
        fileOrObj <<- "obj"

        if(length(objLevel) > 1)
            tkconfigure(upBut, state = "normal")
        else
            tkconfigure(upBut, state = "disabled")
    }

    doEnv <- function (item){
        fileOrObj <<- "obj"
        writeObj(listView, ls(env = get(item)))
        objLevel <<- c(objLevel, item)
        writeCap(paste("Object View:", item))
    }

    doDF <- function (item){
    }

    doVec <- function(item){
    }

    doList <- function (item){
        fileOrObj <<- "list"

        if(length(objLevel) > 1){
            writeObj(listView, paste("Variable:",
                                     names(get(item,
                                   env = get(objLevel[length(objLevel)])))))
            obj <<- get(item, env = get(objLevel[length(objLevel)]))
        }else{
            writeObj(listView, names(get(item)))
            obj <<- get(item)
        }
        writeCap("Object View: List")

    }

    doMat <- function (item){
        fileOrObj <<- "list"
        doList(item)
    }

    doObj <- function (item, objType){
        fileOrObj <<- objType

        toWrite <- c(paste("Type:", objType),
                         paste("Number of columns:", ncol(get(item))),
                         paste("Number of row(s):", nrow(get(item))),
                         paste("Column Name(s):"),names(get(item)))

        writeObj(listView, toWrite)
        writeCap(paste("Object View:", item))

    }

    showFiles <- function(){
        if(tkcurselection(listView) != ""){
            selectedObj <<- tkget(listView, tkcurselection(listView))
            if(regexpr(Platform()$file.sep, selectedObj) >= 1){
	        path <<- paste(path, Platform()$file.sep,
                               gsub(Platform()$file.sep, "\\", selectedObj),
                               sep = "")
                doPath()
                writeDir(listView, list.files(path), path)
                writeCap(path)
                if(currentNode >= 2)
                    tkconfigure(upBut, state = "normal")
            }else
                fileSelected <<- selectedObj
        }
    }

    showObj <- function (){
        if(tkcurselection(listView) != ""){
            selectedObj <<- tkget(listView, tkcurselection(listView))
            objType <- typeof(get(selectedObj))
print(objType)
            switch(objType,
                   "environment" = doEnv(selectedObj),
                   "data.frame" = doObj(selectedObj, "data.frame"),
                   "vector" = doObj(selectedObj, "vector"),
                   "list" = doObj(selectedObj, "list"),
                   "matrix" = doObj(selectedObj, "matrix"),
                   "integer" = doObj(selectedObj, "integer"),
                   "character" = doObj(selectedObj, "character")
                   )
            if(length(objLevel) >= 2)
                tkconfigure(upBut, state = "normal")
        }
    }

    inList <- function(){
        num <- as.numeric(tkcurselection(listView))+1
        switch(fileOrObj,
               "file" = showFiles(),
               "obj" = showObj()
           )
    }

    doPath <- function(){
	nodes <<- unlist(strsplit(path, Platform()$file.sep))
        currentNode <<- length(nodes)
    }

    fileUP <- function (){
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

    objUP <- function(){
        if(length(objLevel) > 2){
            writeObj(listView,
                     ls(env = get(objLevel[length(objLevel) - 1])))
            writeCap(paste("Object View:",
                           objLevel[length(objLevel) - 1]))
            objLevel <<- objLevel[1:(length(objLevel) - 1)]
        }else{
            writeObj(listView, search())
            writeCap(paste("Object View:", objLevel[1]))
            tkconfigure(upBut, state = "disabled")
        }
    }

    up <- function(){
        switch(fileOrObj,
               "file" = fileUP(),
               "obj" = objUP(),
               viewObj())
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
    tktitle(base) <- paste("File/Object Browser")

    topMenu <- tkmenu(base)
    tkconfigure(base, menu = topMenu)

    viewMenu <- tkmenu(topMenu, tearoff = FALSE)
    tkadd(viewMenu, "command", label = "Files", command = viewFile)
    tkadd(viewMenu, "command", label = "Work Space", command = viewObj)
    tkadd(topMenu, "cascade", label = "View", menu = viewMenu)

    readMenu <- tkmenu(topMenu, tearoff = FALSE)
    tkadd(readMenu, "command", label = "read.csv")
    tkadd(readMenu, "command", label = "read.delim")
    tkadd(readMenu, "command", label = "readLines")
    tkadd(readMenu, "command", label = "scan")
    tkadd(readMenu, "command", label = "read.table")
    tkadd(topMenu, "cascade", label = "Read", menu = readMenu)

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

    if(fileOrObj == "file")
        return(paste(path,  Platform()$file.sep,
                           fileSelected, sep = ""))
    else
        return(obj)
}















