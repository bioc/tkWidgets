# This function provides the interface to view the objects in the
# work place.
#

objectBrowser <- function (){

    require(tcltk) || stop("tcl/tk library not available")

    LABELFONT <- "Helvetica 10"
    BUTWIDTH <- 8

    objLevel <- "Top Level"
    obj <- NULL
    selectedObj <- NULL

    end <- function(){
        tkdestroy(base)
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

    doObj <- function (item, objType){
        fileOrObj <<- objType

        if(is.null(ncol(get(item))))
            towrite <- c(paste("Type:", objType),
                         paste("Length:", length(get(item))),
                         "Select a read tool from the menu to view.")


        toWrite <- c(paste("Type:", objType),
                     paste("Number of columns:", ncol(get(item))),
                     paste("Number of row(s):", nrow(get(item))),
                     paste("Column Name(s):"),names(get(item)),
                     "Select a read tool from the menu to view.")

        writeObj(listView, toWrite)
        writeCap(paste("Object View:", item))

    }

    inList <- function (){
        if(tkcurselection(listView) != ""){
            selectedObj <<- tkget(listView, tkcurselection(listView))
            objType <- typeof(get(selectedObj))

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

    up <- function(){
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

    writeCap <- function(toWrite)
	tkconfigure(caption, text = toWrite)

    base <- tktoplevel()
    tktitle(base) <- paste("Object Browser")

    topMenu <- tkmenu(base)
    tkconfigure(base, menu = topMenu)

    caption <- tklabel(base, text = "Object View",
                       font = LABELFONT, width = 60)
    tkpack(caption, side = "top")

    listFrame <- tkframe(base, height = 40)
    listView <- makeView(listFrame)
    tkpack(listFrame, fill = "x", expand = TRUE)

    tkbind(listView, "<B1-ButtonRelease>", inList)

    butFrame <- tkframe(base)

    upBut <- tkbutton(butFrame, text = "Up", width = BUTWIDTH,
		      command = up)
    endBut <- tkbutton(butFrame, text = "End", width = BUTWIDTH,
		       command = end)
    tkpack(upBut, endBut, side = "left")
    tkpack(butFrame)

    viewObj()

    tkwait.window(base)

    return(obj)
}















