# This function provides the interface to view the objects in the
# work place.
#

objectBrowser <- function (){

    require(tcltk) || stop("tcl/tk library not available")

    LABELFONT <- "Helvetica 10"
    BUTWIDTH <- 8

    objLevel <- c("Top Level")
#    obj <- NULL
    selectedObj <- NULL
    isPack <- FALSE
    returnObj <- NULL

    end <- function(){
        tkdestroy(base)
    }

    viewObj <- function(){
        writeCap(objLevel[length(objLevel)])
        if(length(objLevel) == 1)
            writeObj(listView, search())
        else
            writeObj(listView,
                     ls(env = get(objLevel[length(objLevel)])))

        if(length(objLevel) > 1)
            tkconfigure(upBut, state = "normal")
        else
            tkconfigure(upBut, state = "disabled")
    }

    doEnv <- function (item){
        writeObj(listView, ls(env = get(item)))
        objLevel <<- c(objLevel, item)
        writeCap(item)
    }

    doPack <- function (index, pack){
        print(paste("index = ", index))
        writeObj(listView, ls(pos = (as.numeric(index) + 1)))
        isPack <<- TRUE
        writeCap(pack, asis = TRUE)
    }

    doElse <- function(){
        # This a temp function for now. More checking will be inplemented
    }

    doObj <- function (item, objType){
        fileOrObj <<- objType

        if(is.null(ncol(get(item))))
            towrite <- c(paste("Type:", objType),
                         paste("Length:", length(get(item))))


        toWrite <- c(paste("Type:", objType),
                     paste("Number of columns:", ncol(get(item))),
                     paste("Number of row(s):", nrow(get(item))),
                     paste("Column Name(s):"),names(get(item)))

        writeObj(listView, toWrite)
        writeCap(item)

    }

    dClick <- function (){
        if(tkcurselection(listView) != ""){
            selectedObj <<- tkget(listView,
                                      tkcurselection(listView))
            if(regexpr("^package", selectedObj) > 0)
                doPack(tkcurselection(listView), selectedObj)
            else{
                isPack <<- FALSE
                objType <- typeof(get(selectedObj))
                switch(objType,
                   "environment" = doEnv(selectedObj),
                    doElse()
#                   "data.frame" = doObj(selectedObj, "data.frame"),
#                   "vector" = doObj(selectedObj, "vector"),
#                   "list" = doObj(selectedObj, "list"),
#                   "matrix" = doObj(selectedObj, "matrix"),
#                   "integer" = doObj(selectedObj, "integer"),
#                   "character" = doObj(selectedObj, "character")
                       )
                if(length(objLevel) >= 2)
                    tkconfigure(upBut, state = "normal")
            }
        }
    }

    sClick <- function () {
        if(tkcurselection(listView) != ""){
            selectedObj <<- tkget(listView,
                                      tkcurselection(listView))
            if(regexpr("^package", selectedObj) > 0)
                doElse()
                # Find a way to grab the whole package
            else
                returnObj <<- get(selectedObj)
        }
    }

    up <- function(){
        if(isPack){
            writeObj(listView,
                     ls(env = get(objLevel[length(objLevle)])))
            writeCap(objLevel[length(objLevle)])
        }else{
            if(length(objLevel) > 2){
                writeObj(listView,
                     ls(env = get(objLevel[length(objLevel) - 1])))
                writeCap(objLevel[length(objLevel) - 1])
                objLevel <<- objLevel[1:(length(objLevel) - 1)]
            }else{
                writeObj(listView, search())
                writeCap(objLevel[1])
                tkconfigure(upBut, state = "disabled")
            }
        }
    }

    writeCap <- function(objName, asis = FALSE){
        if(asis)
            toWrite <- objName
        else{
            if(objName == "Top Level")
                toWrite <- objName
            else
                toWrite <- paste(objName, ":", typeof(get(objName)))
        }
        tkconfigure(caption, text = toWrite)
    }

    base <- tktoplevel()
    tktitle(base) <- paste("Object Browser")

    topMenu <- tkmenu(base)
    tkconfigure(base, menu = topMenu)

    caption <- tklabel(base, text = "Top Level",
                       font = LABELFONT, width = 60)
    tkpack(caption, side = "top")

    listFrame <- tkframe(base, height = 40)
    listView <- makeView(listFrame)
    tkpack(listFrame, fill = "x", expand = TRUE)
    tkbind(listView, "<Double-Button-1>", dClick)
    tkbind(listView, "<B1-ButtonRelease>", sClick)

    butFrame <- tkframe(base)

    upBut <- tkbutton(butFrame, text = "Up", width = BUTWIDTH,
		      command = up)
    endBut <- tkbutton(butFrame, text = "End", width = BUTWIDTH,
		       command = end)
    tkpack(upBut, endBut, side = "left")
    tkpack(butFrame)

    viewObj()

    tkwait.window(base)

    return(returnObj)
}















