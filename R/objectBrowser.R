# This function provides the interface to view the objects in the
# work place.
#

objectBrowser <- function (fun = function(x) TRUE){

    require(tcltk) || stop("tcl/tk library not available")

    LABELFONT <- "Helvetica 12"
    BUTWIDTH <- 8

    objLevel <- c("Top Level", ".GlobalEnv")
    selectedObj <- NULL
    isPack <- FALSE
    returnObj <- NULL
    returnList <- NULL

    end <- function(){
        tkdestroy(base)
    }

    viewObj <- function(){
        writeCap(objLevel[length(objLevel)])
        if(length(objLevel) == 1)
            writeObj(listView, pickObjs(search(),
                                         fun = fun))
        else
            writeObj(listView, pickObjs(objNames =
                               ls(env = get(objLevel[length(objLevel)])),
                                         fun = fun))

        if(length(objLevel) > 1)
            tkconfigure(upBut, state = "normal")
        else
            tkconfigure(upBut, state = "disabled")
    }

    doEnv <- function (item){
        writeObj(listView,  pickObjs(objNames = ls(env = get(item)),
                                      fun = fun))
        objLevel <<- c(objLevel, item)
        writeCap(item)
    }

    doPack <- function (index, pack){
        whichOne <- as.numeric(index) + 1
        writeObj(listView, ls(pos = whichOne))
        isPack <<- TRUE
        writeCap(pack, asis = TRUE)
        tkconfigure(upBut, state = "normal")
    }

    doElse <- function(){
        # This a temp function for now. More checking will be inplemented
    }

    doFrame <- function (aFrame){

        if(is.null(ncol(get(item))))
            towrite <- c("Type: data frame",
                         paste("Length:", length(get(aFrame))))


        toWrite <- c("Type: data frame",
                     paste("Number of columns:", ncol(get(item))),
                     paste("Number of row(s):", nrow(get(item))),
                     paste("Column Name(s):"),names(get(item)))

        writeObj(listView, toWrite)
        writeCap(aFrame)

    }

    dClick <- function (){
        if(tkcurselection(listView) != ""){
            selectedObj <<- tkget(listView,
                                      tkcurselection(listView))
            goin()
#            objType <- findObjType(selectedObj)
#            switch(objType,
#                "environment" = doEnv(selectedObj),
#                "package" =  doPack(tkcurselection(listView), selectedObj),
#                doElse()
#                   "data.frame" = doObj(selectedObj, "data.frame"),
#                   "vector" = doObj(selectedObj, "vector"),
#                   "list" = doObj(selectedObj, "list"),
#                   "matrix" = doObj(selectedObj, "matrix"),
#                   "integer" = doObj(selectedObj, "integer"),
#                   "character" = doObj(selectedObj, "character")
#                   )
#            if(length(objLevel) >= 2)
#                tkconfigure(upBut, state = "normal")
        }
    }

    goin <- function (){
        if(!is.null(selectedObj)){
            objType <- findObjType(selectedObj)
            switch(objType,
                "environment" = doEnv(selectedObj),
                "package" =  doPack(tkcurselection(listView), selectedObj),
                doElse()
#               "data.frame" = doFrame(selectedObj, "data.frame"),
            )
            if(length(objLevel) >= 2)
            tkconfigure(upBut, state = "normal")
            tkconfigure(visitBut, state = "disabled")
            selectedObj <<- NULL
        }
    }


    sClick <- function () {
        if(tkcurselection(listView) != ""){
            selectedObj <<- tkget(listView,
                                      tkcurselection(listView))
            objType <- findObjType(selectedObj)
            if(objType == "package"){
                returnList <<- list("name" = gsub("(^package:)", "\\",
                                    selectedObj), "obj"  =
                                    package.contents(gsub("(^package:)",
                                                     "\\", selectedObj)))
                tkconfigure(visitBut, state = "normal")
            }else{
                switch(objType,
                    "environment" = tkconfigure(visitBut,state="normal"),
                    "data.frame" = tkconfigure(visitBut, state="normal")
                       )
                returnList <<- list("name" = selectedObj,
                                    "obj" = get(selectedObj))
            }
        }
    }

    getAct <- function(){
        selectedObj <<- ".GlobalEnv"
        goin()
    }

    up <- function(){
        if(isPack){
            writeObj(listView, pickObjs(objNames = search(),
                                             fun = fun))
            writeCap(objLevel[1])
        }else{
            if(length(objLevel) > 2){
                writeObj(listView, pickObjs(objNames =
                     ls(env = get(objLevel[length(objLevel) - 1])),
                                             fun = fun))
                writeCap(objLevel[length(objLevel) - 1])
                objLevel <<- objLevel[1:(length(objLevel) - 1)]
            }else{
                writeObj(listView, pickObjs(objNames = search(),
                                             fun = fun))
                writeCap(objLevel[1])
                tkconfigure(upBut, state = "disabled")
            }
        }
    }

    writeCap <- function(objName, asis = FALSE){
        if(asis){
            tkconfigure(butVisit, text = objName)
            tkconfigure(butType, text = findObjType(objName))
        }else{
            if(objName == "Top Level"){
                tkconfigure(butVisit, text = "Top level")
                tkconfigure(butType, text = "Search path")
            }else{
                tkconfigure(butVisit, text = objName)
                tkconfigure(butType, text = findObjType(objName))
            }
        }
    }

    base <- tktoplevel()
    tktitle(base) <- paste("Object Browser")

    capFrame <- tkframe(base)
    labl1 <- tklabel(capFrame, text = "Visiting: ", font = LABELFONT)
    labl2 <- tklabel(capFrame, text = "Type: ", font = LABELFONT)
    butVisit <- tkbutton(capFrame, text = "",
                         font = LABELFONT, width = 18)
    butType <- tkbutton(capFrame, text = "",
                        font = LABELFONT, width = 18)

    tkgrid(labl1, butVisit, labl2, butType)
    tkpack(capFrame, side = "top")

    listFrame <- tkframe(base, height = 40)
    listView <- makeView(listFrame)
    tkpack(listFrame, fill = "x", expand = TRUE)
    tkbind(listView, "<Double-Button-1>", dClick)
    tkbind(listView, "<B1-ButtonRelease>", sClick)

    butFrame <- tkframe(base)

    upBut <- tkbutton(butFrame, text = "Parent", width = BUTWIDTH,
		      command = up)
    activeBut <- tkbutton(butFrame, text = "Active", width = BUTWIDTH,
		      command = getAct)
    visitBut <- tkbutton(butFrame, text = "Visit", width = BUTWIDTH,
		      command = goin, state = "disabled")
    endBut <- tkbutton(butFrame, text = "End", width = BUTWIDTH,
		       command = end)
    tkpack(upBut, activeBut, visitBut, endBut, side = "left")
    tkpack(butFrame)

    viewObj()

    tkwait.window(base)

    return(returnList)
}















