# This function provides the interface to view the objects in the
# work place.

objectBrowser<- function (fun = noAuto, textToShow = "Select object(s)",
                          nSelect = -1){

    on.exit(options(show.error.messages = TRUE))
    on.exit(end())

    LABELFONT1 <- "Helvetica 12 bold"
    LABELFONT2 <- "Helvetica 11"
    BUTWIDTH <- 8

    selectedObj <- NULL
    isPack <- FALSE
    returnObj <- NULL
    returnList <- NULL
    selIndex <- NULL
    objIndex <- NULL
    objsInSel <- NULL
    tempObj <- NULL

    end <- function(){
        if(length(objsInSel) != 0){
            if(nSelect == -1){
                returnList <<- nameToObjList(objsInSel)
                tkdestroy(base)
            }else{
                if(nSelect != length(objsInSel)){
                    tkmessageBox(title = "Wrong number", message =
                       paste("You can only select", nSelect, "object(s)"),
                       icon = "warning", type = "ok")
                }else{
                    returnList <<- nameToObjList(objsInSel)
                    tkdestroy(base)
                }
            }
        }else{
            returnList <<- NULL
            tkdestroy(base)
        }
    }

    viewGlobalEnv <- function(){
        writeObj(listView, pickObjs(objNames = ls(env = .GlobalEnv,
                                    all = TRUE), fun = fun))
        writeCap(".GlobalEnv")
    }

    doEnv <- function (item){
        writeObj(listView,  pickObjs(objNames = ls(env = get(item)),
                                      fun = fun))
        writeCap(item)
        if(!is.null(parent.env(get(item))))
            tkconfigure(upBut, state = "normal")
    }

    doPack <- function (index, pack){
        whichOne <- as.numeric(index) + 1
        writeObj(listView, ls(pos = whichOne))
        isPack <<- TRUE
        writeCap(pack, asis = TRUE)
        tkconfigure(upBut, state = "normal")
    }

    doElse <- function(){
        # This a temp function for now. More checking will be implemented
    }

    doList <- function (aList){

        if(is.null(ncol(get(aList))))
            towrite <- c("Type: List",
                         paste("Length:", length(get(aList))))


        toWrite <- c("Type: List",
                     paste("Number of columns:", ncol(get(aList))),
                     paste("Number of row(s):", nrow(get(aList))),
                     paste("Column Name(s):"),names(get(aList)))

        writeObj(listView, toWrite)
        writeCap(aList)
    }

    dClick <- function (){
#        if(tkcurselection(listView) != ""){
        selectedObj <<- as.character(tkget(listView,
                                           tkcurselection(listView)))
        goin()
#        }
    }

    goin <- function (){
        if(!is.null(selectedObj)){
            if(regexpr("^package", selectedObj) > 0){
                objType <- "package"
            }else{
                objType <- typeof(get(selectedObj))
            }
            switch(objType,
               "environment" = doEnv(selectedObj),
               "package" =  doPack(tkcurselection(listView), selectedObj),
               "list" = doList(selectedObj),
               doElse()
            )
            tkconfigure(selectBut, state = "disabled")
            selectedObj <<- NULL
        }
    }


    sClick <- function () {
        selectedObj <<- NULL
#        selIndex <<- NULL
        tkconfigure(selectBut, state = "normal")
        selIndex <<- unlist(strsplit(tkcurselection(listView), " "))

#        for(i in selIndex){
#            temp <- as.character(tkget(listView, i))
#            objsInSel <<- c(objsInSel, temp)
#            selectedObj <<- c(selected
#        }
        if(length(selIndex) == 1){
            tempObj <<- as.character(tkget(listView, selIndex))
#            objType <- typeof(get(selectedObj))
        }else{
            for(i in selIndex){
                tempObj <<- c(tempObj,
                                  as.character(tkget(listView, i)))
            }

        }
    }


    getAct <- function(){
        selectedObj <<- ".GlobalEnv"
        goin()
    }


    up <- function(){
        options(show.error.messages = FALSE)
        tryMe <- try(parent.env(get(selectedObj)))
        options(show.error.messages = TRUE)

        if(isPack || selectedObj == ".GlobalEnv" ||
           inherits(tryMe, "try-error")){
            writeObj(listView, pickObjs(objNames = search(),
                                             fun = fun))
            writeCap("Top Level")
            tkconfigure(upBut, state = "disabled")
        }else{
            writeObj(listView,
                    pickObjs(objNames = ls(env = get(selectedObj)),
                                             fun = fun))
            writeCap(selectedObj)

        }
        tkconfigure(selectBut, state = "disabled")
    }

    selectObj <- function (){
#        if(length(selIndex) > 0){
#            for(i in selIndex){
#                selObj <- as.character(tkget(listView, i))
        objsInSel <<- c(objsInSel, tempObj)
#            }
#        }
        objsInSel <<- unique(objsInSel)
        writeSelection(objsInSel)
        tkconfigure(clearBut, state = "normal")
        tkconfigure(selectBut, state = "disabled")
    }

    clearSelection <- function(){
        objsInSel <<- NULL
        tkdelete(selectView, 0, "end")
        tkconfigure(clearBut, state = "disabled")
        tkconfigure(removeBut, state = "disabled")
        tkconfigure(selectBut, state = "disabled")
    }

    cancel <- function (){
        objsInSel <<- NULL
        end()
    }

    removeSelection <- function (){
        toRemove <- NULL
        if(length(objIndex) > 0){
            for(i in objIndex)
                toRemove <- c(toRemove, -(as.numeric(i) + 1))

            objsInSel <<- objsInSel[toRemove]
            writeSelection(objsInSel)
        }
        tkconfigure(removeBut, state = "disabled")
    }

    selClick <- function (){
        objIndex <<- NULL
        tkconfigure(removeBut, state = "normal")
        objIndex <<- unlist(strsplit(tkcurselection(selectView), " "))
    }

    writeSelection <- function (toWrite){
        tkdelete(selectView, 0, "end")
        for(i in toWrite)
            tkinsert(selectView, "end", i)
        fileIndex <<- NULL
    }

    writeCap <- function(objName, asis = FALSE){
        if(asis){
            tkconfigure(labl1, text = objName)
        }else{
            if(objName == "Top Level"){
                tkconfigure(labl1, text = "Top level")
            }else{
                tkconfigure(labl1, text = objName)
            }
        }
    }

    base <- tktoplevel()
    tktitle(base) <- paste("Bioconductor Object Browser")

    capFrame <- tkframe(base)
    noteLabel <- tklabel(capFrame, text = textToShow, font = LABELFONT1)
    labl1 <- tklabel(capFrame, text = " ", font = LABELFONT2)
    labl2 <- tklabel(capFrame, text = "Selected", font = LABELFONT2)
    dummyLabel <- tklabel(capFrame, text = "           ")
    tkgrid(noteLabel, columnspan = 3)
    tkgrid(labl1, dummyLabel, labl2)
    tkgrid(capFrame, columnspan = 2, padx = 10)

    leftFrame <- tkframe(base)

    listFrame <- tkframe(leftFrame)
    listView <- makeViewer(listFrame)
    tkgrid(listFrame, columnspan = 2)
    tkconfigure(listView, selectmode = "extended", font = LABELFONT2)
    tkbind(listView, "<Double-Button-1>", dClick)
    tkbind(listView, "<B1-ButtonRelease>", sClick)

    butFrame <- tkframe(leftFrame)
    upBut <- tkbutton(butFrame, text = "Up", width = BUTWIDTH,
		      command = up)
    activeBut <- tkbutton(butFrame, text = "Back", width = BUTWIDTH,
		      command = getAct)
    selectBut <- tkbutton(butFrame, text = "Select >>", width = BUTWIDTH,
                          command = selectObj, state = "disabled")
    tkgrid(upBut, selectBut)
    tkgrid(activeBut, columnspan = 2)
    tkgrid(butFrame)

    rightFrame <- tkframe(base)
    selectFrame <- tkframe(rightFrame)
    selectView <- makeViewer(selectFrame)
    tkconfigure(selectView, selectmode = "extended", font = LABELFONT2)
    tkgrid(selectFrame, columnspan = 2)
    tkbind(selectView, "<B1-ButtonRelease>", selClick)

    butFrame2 <- tkframe(rightFrame)
    removeBut <- tkbutton(butFrame2, text = "<< Remove", width = BUTWIDTH,
		      command = removeSelection, state = "disabled")
    clearBut <- tkbutton(butFrame2, text = "Clear", width = BUTWIDTH,
		      command = clearSelection, state = "disabled")
    canBut <- tkbutton(butFrame2, text = "Cancel", width = BUTWIDTH,
                       command = cancel)
    endBut <- tkbutton(butFrame2, text = "Finish", width = BUTWIDTH,
		       command = end)
    tkgrid(removeBut, clearBut)
    tkgrid(canBut, endBut)
    tkgrid(butFrame2)

    tkgrid(leftFrame, rightFrame)

    viewGlobalEnv()

    tkwait.window(base)

    return(returnList)
}















