# This function provides the interface to browse objects in the work
# place and allows users to pick given number of objects to be
# returned.
# fun = a function that filters objects to be shown;
# textToShow = text to be shown as the title of the widget;
# nSelect = number of selection can be made (default to no limit).
#
#Copyright 2002, J. Zhang, all rights reserved
#

objectBrowser<- function (env = .GlobalEnv,
                          fun = noAuto, textToShow = "Select object(s)",
                          nSelect = -1){

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
    currentEnv <- env
    currentState <- "env"

    # close window
    end <- function(){
        tkgrab.release(base)
        tkdestroy(base)
    }
    on.exit(end(), add = TRUE)
    # Executed when a user clicks the end button. returnList that
    # contains names of selected objects will be updated before the
    # window closes.
    finish <- function(){
        if(currentState != "env"){
            returnList <<- objsInSel
            end()
        }else{
            if(length(objsInSel) != 0){
                if(nSelect == -1){
                    returnList <<- objNameToList(objsInSel, currentEnv)
                    end()
                }else{
                    if(nSelect != length(objsInSel)){
                        tkmessageBox(title = "Wrong number", message =
                         paste("You can only select", nSelect, "object(s)"),
                         icon = "warning", type = "ok")
                    }else{
                        returnList <<- objNameToList(objsInSel, currentEnv)
                        end()
                    }
                }
            }else{
                returnList <<- NULL
                end()
            }
        }
    }
    # Write the content of the global environment to the list box for
    # object names
    viewEnv <- function(env){
        writeList(listView, pickObjs(objNames = ls(env = env,
                                    all = TRUE), fun = fun), clear = TRUE)
        writeCap(substitute(env))
    }
    # Executed when a user double clicks an object that is an R
    # environment. List object names in an enviroment to the list
    # boxes for objects.
    doEnv <- function (item){
        writeList(listView,  pickObjs(objNames = ls(env = get(item)),
                                      fun = fun), clear = TRUE)
        writeCap(item)
        if(!is.null(parent.env(get(item))))
            tkconfigure(upBut, state = "normal")
    }
    # Executed when a user couble clicks an object that is an R
    # package. List all object names in the list box for objects.
    doPack <- function (index, pack){
        whichOne <- as.numeric(index) + 1
        writeList(listView, ls(pos = whichOne), clear = TRUE)
        isPack <<- TRUE
        writeCap(pack, asis = TRUE)
        tkconfigure(upBut, state = "normal")
    }
    # Will be done for other objects
    doElse <- function(data){
        if(is.vector(data)){
            doVector(data)
        }
    }
    # Executed when a user double clicks an ojbect that is a
    # list. Shows the number of columns, number of rows and column names.
    doList <- function (aList){
        currentState <<- "list"
        if(is.data.frame(get(aList))){
            toWrite <- c("Type: data frame",
                         paste("Number of columns:", ncol(get(aList))),
                         paste("Number of row(s):", nrow(get(aList))),
                         "Column name(s):",
                         colnames(get(aList)),
                         "Row name(s):",
                         row.names(get(aList)))
        }else{
            toWrite <- c("Type: list",
                         paste("Length:", length(get(aList))),
                         "Element name(s):",
                         names(get(aList)))
        }
        writeList(listView, toWrite, clear = TRUE)
        writeCap(aList)
        tkconfigure(selectBut, state = "disabled")
    }

    doVector <- function(vect){
        currentState <<- "vector"
        writeList(listView, get(vect), clear = TRUE)
    }

    doClosure <- function(code){
    }

    # Executed when a user double clicks an object name in a list box.
    # Shows the content of the object if there is any
    dClick <- function (){
        selectedObj <<- as.character(tkget(listView,
                                           tkcurselection(listView)))
        goin()
    }

    # Determines the type of an object that is double clicked to
    # decide what to do
    goin <- function (){
        if(!is.null(selectedObj)){
            if(regexpr("^package", selectedObj) > 0){
                objType <- "package"
            }else{
                objType <- typeof(get(selectedObj))
            }
            switch(objType,
               "closure" = doClosure(selectedObj),
               "environment" = doEnv(selectedObj),
               "package" =  doPack(tkcurselection(listView), selectedObj),
               "list" = doList(selectedObj),
               doElse(selectedObj)
            )
            tkconfigure(selectBut, state = "disabled")
            selectedObj <<- NULL
        }
    }

    # Response to a single click to an object name in the list box for
    # object names
    sClick <- function () {
        selectedObj <<- NULL
        if(currentState != "list"){
            tkconfigure(selectBut, state = "normal")
        }
        selIndex <<- unlist(strsplit(
                              as.character(tkcurselection(listView)), " "))
        if(length(selIndex) == 1){
            tempObj <<- as.character(tkget(listView, selIndex))
        }else{
            for(i in selIndex){
                tempObj <<- c(tempObj,
                                  as.character(tkget(listView, i)))
            }

        }
    }

    # When the global environment is clicked and shows the content
    getAct <- function(){
        currentState <<- "env"
        tkdelete(selectView, "0", "end")
        objsInSel <<- NULL
        selectedObj <<- ".GlobalEnv"
        goin()
        tkconfigure(selectBut, state = "disabled")
    }

    # Moves one step back the path of objects browsered
    up <- function(){
        options(show.error.messages = FALSE)
        tryMe <- try(parent.env(get(selectedObj)))
        options(show.error.messages = TRUE)

        if(isPack || selectedObj == ".GlobalEnv" ||
           inherits(tryMe, "try-error")){
            writeList(listView, pickObjs(objNames = search(),
                                             fun = fun), clear = TRUE)
            writeCap("Top Level")
            tkconfigure(upBut, state = "disabled")
        }else{
            writeList(listView,
                    pickObjs(objNames = ls(env = get(selectedObj)),
                                             fun = fun), clear = TRUE)
            writeCap(selectedObj)

        }
        tkconfigure(selectBut, state = "disabled")
    }
    # Writes the name of an object in the list box for object names to
    # the list box for selected objects.
    selectObj <- function (){
        objsInSel <<- c(objsInSel, tempObj)
        objsInSel <<- unique(objsInSel)
        writeSelection(objsInSel)
        tkconfigure(clearBut, state = "normal")
        tkconfigure(selectBut, state = "disabled")
    }
    # Removes everything from the list box for selected ojbects
    clearSelection <- function(){
        objsInSel <<- NULL
        tkdelete(selectView, 0, "end")
        tkconfigure(clearBut, state = "disabled")
        tkconfigure(removeBut, state = "disabled")
        tkconfigure(selectBut, state = "disabled")
    }
    # Destroy the widow and returns NULL
    cancel <- function (){
        objsInSel <<- NULL
        finish()
    }
    # Removes items from the list box for selected objects
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
    # When an object in the box for selected objects is clicked, the
    # index of the object name is remembered
    selClick <- function (){
        objIndex <<- NULL
        tkconfigure(removeBut, state = "normal")
        objIndex <<- unlist(strsplit(
                      as.character(tkcurselection(selectView)), " "))
    }
    # Write to the list box for selected objects
    writeSelection <- function (toWrite){
        writeList(selectView, toWrite, clear = TRUE)
        #tkdelete(selectView, 0, "end")
        #for(i in toWrite)
        #    tkinsert(selectView, "end", i)
    }
    # Writes to the top of the widget to indicate the current environment
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

    ## This portion sets up the interface
    base <- tktoplevel()
    tktitle(base) <- paste("Bioconductor Object Browser")
    # Set up the labels for list boxes of objects
    capFrame <- tkframe(base)
    noteLabel <- tklabel(capFrame, text = textToShow, font = LABELFONT1)
    labl1 <- tklabel(capFrame, text = " ", font = LABELFONT2)
    labl2 <- tklabel(capFrame, text = "Selected", font = LABELFONT2)
    dummyLabel <- tklabel(capFrame, text = "           ")
    tkgrid(noteLabel, columnspan = 3)
    tkgrid(labl1, dummyLabel, labl2)
    tkgrid(capFrame, columnspan = 2, padx = 10)
    # Sets up the list boxes. One for objects to browse and one for selected
    leftFrame <- tkframe(base)
    listFrame <- tkframe(leftFrame)
    listView <- makeViewer(listFrame)
    tkgrid(listFrame, columnspan = 2)
    tkconfigure(listView, selectmode = "extended", font = LABELFONT2)
    tkbind(listView, "<Double-Button-1>", dClick)
    tkbind(listView, "<B1-ButtonRelease>", sClick)
    # Puts in the buttons for the list box for object names
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
    # puts in the buttons for list box for selected objects
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
		       command = finish)
    tkgrid(removeBut, clearBut)
    tkgrid(canBut, endBut)
    tkgrid(butFrame2)

    tkgrid(leftFrame, rightFrame)

    viewEnv(env)

    tkgrab.set(base)

    tkwait.window(base)

    return(returnList)
}

