# This function provides the interface to view the objects in the
# work place.
#

objectBrowser <- function (fun = function(x) TRUE){

    require(tcltk) || stop("tcl/tk library not available")
    on.exit(options(show.error.messages = TRUE))

    LABELFONT <- "Helvetica 12"
    BUTWIDTH <- 8

    selectedObj <- NULL
    isPack <- FALSE
    returnObj <- NULL
    returnList <- NULL

    end <- function(){
        tkdestroy(base)
    }

    viewGlobalEnv <- function(){
        writeObj(listView, pickObjs(objNames = ls(env = get(".GlobalEnv")),
                                             fun = fun))
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
        # This a temp function for now. More checking will be inplemented
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
            objType <- findObjType(selectedObj)
            switch(objType,
               "environment" = doEnv(selectedObj),
               "package" =  doPack(tkcurselection(listView), selectedObj),
               "list" = doList(selectedObj),
               doElse()
            )
            tkconfigure(visitBut, state = "disabled")
            selectedObj <<- NULL
        }
    }


    sClick <- function () {
#        if(tkcurselection(listView) != ""){
            selectedObj <<- as.character(tkget(listView,
                                      tkcurselection(listView)))
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
                    "list" = tkconfigure(visitBut, state="normal")
                       )
                returnList <<- list("name" = selectedObj,
                                    "obj" = get(selectedObj))
            }
#        }
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

    viewGlobalEnv()

    tkwait.window(base)

    return(returnList)
}















