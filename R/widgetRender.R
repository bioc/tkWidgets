# This function creates a widget based on the requirements passed as a
# list of lists.
# iWidget: a list of lists defining the appearance and behavour of the
# primitive widgets such entry boxex, buttons, ...
# tkTitle: text to be appear in the title bar of the widgets.
#
widgetRender <- function (iWidget, tkTitle) {
    LABELFONT <- "Helvetica 12"
    ENTRYWIDTH <- 50
    BUTWIDTH <- 10
    BUTTONLAST <- NULL

    savediWidget <- iWidget
    wList <- WwList(iWidget)

    if(is.null(wList) || is.na(wList) )
        stop("Either wList or/and funName is null or empty")
    require(tcltk) || stop("tcltk is not available.")

    if(!is.null(WpreFun(iWidget)))
        tt <- eval(WpreFun(iWidget)())
    if(!is.null(WpostFun(iWidget)))
        on.exit(eval(WpostFun(iWidget)()))

    PFRAME <- parent.frame(1)
    CANCEL <- FALSE
    END <- FALSE
    cancel <- function() {
        CANCEL <<- TRUE
        tkdestroy(base)
    }
    end <- function() {
        END <<- TRUE
        tkdestroy(base)
    }

    base <- tktoplevel()
    tktitle(base) <- tkTitle
    eFrame <- tkframe(base)

    ## function that gets called at the end and updates the
    ## values of iWidget using data from the entry boxes
    getEntryValues <- function(){
        for(i in 1:length(wList) ) {
            if(!BUTTONLAST[[i]]){
                if(!is.null(WfromText(wList[[i]]))){
#                     eval(substitute(WLValue(iWidget, i) <<-
#                                     WfromText(wList[[i]])
#                                     (tclvalue(entryValue[[i]])),
#                                     list(i = i)))
                     WLValue(iWidget, i) <<- WfromText(wList[[i]])
                                     (tclvalue(entryValue[[i]]))
                }else{
                     WLValue(iWidget, i) <<- tclvalue(entryValue[[i]])
                }
            }
        }
    }

    ##build button functions
    for(i in 1:length(wList)) {
        fun <- function() {}
        body <- list(as.name("{"),
                     substitute(rval <-
                                eval(as.call(list(WbuttonFun(wList[[j]]))),
                                             env=PFRAME),
                                     list(j=i)),
                     substitute(mytext <- WtoText(wList[[j]])(rval),
                                list(j=i)),

                     substitute(if(!WcanEdit(wList[[j]]))
                                tkconfigure(entryList[[j]],state = "normal"),
                                list(j=i)),
                     substitute(tkdelete(entryList[[j]], 0, "end"),
                                list(j=i)),
                     substitute(tkinsert(entryList[[j]], 0, mytext),
                                list(j=i)),
                     substitute(if(!WcanEdit(wList[[j]]))
                                tkconfigure(entryList[[j]],
                                            state = "disabled"),
                                list(j=i)),

                     substitute(BUTTONLAST[[j]] <<- TRUE, list(j=i)),

                     substitute(WLValue(iWidget, j) <<- rval,
                                list(j = i))
                     )

        body(fun) <- as.call(body)
        assign(paste("funList",i,sep=""), fun)
    }

    ##initialize the buttons/boxes
    entryList <- vector("list", length = length(wList))
    entryValue <- vector("list", length = length(wList))
    for(i in 1:length(wList) ) {
        pW <- wList[[i]]
        label <- tklabel(eFrame, text = WName(pW), font = LABELFONT)
        entryValue[[i]] <- tclVar()
        BUTTONLAST[[i]] <- TRUE

        entryList[[i]] <- tkentry(eFrame,
                                  textvariable = entryValue[[i]],
                                  width=ENTRYWIDTH)

        eval(substitute(tkbind(entryList[[i]], "<KeyPress>",
               kpress <- function() BUTTONLAST[[i]] <<- FALSE), list(i = i)))

        if( !is.null(WValue(pW))){
            tkinsert(entryList[[i]], 0, WValue(pW))
        }

        if(!WcanEdit(wList[[i]]))
           tkconfigure(entryList[[i]], state = "disabled")

        if(is.null(WbuttonFun(pW)))
            browse <- tklabel(eFrame, text = "  ")
        else
            browse <- tkbutton(eFrame, text= WbuttonText(pW),
                               width=BUTWIDTH,
                               command=get(paste("funList",i,sep="")))
        tkgrid(label, entryList[[i]], browse)
    }

    tkpack(eFrame)

    butFrame <- tkframe(base)
    cancelBut <- tkbutton(butFrame, text = "Cancel",
                  width = BUTWIDTH, command = cancel)
    doneBut <- tkbutton(butFrame, text = "Done",
                  width = BUTWIDTH, command = end)
    tkgrid(doneBut, cancelBut)
    tkpack(butFrame)
    tkwait.window(base)

    if(END){
        getEntryValues()
        iWidget$end <- "END"
        class(iWidget) <- "Widget"
        return(iWidget)
    }else{
        savediWidget$end <- "CANCEL"
        class(savediWidget) <- "Widget"
        return(savediWidget)
    }
}















