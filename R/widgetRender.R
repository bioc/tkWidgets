widgetRender <- function (iWidget, tkTitle) {
    LABELFONT <- "Helvetica 12"
    ENTRYWIDTH <- 50
    BUTWIDTH <- 10

    savediWidget <- iWidget
    wList <- iWidget$wList

    if(is.null(wList) || is.na(wList) )
        stop("Either wList or/and funName is null or empty")

    if(!is.null(WpreFun(iWidget)))
        WpreFun(iWidget)
    if(!is.null(WpostFun(iWidget)))
        on.exit(WpostFun(iWidget))

    require(tcltk) || stop("tcltk is not available.")

    CANCEL <- FALSE
    END <- FALSE
    cancel <- function() {
        CANCEL <<- FALSE
        rval <<- savediWidget
        tkdestroy(base)
    }
    end <- function() {
        END <<- FALSE
        tkdestroy(base)
    }

    base <- tktoplevel()
    tktitle(base) <- tkTitle

    eFrame <- tkframe(base)

    ##build button functions
    for(i in 1:length(wList)) {
        fun <- function() {}
        body <- list(as.name("{"),
                     substitute(rval <- WbuttonFun(wList[[i]])(),
                                     list(i=i)),
                     substitute(text <- WtoText(wList[[i]])(rval),
                                list(i=i)),
                     substitute(tkdelete(entryList[[i]], 0, "end"),
                                list(i=i)),
                     substitute(tkinsert(entryList[[i]], 0, text),
                                list(i=i)),
                     substitute(wList[[i]]$Value <<- rval, list(i=i))
                          )

        body(fun) <- as.call(body)
        assign(paste("funList",i,sep=""), fun)
    }

    ##initialize the buttons/boxes
    entryList <- vector("list", length = length(wList))
    for(i in 1:length(wList) ) {
        pW <- wList[[i]]

        label <- tklabel(eFrame, text = WName(pW), font = LABELFONT)

        entryList[[i]] <- tkentry(eFrame, width=ENTRYWIDTH)

        if( !is.null(WValue(pW))){
            tkinsert(entryList[[i]], 0, WValue(pW))
        }

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
    return(list(wList))
}

