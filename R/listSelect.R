# This function makes a list of items for selection denoted by a yes
# and no radio button.
#
# Copyright 2002, J. Zhang. All rights reserved
#
listSelect <- function(aList,
                       topLbl = "Select Elements From The Following List",
                       typeFun = stdType, valueFun = stdView){

    require(tcltk) || stop("tcltk support is absent")

    returnList <- list()
    end <- FALSE

    if(is.null(aList) || length(aList) < 1)
        stop("Invalid input")

    for(i in names(aList))
        i <- tclVar(TRUE)

    clear <- function(){
        for(i in names(aList))
            tclvalue(i) <<- 1
    }

    cancel <- function(){
        tkdestroy(base)
    }

    finish <- function(){
        for(i in names(aList)){
            if(tclvalue(i) == 1)
                returnList[[i]] <<- TRUE
            else
                returnList[[i]] <<- FALSE
        }
        end <<- TRUE
        cancel()
    }

    butList <- list(Clear = clear, Cancel = cancel,
                    Finish = finish)

    base <- tktoplevel()
    tkwm.title(base,"BioC Widget")

    can <- tkcanvas(base, width = 300, height = 200)
    scr <- tkscrollbar(base, repeatinterval=5,
                       command=function(...)tkyview(can,...))
    tkconfigure(can, yscrollcommand=function(...)tkset(scr,...))

    topFrame <- tkframe(can)
    titlelbl <- tklabel(topFrame, text = topLbl, font = "Helvetica 12")
    tkpack(titlelbl, side = "top", fill = "both", expand = TRUE)
    selFrame <- tkframe(topFrame, borderwidth = 5)
    writeSelBox(selFrame, aList, typeFun, valueFun)
    tkpack(selFrame, side = "top")
    butFrame <- tkframe(topFrame, borderwidth = 5)
    writeBut(butFrame, butList)
    tkpack(butFrame, side = "top")
    tkcreate(can, "window", 5,5, anchor = "nw", window = topFrame)

    tkpack(can, side="left", fill="both", expand = TRUE)
    tkpack(scr, side="right", fill = "y", expand = TRUE)

    tkwait.window(base)

    if(end)
        return(returnList)
    else
        return(aList)
}

writeSelBox <- function(baseW, aList, typeFun = NULL, valueFun = NULL){

    LABELFONT <- "Helvetica 12"

    writeLabel(baseW, typeFun, valueFun)

    for (i in names(aList)){
        tempName <- tklabel(baseW, text = paste(i, ":", sep = ""),
                            font = LABELFONT, padx = 2)
        if(!is.null(typeFun))
            tempType <- tklabel(baseW,
                                text = eval(call(paste(quote(typeFun)),
                                eval(substitute(aList[[i]], list(i = i))))),
                                font = LABELFONT, padx = 2)
        else
            tempType <- tklabel(baseW, text = "")
        if(!is.null(valueFun)){
            fun <- function() {}
            body <- list(as.name("{"),
                         substitute(valueFun(aList[[i]]), list(i = i))

                         )
            body(fun) <- as.call(body)
            viewBut <- tkbutton(baseW, text = "View", command = fun)
        }
        else
            viewBut <- tklabel(baseW, text = "")
        tempCheck <-tkcheckbutton(baseW, text = "Accept", variable = i)
        tkgrid(tempName, tempType, viewBut, tempCheck)
        tclvalue(i) <- 1
    }
}

writeBut <- function(baseW, butList, butWidth = 6){

    butFrame <- tkframe(baseW, borderwidth = 5)
    for(i in 1:length(butList)){
        button <- tkbutton(butFrame, text= names(butList)[i],
                           width=butWidth, command = butList[[i]])
        tkpack(button, side = "left")
    }
    tkgrid(butFrame, columnspan = 4)
}

writeLabel <- function(baseW, typeFun, valueFun){
    LABELFONT <- "Helvetica 12"
    name <- tklabel(baseW, text = "Name", font = LABELFONT, padx = 2)
    if(!is.null(typeFun))
        type <- tklabel(baseW, text = "Type", font = LABELFONT, padx = 2)
    else
        type <- tklabel(baseW, text = "", font = LABELFONT, padx = 2)
    if(!is.null(valueFun))
        view <- tklabel(baseW, text = "Value", font = LABELFONT, padx = 2)
    else
        view <- tklabel(baseW, text = "", font = LABELFONT, padx = 2)
    option <- tklabel(baseW, text = "Option", font = LABELFONT, padx = 2)

    tkgrid(name, type, view, option)
}









