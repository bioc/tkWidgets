# This function creates a widget with a top frame that will filled
# with a widget passed as one of the arguments and bottom frame with
# buttons.
#
# Copyright 2002, J. Zhang. All right reserved.
#
dataViewer <- function(data, save = TRUE){

    base <- tktoplevel()
    tktitle(base) <- "BioC tkWidgets"

    exit <- function() tkdestroy(base)

    boxFrame <- tkframe(base)

    # Insert data into the canvas
    innerFrame <- makeViewer(boxFrame,
                             vScroll = TRUE, side = "top",
                             hScroll = TRUE, what = "canvas")

    dataFrame <- tkframe(innerFrame)
    columnLength <- numberChar(data)
    for(i in 1:ncol(data)){
        tempFrame <- tkframe(dataFrame)
        if(!is.null(colnames(data))){
            colWidth <- max(columnLength[i], nchar(colnames(data)[i]))
            tempName <- tkbutton(tempFrame, text = colnames(data)[i],
                                 width = colWidth - 3)
            tkpack(tempName)
        }
        tempList <- tklistbox(tempFrame, width = colWidth, height = 0,
                                             background = "white")
        writeList(tempList, data[,i])
        tkpack(tempList, side = "left", expand = TRUE, fill = "y")
        tkpack(tempFrame, side = "left")
    }
#    tkpack(dataFrame)
#    tkwindow.create(innerFrame, "0.0", window = dataFrame)

    tkcreate(innerFrame, "window", 0, 0, anchor = "nw", window = dataFrame)

    tkpack(boxFrame, side = "top")

    botFrame <- tkframe(base)
    if(save){
        save <- function(){
            name <- tclvalue(tkgetSaveFile())
            write.table(data, name, quote = FALSE, sep = "\t",
                        row.names = FALSE, col.names = FALSE)
            exit()
        }
        saveBut <- tkbutton(botFrame, text = "Save", width = 8,
                            command = save)
        tkpack(saveBut, side = "left")
    }
    exitBut <- tkbutton(botFrame, text = "Exit", width = 8, command = exit)
    tkpack(exitBut, side = "left")
    tkpack(botFrame, side = "top")

    tkwait.window(base)
}

