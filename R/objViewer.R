# This function creates a widget with a scrollable text box to show
# R object passed in the text box in a predefined format.
#
# Copyright 2002, J. Zhang. All right reserved.
#
objViewer <- function(toView, width = 40, height = 10){

    ok <- function() tkdestroy(base)

    base <- tktoplevel()
    tktitle(base) <- "Bioconductor Object Viewer"

    boxFrame <- tkframe(base)
    boxView <- makeViewer(boxFrame, vWidth = width,
                        vHeight = height, vScroll = TRUE,
                        hScroll = TRUE)
    writeObj(boxView, toView)
    tkpack(boxFrame, side = "top")

    okBut <- tkbutton(base, text = "OK", command = ok)
    tkpack(okBut,side = "bottom")

    tkwait.window(base)
}
