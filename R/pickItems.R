# This function provides the interface for inputing query parameters
# for querying a table in a given database. Interface between R and
# the underlaying database management system is through Rdbi.
#
# items - a vector of character strings for the items to be picked
# from.
#
# Copyright 2002 J. Zhang, all rights reserved
#
pickItems <- function (items){

    on.exit(end())

    columns <- NULL
    colIndex <- NULL
    indexInSel <- NULL
    text2 <- "Select item(s) from the list box on the left"

    end <- function(){
        tkdestroy(base)
    }

    # When a user double clicks a column name in the list box, put the
    # library name in the vector for selected SAGE libraries.
    colDClicked <- function(){
        column <- as.character(tkget(colView,(tkcurselection(colView))))
        columns <<- unique(c(columns, column))
        writeList(selectView, columns)
        tkconfigure(clearBut, state = "normal")
        tkconfigure(selectBut, state = "disabled")
    }
    # When a user single clicked a column name, remember that name and
    # activate the select button
    colSClicked <- function(){
        colIndex <<- unlist(strsplit(tkcurselection(colView), " "))
        tkconfigure(selectBut, state = "normal")
    }
    # When a user click the select button, put the selected column name
    # in the list box for selected columns
    selectCol <- function(){
        for(i in colIndex){
            columns <<- c(columns,
                          as.character(tkget(colView, i)), sep = "")
        }
        writeList(selectView, unique(columns))
        tkconfigure(selectBut, state = "disabled")
        tkconfigure(clearBut, state = "normal")
    }
    # Remember the column name when a name in the selected column names list
    # box is single clicked
    selSClick <- function(){
        indexInSel <<- unlist(strsplit(tkcurselection(selectView), " "))
        tkconfigure(remBut, state = "normal")
    }
    # Removes the selected column name from the list box for selected
    # column names
    dropSelCol <- function(){
        for(i in indexInSel){
            columns <<- columns[columns != as.character(tkget(selectView, i))]
        }
        writeList(selectView, columns)
        tkconfigure(remBut, state = "disabled")
    }
    # Remove everything from the list box for selected column names
    clearSelCol <- function(){
        writeList(selectView, "")
        columns <<- NULL
        tkconfigure(clearBut, state = "disabled")
    }

    base <- tktoplevel()
    tktitle(base) <- "BioC tkWidgets"

    # Lists for column names
    midFrame <- tkframe(base)
    text2Label <- tklabel(midFrame, text = text2, font = "Helvetica 12")
    tkgrid(text2Label, columnspan = 2, pady = 2)
    # Label for available SAGE libs
    leftFrame <- tkframe(midFrame)
    label1 <- tklabel(leftFrame, text = "Items to pick",
                        font = "Helvetica 11")
    tkpack(label1)
    # List box showing the available SAGE libs
    colFrame <- tkframe(leftFrame)
    colView <- makeViewer(colFrame, vWidth = 40, vHeight = 15,
                           hScroll = TRUE)
    tkbind(colView, "<Double-Button-1>", colDClicked)
    tkbind(colView, "<B1-ButtonRelease>", colSClicked)
    tkpack(colFrame, padx = 5)
    selectBut <- tkbutton(leftFrame, text = "Select >>", width = 12,
		      state = "disabled", command = selectCol)
    tkpack(selectBut)
    tkconfigure(colView, selectmode = "extended")
    # Put the list box for selected SAGE libs and the associated buttons
    rightFrame <- tkframe(base)
    label2 <- tklabel(rightFrame, text = "Picked items",
                        font = "Helvetica 11")
    tkgrid(label2, columnspan = 2)
    selLFrame <- tkframe(rightFrame)
    selectView <- makeViewer(selLFrame, vWidth = 40, vHeight = 15,
                             hScroll = TRUE)
    tkgrid(selLFrame, columnspan = 2, padx = 5)
    tkconfigure(selectView, selectmode = "extended")
    tkbind(selectView, "<B1-ButtonRelease>", selSClick)
    remBut <- tkbutton(rightFrame, text = "<< Remove", width = 12,
		      state = "disabled", command = dropSelCol)
    clearBut <- tkbutton(rightFrame, text = "Clear", width = 12,
		      state = "disabled", command = clearSelCol)
    tkgrid(remBut, clearBut)
    tkgrid.configure(remBut, sticky = "e")
    tkgrid.configure(clearBut, sticky = "w")

    tkgrid(leftFrame, rightFrame)
    tkpack(midFrame)

    # Put the end button
    endBut <- tkbutton(base, text = "Finish", width = 12, command = end)
    tkpack(endBut, pady = 5)

    writeList(colView, items)

    tkwait.window(base)
    return(columns)
}









