# This function lists all the rda files in the data directory of a
# Bioconductor data package and thus allow users to explorer the data
# sets.
#
# Copyrihgt, 2003, J. Zhang, all rights reserved
#

##FIXME: the selection of keys for returning needs to grab the environment
## as well, right now we merely try to use the last environment selected
## and that is not quite what we would like
DPExplorer <- function (pkgName = "", title = "BioC Data Package Explorer",
                        getFocus = TRUE){

    quit <- FALSE
    dataName <- NULL
    keyName <- NULL
    keySelection <- NULL

    currentList <- list()

    if(typeof(pkgName) != "character"){
        tkmessageBox(title = "Argument Error",
                         message = "Package name must be a character string",
                         icon = "error",
                         type = "ok")
        stop()
    }

    loadDP <- function(){
        pkgName <<- tclvalue(nameVar)
        if(pkgName == ""){
            tkmessageBox(title = "No Entry Error",
                         message = "Please enter a package name",
                         icon = "error",
                         type = "ok")
        }else{
            rdas <- loadDataPkg(pkgName)
            if(!is.null(rdas)){
                writeList(dataNameList, rdas)
            }
        }
    }
    activeLoadBut <- function(){
        tkconfigure(loadBut, state = "normal")
    }
    dataSelected <- function(){
        dName <- as.character(tkget(dataNameList,
                                       tkcurselection(dataNameList)))
        if(!is.environment(get(dName, pos = grep(pkgName, search())))){
            writeList(valueList, sort(capture.output(get(dName))))
        }else{
            dataName <<- dName
            currentList <<- as.list(get(dataName,
                                        pos = grep(pkgName, search())))
            #keys <- ls(get(dataName, pos = grep(pkgName, search())))
            writeList(keyList, sort(names(currentList)))
            tkdelete(valueList, "0", "end")
        }
    }
    keySelected <- function(){
        keyName <<- paste(as.character(tkget(keyList,
                                       tkcurselection(keyList))),
                          sep = "", collapse = " ")
        #values <- get(keyName, get(dataName, pos = grep(pkgName, search())))
        #writeList(valueList, values)
        writeList(valueList, currentList[[keyName]])
        tkconfigure(selectBut, state = "normal")
    }
    select <- function(){
        keySelection <<- unique(c(keySelection, keyName))
        writeList(selectionList, keySelection)
        tkconfigure(selectBut, state = "disabled")
        tkconfigure(clearBut, state = "normal")
    }
    drop <- function(){
        keySelection <<- keySelection[keySelection != keyName]
        writeList(selectionList, keySelection)
        tkconfigure(dropBut, state = "disabled")
    }
    clear <- function(){
        keySelection <<- NULL
        writeList(selectionList, NULL)
        tkconfigure(clearBut, state = "disabled")
    }
    selectionSelected <- function(){
        keyName <<- as.character(tkget(selectionList,
                                       tkcurselection(selectionList)))
        tkconfigure(dropBut, state = "normal")
    }
    cancel <- function(){
        quit <- TRUE
        if(getFocus){
            tkgrab.release(base)
        }
        tkdestroy(base)
    }
    on.exit(cancel())
    finish <- function(){
        tkdestroy(base)
    }

    if(pkgName != ""){
        nameVar <- tclVar(pkgName)
    }else{
        nameVar <- tclVar("")
    }

    base <- getTopLevel(title)
    # Pack the top frame with an entry button for package name
    nameFrame <- tkframe(base)
    label <- tklabel(nameFrame, text = "Data Package: ")
    entry <- tkentry(nameFrame, width = 15, textvariable = nameVar)
    tkbind(entry, "<KeyPress>", activeLoadBut)
    tkbind(entry, "<Return>", loadDP)
    loadBut <- tkbutton(nameFrame, width = 8, text = "Load",
                        command = loadDP, state = "disabled")
    tkpack(label, side = "left")
    tkpack(entry, side = "left", expand = TRUE, fill = "x")
    tkpack(loadBut, side = "left")
    tkpack(nameFrame, expand = FALSE, fill = "x", pady = 5, padx = 5)
    # Pack the frame with lists for data
    dataFrame <- tkframe(base)
    # Frame for a list for names of data sets
    dataListFrame <- tkframe(dataFrame)
    dataLabel <- tklabel(dataListFrame, text = "Data:")
    tempFrame <- tkframe(dataListFrame)
    dataNameList <- makeViewer(tempFrame, vWidth = NULL, vHeight = NULL,
                        hScroll = TRUE, vScroll = TRUE,
                        what = "list", side = "bottom")
    tkconfigure(dataNameList, exportselection = FALSE)
    tkbind(dataNameList, "<B1-ButtonRelease>", dataSelected)
    tkpack(dataLabel, side = "top")
    tkpack(tempFrame, side = "bottom", expand = TRUE, fill ="both")
    tkpack(dataListFrame, side = "left", expand = FALSE, fill = "y")
    # Frame for a list for names of keys
    keyListFrame <- tkframe(dataFrame)
    keyLabel <- tklabel(keyListFrame, text = "Keys:")
    tempFrame <- tkframe(keyListFrame)
    keyList <- makeViewer(tempFrame, vWidth = 13, vHeight = 15,
                        hScroll = TRUE, vScroll = TRUE,
                        what = "list", side = "bottom")
    tkconfigure(keyList, exportselection = FALSE)
    tkbind(keyList, "<B1-ButtonRelease>", keySelected)
    tkpack(keyLabel, side = "top")
    tkpack(tempFrame, side = "bottom", expand = TRUE, fill = "both")
    tkpack(keyListFrame, side = "left", expand = FALSE, fill = "y")
    # Frame for a list for displaying values of selected keys
    valueFrame <- tkframe(dataFrame)
    valueLabel <- tklabel(valueFrame, text = "Value(s)")
    temp <- tkframe(valueFrame)
    valueList <- makeViewer(temp, vWidth = 25, vHeight = 15,
                        hScroll = TRUE, vScroll = TRUE,
                        what = "list", side = "bottom")
    selectBut <- tkbutton(valueFrame, text = "Select Key", width = 16,
                          command = select, state = "disabled")
    dropBut <- tkbutton(valueFrame, text = "Drop Key", width = 16,
                         command = drop, state = "disabled")
    clearBut <- tkbutton(valueFrame, text = "Clear Selection", width = 16,
                          comman = clear, state = "disabled")
    tkpack(clearBut, side = "bottom", expand = FALSE, fill = "x")
    tkpack(dropBut, side = "bottom", expand = FALSE, fill = "x")
    tkpack(selectBut, side = "bottom", expand = FALSE, fill = "x")
    tkpack(temp, side = "bottom", expand = TRUE, fill = "both")
    tkpack(valueLabel, side = "bottom")
    tkpack(valueFrame, side = "left", expand = TRUE, fill = "both")
    # Frame for a list for selected keys
    selectionListFrame <- tkframe(dataFrame)
    selectionLabel <- tklabel(selectionListFrame, text = "Selected keys:")
    viewFrame <- tkframe(selectionListFrame)
    selectionList <- makeViewer(viewFrame, vWidth = 13, vHeight = 15,
                        hScroll = FALSE, vScroll = TRUE,
                        what = "list", side = "bottom")
    tkbind(selectionList, "<B1-ButtonRelease>", selectionSelected)
    tkpack(selectionLabel, side = "top")
    tkpack(viewFrame, side = "bottom", expand = TRUE, fill = "both")
    tkpack(selectionListFrame, side = "left", expand = FALSE, fill = "y")
    # Pack the frame for data lists
    tkpack(dataFrame, expand = TRUE, fill = "both", padx = 5)
    # Frame for the buttons
    butFrame <- tkframe(base)
    cancelBut <- tkbutton(butFrame, text = "Cancel", width = 8,
                          command = cancel)
    finishBut <- tkbutton(butFrame, text = "Finish", width = 8,
                          comman = finish)
    tkgrid(cancelBut, finishBut, padx = 10)
    tkpack(butFrame, side = "top", expand = FALSE, fill = "x")

    if(pkgName != ""){
        loadDP()
        activeLoadBut()
    }

    if(getFocus){
        tkgrab.set(base)
    }
    tkwait.window(base)

    if(quit){
        return(NULL)
    }else{
        if(is.null(keySelection)){
            return(NULL)
        }else{
            if(length(keySelection) == 1){
                return(get(keySelection,
                           get(dataName, pos = grep(pkgName, search()))))
            }else{
                return(mget(keySelection,
                            get(dataName, pos = grep(pkgName,
                                          search())), ifnotfound=NA))

            }
        }
    }
}

getTopLevel <- function(title){
    base <- tktoplevel()
    tktitle(base) <- title
    return(base)
}


##fixme: surely this should do something smarter
loadDataPkg <- function(pkgName){
    where <- do.call(require, list(pkgName, quietly=TRUE))
    if(!where){
        tkmessageBox(title = "Invalid Package Name",
                         message = paste("Package", pkgName,
                         "is not valid or has not been loaded yet!"),
                     icon = "error",
                     type = "ok")
        return(NULL)
    }else{
        rdas <- gsub("\\.rda$", "", ls(paste("package:", pkgName, sep="")))
        rdas <- setdiff(rdas, c(pkgName,
                                     paste(pkgName, "QC", sep = "")))
        return(rdas)
    }
}

