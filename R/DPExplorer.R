# This function lists all the rda files in the data directory of a
# Bioconductor data package and thus allow users to explorer the data
# sets.
#
# Copyrihgt, 2003, J. Zhang, all rights reserved
#

DPExplorer <- function (pkgName = "", title = "BioC Data Package Explorer"){
    # Create a working environment
    workEnv <- new.env(hash = TRUE, parent = NULL)
    on.exit(destroy(base))
    base <- getTopLevel(title)
    createDPExplorer(base, pkgName, workEnv)

    # Initialize some of the variables
    assignSelected(NULL, workEnv)
    assignQuit(FALSE, workEnv)

    tkwait.window(base)

    if(getQuitFromEnv(workEnv)){
        return(NULL)
    }else{
        if(is.null(getSelectedFromEnv(workEnv))){
            return(NULL)
        }else{
            keys <- getSelectedFromEnv(workEnv)
            where <- grep(getPkgNameFromEnv(workEnv), search())
            if(length(keys) == 1){
                return(get(keys, get(getDataNameFromEnv(workEnv),
                                     pos = where)))
            }else{
                return(multiget(keys,
                                 get(getDataNameFromEnv(workEnv),
                                     pos = where)))
            }
        }
    }
}

assignQuit <- function(quit, env){
    assign("quit", quit, env)
}
getQuitFromEnv <- function(env){
    return(get("quit", env))
}

assignDataList <- function(winID, env){
    assign("dataList", winID, env)
}

getDataListFromEnv <- function(env){
    return(get("dataList", env))
}

assignKeyList <- function(keyList, env){
    assign("keyList", keyList, env)
}

getKeyListFromEnv <- function(env){
    return(get("keyList", env))
}

assignValueList <- function(valueList, env){
    assign("valueList", valueList, env)
}

getValueListFromEnv <- function(env){
    return(get("valueList", env))
}

assignSelectionList <- function(selectionList, env){
    assign("selectionList", selectionList, env)
}
getSelectionListFromEnv <- function(env){
    return(get("selectionList", env))
}

assignPkgName <- function(pkgName, env){
    assign("pkgName", pkgName, env)
}
getPkgNameFromEnv <- function(env){
    return(get("pkgName", env))
}

assignDataName <- function(dataName, env){
    assign("dataName", dataName, env)
}
getDataNameFromEnv <- function(env){
    return(get("dataName", env))
}

assignKey <- function(key, env){
    assign("key", key, env)
}
getKeyFromEnv <- function(env){
    return(get("key", env))
}

assignSelected <- function(selected, env){
    assign("selected", selected, env)
}
getSelectedFromEnv <- function(env){
    return(get("selected", env))
}

assignSelectBut <- function(tkWin, env){
    assign("selectBut", tkWin, env)
}
getSelectBut <-function(env){
    return(get("selectBut", env))
}

assignDropBut <- function(tkWin, env){
    assign("dropBut", tkWin, env)
}
getDropBut <- function(env){
    return(get("dropBut", env))
}

assignClearBut <- function(tkWin, env){
    assign("clearBut", tkWin, env)
}
getClearBut <- function(env){
    return(get("clearBut", env))
}

createDPExplorer <- function(base, pkgName, env){
    # The data frame contains a list box for the name of data sets in
    # a data package, a list box for the keys of a selected data set,
    # a list box for the content of a selected key, and a group of
    # buttons to allow users to cancel or end the process or clean the
    # selections. getData frame returns the id for the list box for
    # the names of data sets as the id is need for the entry box in
    # nameFrame to interate with
    dataFrame <- getDataFrame(base, env)
    # The name frame contains a label, an entry box for users to entre
    # the name of a data package, and a "load" button to allow users to
    # load a data package whose name is entered in the entry box
    nameFrame <- getNameFrame(base, pkgName, env)
    # The button frame contains a cancel and finish button
    butFrame <- getButFrame(base, env)

    tkpack(nameFrame, expand = TRUE, fill = "x", pady = 5, padx = 5)
    tkpack(dataFrame, expand = TRUE, fill = "both", padx = 5)
    tkpack(butFrame, expand = TRUE, fill = "x", pady = 5)
}

destroy <- function(base){
    tkdestroy(base)
}

getTopLevel <- function(title){
    base <- tktoplevel()
    tktitle(base) <- title
    return(base)
}

getDataFrame <- function(base, env){
    tempFrame <- tkframe(base)
    # Pack the list box for data set names
    packDataNameList(tempFrame, env)
    # Pack the list box for keys
    packKeyList(tempFrame, env)
    # Pack the list box fro values and three buttons
    packValueNBut(tempFrame, env)
    # Pack the list box for selected keys
    packSeletionList(tempFrame, env)

    return(tempFrame)
}

packDataNameList <- function(base, env){

    dataSelected <- function(){
        dataName <- as.character(tkget(dataNameList,
                                       tkcurselection(dataNameList)))
        where <- grep(getPkgNameFromEnv(env), search())
        keys <- ls(get(dataName, pos = where))
        writeList(getKeyListFromEnv(env), keys)
        assignDataName(dataName, env)
    }
    dataListFrame <- tkframe(base)
    dataLabel <- tklabel(dataListFrame, text = "Data Set:")
    tempFrame <- tkframe(dataListFrame)
    dataNameList <- makeViewer(tempFrame, vWidth = NULL, vHeight = NULL,
                        hScroll = FALSE, vScroll = TRUE,
                        what = "list", side = "bottom")
    tkbind(dataNameList, "<B1-ButtonRelease>", dataSelected)
    tkpack(dataLabel, side = "top")
    tkpack(tempFrame, side = "bottom", expand = TRUE, fill ="both")
    tkpack(dataListFrame, side = "left", expand = TRUE, fill = "both")
    assignDataList(dataNameList, env)
}

packKeyList <- function(base, env){

    keySelected <- function(){
        keyName <- as.character(tkget(keyList,
                                       tkcurselection(keyList)))
        where <- grep(getPkgNameFromEnv(env), search())
        values <- get(keyName, get(getDataNameFromEnv(env), pos = where))
        writeList(getValueListFromEnv(env), values)
        assignKey(keyName, env)
        tkconfigure(getSelectBut(env), state = "normal")
    }
    keyListFrame <- tkframe(base)
    keyLabel <- tklabel(keyListFrame, text = "Keys:")
    tempFrame <- tkframe(keyListFrame)
    keyList <- makeViewer(tempFrame, vWidth = NULL, vHeight = NULL,
                        hScroll = FALSE, vScroll = TRUE,
                        what = "list", side = "bottom")
    tkbind(keyList, "<B1-ButtonRelease>", keySelected)
    tkpack(keyLabel, side = "top")
    tkpack(tempFrame, side = "bottom", expand = TRUE, fill = "both")
    tkpack(keyListFrame, side = "left", expand = TRUE, fill = "both")
    assignKeyList(keyList, env)
}

packValueNBut <-function(base, env){
    select <- function(){
        selected <- unique(c(getSelectedFromEnv(env), getKeyFromEnv(env)))
        writeList(getSelectionListFromEnv(env), selected)
        assignSelected(selected, env)
        tkconfigure(selectBut, state = "disabled")
        tkconfigure(clearBut, state = "normal")
    }
    drop <- function(){
        selected <- getSelectedFromEnv(env)[getSelectedFromEnv(env)
                                            != getKeyFromEnv(env)]
        writeList(getSelectionListFromEnv(env), selected)
        assignSelected(selected, env)
        tkconfigure(dropBut, state = "disabled")
    }
    clear <- function(){
        assignSelected(NULL, env)
        writeList(getSelectionListFromEnv(env), NULL)
        tkconfigure(clearBut, state = "disabled")
    }
    tempFrame <- tkframe(base)
    valueLabel <- tklabel(tempFrame, text = "Value(s)")
    temp <- tkframe(tempFrame)
    valueList <- makeViewer(temp, vWidth = NULL, vHeight = NULL,
                        hScroll = FALSE, vScroll = TRUE,
                        what = "list", side = "bottom")
    selectBut <- tkbutton(tempFrame, text = "Select Key", width = 16,
                          command = select, state = "disabled")
    assignSelectBut(selectBut, env)
    dropBut <- tkbutton(tempFrame, text = "Drop Key", width = 16,
                         command = drop, state = "disabled")
    assignDropBut(dropBut, env)
    clearBut <- tkbutton(tempFrame, text = "Clear Selection", width = 16,
                          comman = clear, state = "disabled")
    assignClearBut(clearBut, env)
    tkpack(clearBut, side = "bottom", expand = TRUE, fill = "x")
    tkpack(dropBut, side = "bottom", expand = TRUE, fill = "x")
    tkpack(selectBut, side = "bottom", expand = TRUE, fill = "x")
    tkpack(temp, side = "bottom", expand = TRUE, fill = "both")
    tkpack(valueLabel, side = "bottom")
    tkpack(tempFrame, side = "left", expand = TRUE, fill = "both")
    assignValueList(valueList, env)
}

packSeletionList <- function(base, env){

    keySelected <- function(){
        keyName <- as.character(tkget(selectionList,
                                       tkcurselection(selectionList)))
        assignKey(keyName, env)
        tkconfigure(getDropBut(env), state = "normal")
    }

    selectionListFrame <- tkframe(base)
    selectionLabel <- tklabel(selectionListFrame, text = "Selected keys:")
    tempFrame <- tkframe(selectionListFrame)
    selectionList <- makeViewer(tempFrame, vWidth = NULL, vHeight = NULL,
                        hScroll = FALSE, vScroll = TRUE,
                        what = "list", side = "bottom")
    tkbind(selectionList, "<B1-ButtonRelease>", keySelected)
    tkpack(selectionLabel, side = "top")
    tkpack(tempFrame, side = "bottom", expand = TRUE, fill = "both")
    tkpack(selectionListFrame, side = "left", expand = TRUE, fill = "both")
    assignSelectionList(selectionList, env)
}

getButFrame <- function (base, env){
    cancel <- function(){
        assignQuit(TRUE, env)
        destroy(base)
    }
    finish <- function(){
        destroy(base)
    }

    butFrame <- tkframe(base)
    temp <- tkframe(butFrame)
    cancelBut <- tkbutton(temp, text = "Cancel", width = 8,
                          command = cancel)
    finishBut <- tkbutton(temp, text = "Finish", width = 8,
                          comman = finish)
    tkgrid(cancelBut, finishBut, padx = 10)
    tkpack(temp, side = "top", expand = TRUE, fill = "x")

    return(butFrame)
}

getNameFrame <- function(base, pkgName, env){
    loadDP <- function(){
        pkgName <- tclvalue(nameVar)
        if(pkgName == ""){
            tkmessageBox(title = "No Entry Error",
                         message = "Please enter a package name",
                         icon = "error",
                         type = "ok")
        }else{
            rdas <- loadDataPkg(pkgName, env)
            writeList(getDataListFromEnv(env), rdas)
        }
    }
    activeLoadBut <- function(){
        tkconfigure(loadBut, state = "normal")
    }
    if(pkgName != ""){
        nameVar <- tclVar(pkgName)
    }else{
        nameVar <- tclVar("")
    }
    tempFrame <- tkframe(base)
    label <- tklabel(tempFrame, text = "Data Package: ")
    entry <- tkentry(tempFrame, width = 15, textvariable = nameVar)
    tkbind(entry, "<KeyPress>", activeLoadBut)
    tkbind(entry, "<Return>", loadDP)
    loadBut <- tkbutton(tempFrame, width = 8, text = "Load",
                        command = loadDP, state = "disabled")
    tkpack(label, side = "left")
    tkpack(entry, side = "left", expand = TRUE, fill = "x")
    tkpack(loadBut, side = "left")

    if(pkgName != ""){
        print(paste("Loading data package ", pkgName, ".", sep = ""))
        loadDP()
        activeLoadBut()
    }

    return(tempFrame)
}

loadDataPkg <- function(pkgName, env){

    options(show.error.messages = FALSE)
    tryMe <- try(do.call("library", list(package = pkgName)))
    options(show.error.messages = TRUE)
    if(inherits(tryMe, "try-error")){
        tkmessageBox(title = "Invalid Package Name",
                         message = "The package name is not valid",
                     icon = "error",
                     type = "ok")
    }else{
        assignPkgName(pkgName, env)
        return(gsub("\\.rda$", "",
                    list.files(file.path(.path.package(pkgName),
                                         "data"), pattern = ".rda")))
        }
}

