# This function lists all the rda files in the data directory of a
# Bioconductor data package and thus allow users to explorer the data
# sets.
#
# Copyrihgt, 2003, J. Zhang, all rights reserved
#

DPExplorer <- function (title = "BioC Data Package Explorer", pkgName = ""){
    # Create a working environment
    workEnv <- new.env(hash = TRUE, parent = NULL)
    on.exit(destroy(base))
    base <- getTopLevel(title)
    assignBase(base, workEnv)
    createDPExplorer(base, workEnv)
    tkwait.window(base)
}

assignBase <- function(base, env){
    assign("topWin", base, env)
}

getBaseFromEnv <- function(env){
    return(get("topWin", env))
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
getSelectionList <- function(env){
    return(get("selectionList", env))
}

createDPExplorer <- function(base, env){
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
    nameFrame <- getNameFrame(base, env)
    # The button frame contains a cancel and finish button
    butFrame <- getButFrame(base, env)

    tkpack(nameFrame, expand = TRUE, fill = "x")
    tkpack(dataFrame, expand = TRUE, fill = "both")
    tkpack(butFrame, expand = TRUE, fill = "x")
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
        dataName <- tkget(dataNameList,(tkcurselection(dataNameList)))
        keys <- ls(dataName)
        showKeys(keys, getKeyListFromEnv(env))
    }
    dataListFrame <- tkframe(base)
    dataLabel <- tklabel(dataListFrame, text = "Data Set:")
    tempFrame <- tkframe(dataListFrame)
    dataNameList <- makeViewer(tempFrame, vWidth = NULL, vHeight = NULL,
                        hScroll = FALSE, vScroll = TRUE,
                        what = "list", side = "bottom")
    tkpack(dataLabel, side = "top")
    tkpack(tempFrame, side = "bottom", expand = TRUE, fill ="both")
    tkpack(dataListFrame, side = "left", expand = TRUE, fill = "both")
    assignDataList(dataNameList, env)
}

showKeys <- function(keys, base){

}
packKeyList <- function(base, env){
    keyListFrame <- tkframe(base)
    keyLabel <- tklabel(keyListFrame, text = "Keys:")
    tempFrame <- tkframe(keyListFrame)
    keyList <- makeViewer(tempFrame, vWidth = NULL, vHeight = NULL,
                        hScroll = FALSE, vScroll = TRUE,
                        what = "list", side = "bottom")
    tkpack(keyLabel, side = "top")
    tkpack(tempFrame, side = "bottom", expand = TRUE, fill = "both")
    tkpack(keyListFrame, side = "left", expand = TRUE, fill = "both")
    assignKeyList(keyList, env)
}

packValueNBut <-function(base, env){
    select <- function(){
    }
    drop <- function(){
    }
    clear <- function(){
    }
    tempFrame <- tkframe(base)
    valueLabel <- tklabel(tempFrame, text = "Value(s)")
    temp <- tkframe(tempFrame)
    valueList <- makeViewer(temp, vWidth = NULL, vHeight = NULL,
                        hScroll = FALSE, vScroll = TRUE,
                        what = "list", side = "bottom")
    selectBut <- tkbutton(tempFrame, text = "Select Key", width = 16,
                          command = select)
    dropBut <- tkbutton(tempFrame, text = "Drop Key", width = 16,
                         command = drop)
    clearBut <- tkbutton(tempFrame, text = "Clear Selection", width = 16,
                          comman = clear)
    tkpack(clearBut, side = "bottom", expand = TRUE, fill = "x")
    tkpack(dropBut, side = "bottom", expand = TRUE, fill = "x")
    tkpack(selectBut, side = "bottom", expand = TRUE, fill = "x")
    tkpack(temp, side = "bottom", expand = TRUE, fill = "both")
    tkpack(valueLabel, side = "bottom")
    tkpack(tempFrame, side = "left", expand = TRUE, fill = "both")
    assignValueList(valueList, env)
}

packSeletionList <- function(base, env){
    selectionListFrame <- tkframe(base)
    selectionLabel <- tklabel(selectionListFrame, text = "Selected keys:")
    tempFrame <- tkframe(selectionListFrame)
    selectionList <- makeViewer(tempFrame, vWidth = NULL, vHeight = NULL,
                        hScroll = FALSE, vScroll = TRUE,
                        what = "list", side = "bottom")
    tkpack(selectionLabel, side = "top")
    tkpack(tempFrame, side = "bottom", expand = TRUE, fill = "both")
    tkpack(selectionListFrame, side = "left", expand = TRUE, fill = "both")
    assignSelectionList(selectionList, env)
}

getButFrame <- function (base, env){
    cancel <- function(){
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

getNameFrame <- function(base, env){
    loadDP <- function(){
        pkgName <- tclvalue(nameVar)
        if(pkgName == ""){
            tkmessageBox(title = "No Entry Error",
                         message = "Please enter a package name",
                         icon = "error",
                         type = "ok")
        }else{
            rdas <- loadDataPkg(pkgName)
            writeList(getDataListFromEnv(env), rdas)
        }
    }

    nameVar <- tclVar("")
    tempFrame <- tkframe(base)
    label <- tklabel(tempFrame, text = "Data Package: ")
    entry <- tkentry(tempFrame, width = 15, textvariable = nameVar)
    loadBut <- tkbutton(tempFrame, width = 8, text = "Load", command = loadDP)
    tkpack(label, side = "left")
    tkpack(entry, side = "left", expand = TRUE, fill = "x")
    tkpack(loadBut, side = "left")

    return(tempFrame)
}

loadDataPkg <- function(pkgName){

    options(show.error.messages = FALSE)
    tryMe <- try(do.call("library", list(package = pkgName)))
    options(show.error.messages = TRUE)
    if(inherits(tryMe, "try-error")){
        tkmessageBox(title = "Invalid Package Name",
                         message = "The package name is not valid",
                     icon = "error",
                     type = "ok")
    }else{
        return(gsub("\\.rda$", "",
                    list.files(file.path(.path.package(pkgName),
                                         "data"), pattern = ".rda")))
        }
}

