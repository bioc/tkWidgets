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

createDPExplorer <- function(base, env){
    # The data frame contains a list box for the name of data sets in
    # a data package, a list box for the keys of a selected data set,
    # a list box for the content of a selected key, and a group of
    # buttons to allow users to cancel or end the process or clean the
    # selections. getData frame returns the id for the list box for
    # the names of data sets as the id is need for the entry box in
    # nameFrame to interate with
    getDataFrame(base, env)
    # The name frame contains a label, an entry box for users to entre
    # the name of a data package, and a "load" button to allow users to
    # load a data package whose name is entered in the entry box
    getNameFrame(base, env)
}

destroy <- function(env){
    tkdestroy(getBaseFromEnv(env))
}

getTopLevel <- function(title){
    base <- tktoplevel()
    tktitle(base) <- title
    return(base)
}

getDataFrame <- function(base, env){
    tempFrame <- tkframe(base)
    # Pack the list box for data set names
    dataNameList <- packDataNameList(tempFrame, env)
    # Pack the list box for keys
    keyList <- packKeyList(tempFrame, env)
    # Pack the list box fro values and three buttons
    valueList <- packValueNBut(tempFrame, env)
    tkpack(tempFrame, side = "bottom", expand = TRUE, fill = "both")
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
                        what = "text", side = "bottom")
    tkpack(keyLabel, side = "top")
    tkpack(tempFrame, side = "bottom", expand = TRUE, fill = "both")
    tkpack(keyListFrame, side = "left", expand = TRUE, fill = "both")
    assignKeyList(keyList, env)
}

packValueNBut <-function(base, env){
    cancel <- function(){
        destroy(getBaseFromEnv(env))
    }
    clear <- function(){
    }
    finish <- function(){
        destroy(getBaseFromEnv(env))
    }
    tempFrame <- tkframe(base)
    valueLabel <- tklabel(tempFrame, text = "Value(s)")
    temp <- tkframe(tempFrame)
    valueList <- makeViewer(temp, vWidth = NULL, vHeight = NULL,
                        hScroll = FALSE, vScroll = TRUE,
                        what = "list", side = "bottom")
    valueList <- tklistbox(tempFrame)
    butFrame <- tkframe(tempFrame)
    cancelBut <- tkbutton(butFrame, text = "Cancel", width = 8,
                          command = cancel)
    clearBut <- tkbutton(butFrame, text = "Clear", width = 8,
                         command = clear)
    finishBut <- tkbutton(butFrame, text = "Finish", width = 8,
                          comman = finish)
    tkpack(cancelBut, side = "left")
    tkpack(clearBut, side = "left")
    tkpack(finishBut, side = "left")
    tkpack(butFrame, side = "bottom")
    tkpack(temp, side = "bottom", expand = TRUE, fill = "both")
    tkpack(valueLabel, side = "bottom")
    tkpack(tempFrame, side = "bottom", expand = TRUE, fill = "both")
    assignValueList(valueList, env)
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
    tkpack(tempFrame, side = "bottom", expand = TRUE, fill = "x")
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

