# This widget allows users to explore an R package.
#

pExplorer <- function (pkgName = "", pkgPath = "", exclude = getExclude()){

    pkgNames <- list.files(file.path(R.home(), "library"))

    if(pkgName == ""){
        selection <- tclVar(pkgNames[1])
    }else{
        selection <- tclVar(pkgName)
    }
    if(pkgPath == ""){
        pkgPath <- .libPaths()[1]
        baseDir <- file.path(.libPaths()[1], tclvalue(selection))
    }else{
        baseDir <- file.path(pkgPath, tclvalue(selection))
    }

    curDir <- baseDir

    end <- function(){
        tkdestroy(base)
    }
    on.exit(end())
    writePkgDirs <- function(){
        writeList(listView, getPkgContents(pkgPath,
                                           tclvalue(selection), exclude))
        tkconfigure(contViewer, state = "normal")
        tkdelete(contViewer, "0.0", "end")
        tkconfigure(contViewer, state = "disabled")
    }
    # When a user double clicks an item in the list box,
    # populate the list box with new files if the item is a directory
    # or write the contents to the display window if the item is a
    dClick <- function(){
        selectedObj <- as.character(tkget(listView,
                                          (tkcurselection(listView))))
        if(regexpr(.Platform$file.sep, selectedObj) >= 1){
            curDir <<- file.path(curDir,
                                 gsub(.Platform$file.sep, "", selectedObj))
            writeList(listView, appendSepDir(curDir))
            tkconfigure(upBut, state = "normal")
        }else{
            popDisplay(getFileContents(file.path(curDir, selectedObj)))
        }
    }
    popDisplay <- function(contents){
        contents <- paste(contents, sep = "", collapse = "\n")
        tkconfigure(contViewer, state = "normal")
        writeText(contViewer, contents, TRUE)
        tkconfigure(contViewer, state = "disabled")
    }
    # When a single click is applied to an item in the list box, the
    # content of a file will be displayed. The action will be ignored
    # for directories
    sClick <- function(){
        selectedObj <- as.character(tkget(listView,
                                          (tkcurselection(listView))))
        if(regexpr(.Platform$file.sep, selectedObj) >= 1){
            curDir <<- file.path(curDir,
                                 gsub(.Platform$file.sep, "", selectedObj))
            writeList(listView, appendSepDir(curDir))
            tkconfigure(upBut, state = "normal")
        }else{
            popDisplay(getFileContents(file.path(curDir, selectedObj)))
        }
    }

    # Move the browser one level up the directory path
    goUp <- function(){
        curDir <<- gsub(paste(.Platform$file.sep,
                              basename(curDir), sep = ""), "", curDir)
        writeList(listView, appendSepDir(curDir))
        if(curDir == baseDir){
            tkconfigure(upBut, state = "disabled")
        }
        tkconfigure(contViewer, state = "normal")
        tkdelete(contViewer, "0.0", "end")
        tkconfigure(contViewer, state = "disabled")
    }
    tryExp <- function(){
        eExplorer(tclvalue(selection))
    }

    base <- tktoplevel()
    tktitle(base) <- paste("BioC Package Explorer")
#    pathFrame <- tkframe(base)
#    tkpack(tklabel(pathFrame, text = "Package path:"), side = "left",
#           expand = FALSE)
#    pathEntry <- tkentry(pathFrame, width = 20, textvariable = path)
#    tkpack(pathEntry, side = "left", expand = TRUE, fill = "x")
#    pathBut <- tkbutton(pathFrame, text = "Browse", width = 10,
#                        command = setPath)
#    tkpack(pathBut, side = "left", expand = FALSE)
#    tkpack(pathFrame, side = "top", expand = FALSE, fill = "x",
#           pady = 5, padx = 5)
    # A drop down list box that allows users to pick a package
    topFrame <- tkframe(base)
    tkpack(tklabel(topFrame, text = "Package to explore:"), side = "left",
           expand = FALSE)
    dropFrame <- tkframe(topFrame)
    dropdownList(dropFrame, pkgNames, selection, 20, tclvalue(selection))
    tkpack(dropFrame, side = "left", expand = TRUE, fill = "x")
    exBut <- tkbutton(topFrame, text = "Explore", width = 10,
                      command = writePkgDirs)
    tkpack(exBut, side = "left", expand = FALSE)
    tkpack(topFrame, side = "top", expand = FALSE, fill = "x",
           pady = 5, padx = 5)
    # Put the list box for package contents and associated buttons
    midFrame <- tkframe(base)
    leftFrame <- tkframe(midFrame)
    tkpack(tklabel(leftFrame, text = "Package contents"), side = "top",
           expand = FALSE, fill = "x")
    dirLFrame <- tkframe(leftFrame)
    listView <- makeViewer(dirLFrame, vWidth = 15, vHeight = 20,
                           hScroll = TRUE)
    tkpack(dirLFrame, side = "top", expand = TRUE, fill = "both")
    upBut <- tkbutton(leftFrame, text = "Back", width = 10,
                      state = "disabled", command = goUp)
    tkpack(upBut, side = "top", expand = FALSE, fill = "x")

    tkbind(listView, "<Double-Button-1>", dClick)
#    tkbind(listView, "<B1-ButtonRelease>", sClick)
    tkpack(leftFrame, side = "left", expand = TRUE, fill = "both")
    # Put the text box to show contents of a selected file
    rightFrame <- tkframe(midFrame)
    tkpack(tklabel(rightFrame, text = "Dispaly window"), side = "top",
           expand = FALSE, fill = "x")
    textFrame <- tkframe(rightFrame)
    contViewer <- makeViewer(textFrame, vWidth = 50, vHeight = 10,
                             vScroll = TRUE, hScroll = TRUE, what = "text")
    tkconfigure(contViewer, state = "disabled")
    tkpack(textFrame, side = "top", expand = TRUE, fill = "both")
    tkpack(rightFrame, side = "left", expand = TRUE, fill = "both")
    tkpack(midFrame, side = "top", expand = TRUE, fill = "both",
           padx = 5)
    # Put the end button
    butFrame <- tkframe(base)
    eExpBut <- tkbutton(butFrame, text = "Try examples", width = 12,
                    command = tryExp)
    tkpack(eExpBut, side = "left", expand = FALSE, padx = 5)
    endBut <- tkbutton(butFrame, text = "Finish", width = 12, command = end)
    tkpack(endBut, side = "left", expand = FALSE, padx = 5)
    tkpack(butFrame, pady = 5)

    if(pkgName != ""){
        writePkgDirs()
    }

    tkwait.window(base)

    return(invisible())
}

getPkgContents <- function(pkgPath, pkgName, exclude = getExclude()){
    pkgNames <- list.files(pkgPath, pkgName)
    if(!any(pkgNames == pkgName)){
        return(paste(pkgName, "is not in the R library"))
    }else{
        return(setdiff(appendSepDir(file.path(pkgPath, pkgName)),
                       exclude))
    }
}

getFileContents <- function(fileName){
    if(regexpr("\\.rda", fileName) > 0 ){
        load(fileName)
        return(capture.output(get(gsub("\\.rda", "",
                              basename(fileName)), parent.frame())))
    }
    if(regexpr("\\.gz", fileName) > 0){
        return(readLines(gzfile(fileName)))
    }
    if(regexpr("\\.zip", fileName) > 0){
        return(readLines(unz(fileName)))
    }
    # Ohterwise, use readLines
    return(readLines(fileName))
}

getExclude <- function(){
    return(c("Meta/", "html/", "latex/", "INDEX"))
}









