# This widget allows users to explore an R package.
#

pExplorer <- function (pkgName = "", pkgPath = "", exclude = getExclude()){

    if(pkgPath == ""){
        pkgPaths <- .libPaths()
    }else{
        pkgPaths <- c(pkgPath, .libPaths())
    }
    if(pkgName == ""){
        pkgPath <- tclVar(pkgPaths[1])
        pkgName <- tclVar(list.files(tclvalue(pkgPath))[1])
        pkgNames <- list.files(tclvalue(pkgPath))
    }else{
        pkgPath <- tclVar(file.path(pkgPaths[1], pkgName))
        pkgName <- tclVar(pkgName)
        pkgNames <- pkgName
    }

    curDir <- file.path(tclvalue(pkgPath), tclvalue(pkgName))

    end <- function(){
        tkdestroy(base)
    }
    on.exit(end())
    upDatePath <- function(){
        options(show.error.messages = FALSE)
        opt <- try(getListOption(pathEntry, pkgPaths))
        options(show.error.messages = TRUE)
        if(!inherits(opt, "try-error")){
            writeList(pathEntry, opt, clear = TRUE)
            tclvalue(pkgPath) <<- opt
            pkgNames <<- list.files(opt)
            tclvalue(pkgName) <<- pkgNames[1]
            curDir <<- file.path(opt, pkgNames[1])
            writePkgDirs()
        }
    }
    browse <- function(){
        tclvalue(pkgPath) <<- tkchooseDirectory()
        pkgNames <<- list.files(tclvalue(pkgPath))
        tclvalue(pkgName) <<- pkgNames[1]
        curDir <<- file.path(tclvalue(pkgPath), tclvalue(pkgName))
        writePkgDirs()
    }
    writePkgDirs <- function(){
        writeList(listView, getPkgContents(curDir, getExclude()))
        tkconfigure(contViewer, state = "normal")
        tkdelete(contViewer, "0.0", "end")
        tkconfigure(contViewer, state = "disabled")
    }
    pkgSelect <- function(){
        options(show.error.messages = FALSE)
        opt <- try(getListOption(pkgEntry, pkgNames))
        options(show.error.messages = TRUE)
        if(!inherits(opt, "try-error")){
            writeList(pkgEntry, opt, clear = TRUE)
            tclvalue(pkgName) <<- opt
            curDir <<- file.path(tclvalue(pkgPath), opt)
            writePkgDirs()
        }
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
            writeList(listView, getPkgContents(curDir, getExclude()))
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

    # Move the browser one level up the directory path
    goUp <- function(){
        curDir <<- gsub(paste(.Platform$file.sep,
                              basename(curDir), sep = ""), "", curDir)
        writeList(listView, getPkgContents(curDir, getExclude()))
        if(curDir == file.path(tclvalue(pkgPath), tclvalue(pkgName))){
            tkconfigure(upBut, state = "disabled")
        }
        tkconfigure(contViewer, state = "normal")
        tkdelete(contViewer, "0.0", "end")
        tkconfigure(contViewer, state = "disabled")
    }
    tryExp <- function(){
        eExplorer(tclvalue(pkgName))
    }

    base <- tktoplevel()
    tktitle(base) <- paste("BioC Package Explorer")
    # A drop down list box that allows users to pick a package
    pathFrame <- tkframe(base)
    tkpack(tklabel(pathFrame, text = "Package path:"), side = "left",
           expand = FALSE)
    dropFrame <- tkframe(pathFrame, borderwidth = 2, relief = "sunken")
    pathEntry <- tkentry(dropFrame, width = 40, textvariable = pkgPath,
                     borderwidth = 1)
    tkpack(pathEntry, side = "left", expand = TRUE, fill = "both")
    pathDropBut <- tkbutton(dropFrame, width = 1, text = "v", font = "bold",
                        command = upDatePath)
    tkpack(pathDropBut, side = "left", expand = FALSE)
    tkpack(dropFrame, side = "left", expand = TRUE, fill = "x")
    browseBut <- tkbutton(pathFrame, text = "Browse", width = 6,
                      command = browse)
    tkpack(browseBut, side = "left", padx = 5, expand = FALSE)
    tkpack(pathFrame, side = "top", expand = FALSE, fill = "x",
           pady = 5, padx = 5)

    pkgFrame <- tkframe(base)
    tkpack(tklabel(pkgFrame, text = "Package to explore:"), side = "left",
           expand = FALSE)
    pkgDFrame <- tkframe(pkgFrame, borderwidth = 2, relief = "sunken")
    pkgEntry <- tkentry(pkgDFrame, width = 40, textvariable = pkgName,
                     borderwidth = 1)
    tkpack(pkgEntry, side = "left", expand = TRUE, fill = "both")
    pkgDropBut <- tkbutton(pkgDFrame, width = 1, text = "v", font = "bold",
                        command = pkgSelect)
    tkpack(pkgDropBut, side = "left", expand = FALSE)
    tkpack(pkgDFrame, side = "left", expand = TRUE, fill = "x")
    tkpack(pkgFrame, side = "top", expand = FALSE, fill = "x",
           pady = 5, padx = 5)

    # Put the list box for package contents and associated buttons
    midFrame <- tkframe(base)
    leftFrame <- tkframe(midFrame)
    tkpack(tklabel(leftFrame, text = "Contents"), side = "top",
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
    writePkgDirs()

    tkwait.window(base)

    return(invisible())
}

getPkgContents <- function(pkgName, exclude = getExclude()){
    options(show.error.messages = FALSE)
    cont <- try(list.files(pkgName))
    options(show.error.messages = TRUE)
    if(inherits(cont, "try-error")){
        return(paste(pkgName, "may not be a valid directory"))
    }else{
        return(setdiff(appendSepDir(file.path(pkgName)), exclude))
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









