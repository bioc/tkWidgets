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
    }else{
        pkgPath <- tclVar(pkgPaths[1])
        pkgName <- tclVar(pkgName)
    }
    pkgNames <- getRPkgs(tclvalue(pkgPath))
    baseDir <- file.path(tclvalue(pkgPath), tclvalue(pkgName))
    subDirs <- NULL
    end <- function(){
        tkdestroy(base)
    }
    on.exit(end())
    pathEntered <- function(){
        tempNames <- getRPkgs(tclvalue(pkgPath))
        if(!is.null(tempNames)){
            pkgNames <<- tempNames
            tclvalue(pkgName) <<- pkgNames[1]
            baseDir <<- file.path(tclvalue(pkgPath), pkgNames[1])
            writePkgDirs()
            subDirs <<- NULL
        }else{
            tclvalue(pkgPath) <<- gsub(paste(.Platform$file.sep,
                                     basename(baseDir), sep = ""), "",
                                       baseDir)
        }
    }
    upDatePath <- function(){
        options(show.error.messages = FALSE)
        opt <- try(getListOption(pathEntry, pkgPaths))
        options(show.error.messages = TRUE)
        if(!inherits(opt, "try-error")){
            writeList(pathEntry, opt, clear = TRUE)
            tclvalue(pkgPath) <<- opt
            pkgNames <<- getRPkgs(opt)
            tclvalue(pkgName) <<- pkgNames[1]
            baseDir <<- file.path(opt, pkgNames[1])
            writePkgDirs()
            subDirs <<- NULL
        }
    }
    browse <- function(){
        tempPath <- tclvalue(tkchooseDirectory(mustexist = TRUE))
        tempNames <- getRPkgs(tempPath)
        if(!is.null(tempNames)){
            pkgNames <<- tempNames
            tclvalue(pkgPath) <<- tempPath
            pkgNames <<- getRPkgs(tclvalue(pkgPath))
            tclvalue(pkgName) <<- pkgNames[1]
            baseDir <<- file.path(tclvalue(pkgPath), tclvalue(pkgName))
            subDir <<- NULL
            writePkgDirs()
        }
    }
    writePkgDirs <- function(){
        writeList(listView, getPkgContents(baseDir, getExclude()))
        tkconfigure(contViewer, state = "normal")
        tkdelete(contViewer, "0.0", "end")
        tkconfigure(contViewer, state = "disabled")
    }
    pkgSelect <- function(){
        options(show.error.messages = FALSE)
        opt <- try(getListOption(pkgDropBut, pkgNames, 15, TRUE))
        options(show.error.messages = TRUE)
        if(!inherits(opt, "try-error")){
            #tkconfigure(pkgEntry, state = "normal")
            #writeList(pkgEntry, opt, clear = TRUE)
            #tkconfigure(pkgEntry, state = "disabled")
            tclvalue(pkgName) <<- opt
            baseDir <<- file.path(tclvalue(pkgPath), opt)
            subDirs <<- NULL
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
            subDirs <<- c(subDirs, gsub(.Platform$file.sep, "", selectedObj))
            writeList(listView, getPkgContents(file.path(baseDir,
                paste(subDirs, sep = "", collapse = .Platform$file.sep)),
                                               getExclude()))
            tkconfigure(upBut, state = "normal")
        }else{
            popDisplay(getFileContents(file.path(baseDir,
                paste(subDirs, sep = "", collapse = .Platform$file.sep)),
                                       selectedObj))
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
        if(length(subDirs) >= 1){
            subDirs <<- subDirs[1:length(subDirs) - 1]
            if(length(subDirs) == 0){
                writeList(listView, getPkgContents(baseDir,
                                                          getExclude()))
                tkconfigure(upBut, state = "disabled")
            }
            writeList(listView, getPkgContents(file.path(baseDir,
                paste(subDirs, sep = "", collapse = .Platform$file.sep)),
                                               getExclude()))
            tkconfigure(contViewer, state = "normal")
            tkdelete(contViewer, "0.0", "end")
            tkconfigure(contViewer, state = "disabled")
            if(length(subDirs) == 0){
                tkconfigure(upBut, state = "disabled")
            }
        }
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
    tkbind(pathEntry, "<KeyPress-Return>", pathEntered)
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
    tkpack(tklabel(pkgFrame, text = "Exploring:"), side = "left",
           expand = FALSE)
    pkgDFrame <- tkframe(pkgFrame, borderwidth = 2, relief = "sunken")
    pkgEntry <- tklabel(pkgDFrame, textvariable = pkgName,
                        borderwidth  = 1)
    tkpack(pkgEntry, side = "left", expand = TRUE, fill = "both")
    pkgDropBut <- tkbutton(pkgDFrame, width = 14, text = "Select Package",
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
    listView <- makeViewer(dirLFrame, vWidth = 15, vHeight = 10,
                           hScroll = TRUE)
    tkpack(dirLFrame, side = "top", expand = TRUE, fill = "y")
    upBut <- tkbutton(leftFrame, text = "Back", width = 10,
                      state = "disabled", command = goUp)
    tkpack(upBut, side = "top", expand = FALSE, fill = "x")

    tkbind(listView, "<Double-Button-1>", dClick)
#    tkbind(listView, "<B1-ButtonRelease>", sClick)
    tkpack(leftFrame, side = "left", expand = FALSE, fill = "y")
    # Put the text box to show contents of a selected file
    rightFrame <- tkframe(midFrame)
    tkpack(tklabel(rightFrame, text = "Display window"), side = "top",
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
        tkmessageBox(title = "Access Error",
                     message = (paste("Problem accessing", pkgName,
                                      "\nbecause of:\n",
                                      cont)),
                     icon = "info",
                     type = "ok")
        return("")
    }else{
        return(setdiff(appendSepDir(file.path(pkgName)), exclude))
    }
}

getFileContents <- function(path, fileName){
    if(basename(path) == "help"){
        return(procHelp(file.path(path, fileName)))
    }
    if(regexpr("\\.rda", fileName) > 0 ){
        return(procRda(file.path(path, fileName)))
    }
    if(regexpr("\\.gz", fileName) > 0){
        return(readLines(gzfile(file.path(path, fileName))))
    }
    if(regexpr("\\.zip", fileName) > 0){
        return(readLines(unz(file.path(path, fileName))))
    }
#    if(regexpr("\\.Rd", fileName) > 0){
#        return(procRd(file.path(path, fileName)))
#    }
    if(regexpr("\\.pdf", fileName) > 0){
        return(procPDF(file.path(path, fileName)))
    }
    if(regexpr("\\.html", fileName) > 0){
        return(procHTML(file.path(path, fileName)))
    }
    # Ohterwise, use readLines
    return(readLines(file.path(path, fileName)))
}

# For rda files, try to find the Rd file and return it
procRda <- function(fileName){
    if(regexpr(paste("data", .Platform$file.sep,
                     basename(fileName), sep = ""), fileName) > 0){
        return(procHelp(gsub(paste("data", .Platform$file.sep,
                               basename(fileName), sep = ""),
                         paste("help", .Platform$file.sep,
                               gsub("\\.rda", "", basename(fileName)),
                               sep = ""), fileName)))
    }else{
    # If not in "data" directory, ust do not display
        tkmessageBox(title = "Display Error",
                     message = paste("Data in\n", fileName,
                                      "\nare binary and are not displayed"),
                     icon = "info",
                     type = "ok")
        return("")
    }
}

procHelp <- function(fileName){
    options(show.error.messages = FALSE)
    doc <- try(readLines(fileName))
    options(show.error.messages = TRUE)
    if(inherits(doc, "try-error")){
        tkmessageBox(title = "Display Error",
                     message = (paste("Problem displaying\n", fileName,
                                      "\nbecause of:\n", doc)),
                     icon = "info",
                     type = "ok")
        return("")
    }else{
        # Get rid of "_\b"s
        return(gsub("_\\\b", "", doc))
    }
}

procPDF <- function(fileName){
   OST <- .Platform$OS.type
   if (OST == "windows") {
       shell.exec(fileName)
   }
   else if (OST == "unix") {
       viewer <- getOption("pdfviewer")
       if (is.null(viewer)) {
           for (x in c("xpdf", "acroread", "acroread4")) {
               viewer <- system(paste("which", x), intern = TRUE,
                                ignore.stderr = TRUE)
               if (length(viewer) > 0 && file.exists(viewer))
                   break
               viewer <- character()
           }
           if (length(viewer) == 0) {
               tkmessageBox(title = "Display Error",
                       message = ("No available PDF viewer found on system"),
                       icon = "info",
                       type = "ok")
               return("")
           }
       }
   }

   system(paste(viewer, fileName))
   return(invisible())
}

procHTML <- function(fileName){
    options(show.error.messages = FALSE)
    tryMe <- try(browseURL(fileName))
    options(show.error.messages = TRUE)
    if(inherits(tryMe, "try-error")){
        tkmessageBox(title = "Display Error",
                       message = (paste("Problem displaying\n", fileName,
                                        "becuase of:\n",
                                        tryMe)),
                       icon = "info",
                       type = "ok")
    }
    return(invisible())
}

getExclude <- function(){
    return(c("Meta/", "latex/", "INDEX", "CVS"))
}

getRPkgs <- function(pkgPath){
     options(warn = -1)
     toCheck <- try(list.files(pkgPath))
     options(warn = 0)
     if(length(toCheck) == 0){
         tkmessageBox(title = "No Packages found",
                       message = paste("No R packages found in\n",
                                 pkgPath),
                       icon = "info",
                       type = "ok")
         return(NULL)
     }
     isd <- file.info(file.path(pkgPath, toCheck))
     dirWithDesc <- sapply(file.path(pkgPath,toCheck[isd$isdir]), hasDesc)
     if(length(dirWithDesc) == 0){
         tkmessageBox(title = "No Packages found",
                       message = paste("No R packages found in\n",
                                 pkgPath),
                       icon = "info",
                       type = "ok")
         return(NULL)
     }
     pkgs <- basename(names(dirWithDesc)[dirWithDesc])
     if(length(pkgs) != 0){
         return(pkgs)
     }else{
         tkmessageBox(title = "No Packages found",
                      message = paste("No R packages found in\n",
                                       pkgPath),
                      icon = "info",
                      type = "ok")
         return(NULL)
     }
}

hasDesc <- function(pkgPath){
    if(any(list.files(pkgPath) == "DESCRIPTION")){
        return(TRUE)
    }else{
        return(FALSE)
    }
}








