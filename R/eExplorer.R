# This widget gets all the R example code from R-ex directory of a
# given package and allows users to execute the example code chunks
# and view the result of the execution.
eExplorer <- function(pkgName, font = "arial 13"){
    # An environment to evaluate the code within
    evalEnv <- new.env(hash = TRUE, parent = parent.frame())
    if(any(missing(pkgName), is.null(pkgName), is.na(pkgName),
           pkgName == "")){
        stop("Can not proceed without a package name")
    }
    if(!require(pkgName, character.only = TRUE)){
        stop(paste("Package name", pkgName, "is invalid"))
    }

    chunkList <- getExCode(pkgName)

    if(is.null(chunkList)){
        chunkOrNot <- "No example code obtained"
        nameNCode <- NULL
    }else{
        chunkOrNot <- "Example code chunk"
    }

    end <- function(){
        tkdestroy(base)
    }
    on.exit(end())
    showHelp <- function(){
         chunk <- as.character(tkget(chunkText,
                                          (tkcurselection(chunkText))))
         helpFile <- getHelpFile(pkgName, chunk)
         # Get rid of the "_\b"
         helpFile <- gsub("_\\\b", "", helpFile)
         tkconfigure(resultViewer, state = "normal")
         tkdelete(resultViewer, "1.0", "end")
         for(i in helpFile){
             tkinsert(resultViewer, "end", paste(i, "\n", sep = ""))
         }
         tkconfigure(resultViewer, state = "disabled")
     }

    # Shows the code chunk in a text box that allows the user to
    # editor the code chunk in the box but not to the actual code chunk
    showCode <- function(){
        chunk <- as.character(tkget(chunkText,
                                          (tkcurselection(chunkText))))
        tkconfigure(editViewer, state = "normal")
        tkdelete(editViewer, "1.0", "end")
        for(i in chunkList[[chunk]]){
            tkinsert(editViewer, "end", paste(i, "\n", sep = ""))
        }
        tkconfigure(editViewer, state = "disabled")
        tkconfigure(resultViewer, state = "normal")
        tkdelete(resultViewer, "0.0", "end")
        tkconfigure(resultViewer, state = "disabled")

        tkconfigure(execButton, state = "normal")
        tkconfigure(clearButton, state = "normal")
#        tkconfigure(expoButton, state = "normal")
        tkconfigure(helpButton, state = "normal")
    }
    # Export code chunk to the R session
    export <- function(){
        temp <- objectBrowser(evalEnv)
        for(i in names(temp)){
            assign(i, temp[[i]], env = parent.frame(2))
        }
    }

    # Executes whatever that is in the text box for code chunk
    execute <- function(){
        expCode <- as.character(tkget(chunkText,
                                          (tkcurselection(chunkText))))
        tkconfigure(resultViewer, state = "normal")
        tkdelete(resultViewer, "0.0", "end")
        options(show.error.messages = FALSE)
        out <- try(capture.output(do.call("example", list(topic = expCode))))
        options(show.error.messages = TRUE)
        if(inherits(out, "try-error")){
            cont <- paste("Execution fauled because of:", out)
            tkinsert(resultViewer, "end", out)
                tkinsert(resultViewer, "end", "\n")
        }else{
            if(length(out) > 0 && out != "NULL"){
                for(outputs in out){
                    tkinsert(resultViewer, "end", outputs)
                    tkinsert(resultViewer, "end", "\n")
                }
            }
        }
        tkconfigure(resultViewer, state = "disabled")
        tkconfigure(expoButton, state = "normal")
    }

    # keeps track of code modification done
    codeChanged <- function(){
        newCode <<- TRUE
    }

    # Cleans the boxes for code chunk and result of execution
    clear <- function(){
        tkconfigure(editViewer, state = "normal")
        tkdelete(editViewer, "1.0", "end")
        tkconfigure(editViewer, state = "disabled")
        tkconfigure(resultViewer, state = "normal")
        tkdelete(resultViewer, "1.0", "end")
        tkconfigure(resultViewer, state = "disabled")

        tkconfigure(execButton, state = "disabled")
        tkconfigure(expoButton, state = "disabled")
        tkconfigure(clearButton, state = "disabled")
    }

    # Initilizes the buttons for code chunks
    popChunks <- function(){
        if(!is.null(chunkList)){
            writeList(chunkText, names(chunkList), TRUE)
        }
    }

    base <- tktoplevel()
    tktitle(base) <- "BioC R-ex Explorer\n"
    # Write package and vignette names
    pNvNames <- paste("Package:", pkgName,
                      "\nPick a code chunk to view/execute", sep = "")
    tkpack(tklabel(base, text = pNvNames, font = font), pady = 4)

    listFrame <- tkframe(base)
    # Create a text widgets for code chunks
    chunkFrame <- tkframe(listFrame)
    tkpack(tklabel(chunkFrame, text = chunkOrNot, font = font))
    chunkText <- makeViewer(chunkFrame, vWidth = 18, vHeight = NULL,
                      hScroll = TRUE, vScroll = TRUE, what = "list")
    tkbind(chunkText, "<B1-ButtonRelease>", showCode)
    popChunks()
    tkpack(chunkFrame, side = "left", anchor = "nw", expand = FALSE,
           fill = "y")

    # Create the viewers for code and results of execution
    codeNRelFrame <- tkframe(listFrame)
    editFrame <- tkframe(codeNRelFrame)
    tkpack(tklabel(editFrame, text = "R Source Code", font = font))
    eViewerFrame <- tkframe(editFrame)
    editViewer <- makeViewer(eViewerFrame, vWidth = 50, vHeight = 5,
                      hScroll = TRUE, vScroll = TRUE, what = "text")
    tkconfigure(editViewer, font = font)
    tkconfigure(editViewer, state = "disabled")
#    tkbind(editViewer, "<KeyRelease>", codeChanged)
    tkpack(eViewerFrame, expand = TRUE, fill = "both")
    tkpack(tklabel(editFrame, text = "Display window", font = font))
    rViewerFrame <- tkframe(editFrame)
    resultViewer <-  makeViewer(rViewerFrame, vWidth = 50, vHeight = 5,
                      hScroll = TRUE, vScroll = TRUE, what = "text")
    tkconfigure(resultViewer, font = font)
    tkpack(rViewerFrame, expand = TRUE, fill = "both")
    tkpack(editFrame, expand = TRUE, fill = "both")
    tkpack(codeNRelFrame, side = "left", expand = TRUE, fill = "both")

    tkpack(listFrame, side = "top", expand = TRUE, fill = "both", padx
    = 4, pady = 6)

    # Put the buttons in
    butFrame <- tkframe(base)
    helpButton <- tkbutton(butFrame, text = "View Help", width = 12,
                          font = font, state = "disabled",
                          command = showHelp)
    execButton <- tkbutton(butFrame, text = "Execute Code", width = 12,
                           font = font, command = execute)
    tkconfigure(execButton, state = "disabled")
    expoButton <- tkbutton(butFrame, text = "Export to R", width = 12,
                           font = font, command = export)
    tkconfigure(expoButton, state = "disabled")
    clearButton <- tkbutton(butFrame, text = "Clear", width = 12,
                            font = font, command = clear)
    tkconfigure(clearButton, state = "disabled")
    tkpack(helpButton, execButton, expoButton, clearButton, side = "left")
    tkpack(butFrame, pady = 6)
    # Put end button separately to avoid accidents
    endButton <- tkbutton(base, text = "End", width = 12,
                          font = font, command = end)
    tkpack(endButton)

    tkwait.window(base)
}

getExCode <- function(pkgName){
    options(show.error.messages = FALSE)
    tryMe <- try(list.files(file.path(.path.package(pkgName), "R-ex")))
    options(show.error.messages = TRUE)

    if(inherits(tryMe, "try-error")){
        return(NULL)
    }
    # Make sure only to deal with files with an R extension
    tryMe <- tryMe[grep("\\.R", tryMe)]
    if(length(tryMe) == 0){
        return(NULL)
    }
    codeChunks <- list()
    for(i in tryMe){
        codeChunks[[gsub("\\.R", "", i)]] <- readLines(
                     file.path(.path.package(pkgName), "R-ex", i))
    }
    return(codeChunks)
}

getHelpFile <- function(pkgName, fileName){
    if(any(missing(pkgName), missing(fileName))){
        return("Can't get help file because pkgName or fileName is missing")
    }
    if(any(is.null(pkgName), is.null(fileName), is.na(pkgName),
           is.na(fileName))){
        return(paste("Can't get help file because pkgName or",
                     "fileName is NULL or NA"))
    }
    options(show.error.message = FALSE)
    tryMe <- try(readLines(file.path(.path.package(pkgName), "help",
                                     fileName)))
    options(show.error.messages = TRUE)
    if(inherits(tryMe, "try-error")){
        return("Can't get help file because pkgName or fileName is invalid")
    }else{
        return(tryMe)
    }
}

