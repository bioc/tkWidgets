# This function lists all the R packages in the library that have a
# Bioconductor vignettes and waits for the user to selecte one to
# explore.
#
# Copyrihgt, 2002, J. Zhang, all rights reserved
#

vExplorer <- function (title = "BioC Vignettes Explorer",
                           packNames = ""){

    require(Biobase) || stop("Package Biobase not available!")
    require(tools) || stop("Package tools not available!")

    on.exit(tkdestroy(base))
    PLATFORM <- .Platform$OS.type
    selectedPkg <- NULL
    selectedVig <- NULL
    vigList <- NULL

    end <- function(){
        tkdestroy(base)
    }

    # Executes when a user clicks a package name in the list of pkg names
    packSelected <- function(){
         selectedPkg <<-
             tclvalue(tkget(packViewer,(tkcurselection(packViewer))))
         checkMe <- .getPackNames(selectedPkg)
         if(is.null(checkMe)){
             tkmessageBox(title = "No vignette found",
                     message = paste("Package", selectedPkg,
                     "has no vignette"))
         }else{
             path <- .path.package(selectedPkg)
             vigList <<- getPkgVigList(path)
             tkdelete(vigViewer, 0, "end")
             for(i in names(vigList)){
                 if(!inherits(chunkList, "try-error")){
                     tkinsert(vigViewer, "end", i)
                 }
             }
             tkconfigure(vButton, state = "disabled")
         }
    }

    # Executes when a user clicks a vignette name in the list of Rnw files
    vigSelected <- function(){
        selectedVig <<-
             tclvalue(tkget(vigViewer,(tkcurselection(vigViewer))))
        tkconfigure(vButton, state = "normal")
    }

    # Executes when a user clicks the view button after selecting a
    # vignette
    viewVig <- function(){
        viewVignette(title, selectedPkg, vigList[[selectedVig]]$VigPath,
                     vigList[[selectedVig]]$PDFpath)
    }


    base <- tktoplevel()
    tktitle(base) <- title

    # List box showing all the packages that have a vignette
    packFrame <- tkframe(base)
    packName <- tklabel(packFrame, text = "Package List")
    tkpack(packName, side = "top")
    packViewer <- makeViewer(packFrame, vWidth = 30, vHeight = 6,
                      hScroll = TRUE, vScroll = TRUE)
    tkconfigure(packViewer, selectmode = "browse")
    tkbind(packViewer, "<B1-ButtonRelease>", packSelected)

    # List box for vignettes of a given package
    vigFrame <- tkframe(base)
    vigName <- tklabel(vigFrame, text = "Vignette List")
    tkpack(vigName, side = "top")
    vigViewer <- makeViewer(vigFrame, vWidth = 30, vHeight = 6,
                      hScroll = TRUE, vScroll = TRUE)
    tkconfigure(vigViewer, selectmode = "browse")
    tkbind(vigViewer, "<B1-ButtonRelease>", vigSelected)

    # Buttons to view a vignette and end the widget
    butFrame <- tkframe(base)
    vButton <- tkbutton(butFrame, text = "View", width = 8,
                        command = viewVig)
    tkpack(vButton, side = "top")

    # Put the three frames in
    tkgrid(packFrame, vigFrame, butFrame, pady = 10, padx = 10)

    # put in the end button
    endButton <- tkbutton(base, text = "End", width = 8, command = end)
    tkgrid(endButton, columnspan = 3, pady = 10)

    # Populates the list box for package names
    .popPackList(packViewer)

    tkconfigure(vButton, state = "disabled")

    tkwait.window(base)
}

# Check for the availability of vignettes and populate the list box
# for packages that have a vignette
.popPackList <- function(packViewer){
    packs <- .packages(all = TRUE)
    tkdelete(packViewer, 0, "end")
    for(i in sort(packs)){
        if(!is.null(pkgVignettes(i)) &&
           length(pkgVignettes(i)$docs) > 0){
            tkinsert(packViewer, "end", i)
        }
    }
}

# Returns package names that have a vignette
.getPackNames <- function(packName = ""){
    if(packName == ""){
        packNames <- .packages(all = TRUE)
    }else{
        packNames <- packName
    }
    goodNames <- NULL
    for(i in packNames){
        eval(call(deparse(substitute(require)), i, TRUE))
        path <- .path.package(i)
        options(warn = -1)
        tryMe <- getPkgVigList(path)
        options(warn = 1)
        if(!is.null(tryMe))
        goodNames <- c(goodNames, i)
    }
    return(goodNames)
}

# This window is called by vExplorer for interacting with the code
# chunks of a vignette
viewVignette <- function(title, packName, vigPath, pdfPath){

    on.exit(end)

    selectedChunk <- NULL
    buts <- vector("list")
    codeVersion <- NULL
    newCode <- FALSE
    executed <- NULL
    chunkOrNot <- "Code Chunk"

    chunkList <- getVignetteCode(vigPath)

    if(is.null(chunkList)){
        chunkOrNot <- "No Code Chunk"
        nameNCode <- NULL
#        tkmessageBox(title = "No code chunk found",
#                     message = paste(gsub(".*/(.*)", "\\1", vigPath),
#                     "does not contain any code chunk"))
#        return()
    }else{
        nameNCode <- .getNameNCode(chunkList)
#        codeVersion[[1]] <- chunkList
    }

    end <- function(){
        tkdestroy(base)
    }
    # Not implemented now but will be later
    back <- function(){
        if(length(codeVersion) > 1){
            codeVersion <<- codeVersion[[-length(codeVersion)]]
        }
        if(length(codeVersion) == 1){
            tkconfigure(backButton, state = "disabled")
        }
        tempCode <- chunk(getChunk(codeVersion[[length(codeVersion)]]))
        tkdelete(editViewer, "1.0", "end")
        for(i in tempCode){
            tkinsert(editViewer, "end", paste(i, "\n", sep = ""))
        }
        tkdelete(resultViewer, 0, "end")
    }

    # Executed when a user clicks the view PDF button
    viewPDF <- function(){
        openPDF(pdfPath)
    }
    # Shows the code chunk in a text box that allows the user to
    # editor the code chunk in the box but not to the actual code chunk
    showCode <- function(chunkName){
        tkdelete(editViewer, "1.0", "end")
        tkconfigure(resultViewer, state = "normal")
        tkdelete(resultViewer, "1.0", "end")
        tkconfigure(resultViewer, state = "disabled")
        for(i in nameNCode[[chunkName]]){
            tkinsert(editViewer, "end", paste(i, "\n", sep = ""))
        }
        tkconfigure(buts[[chunkName]], state = "active")
        if(chunkName != 1){
            tkconfigure(buts[[chunkName - 1]], state = "normal")
        }
        tkconfigure(execButton, state = "normal")
        tkconfigure(clearButton, state = "normal")
    }
    # Executes whatever that is in the text box for code chunk
    execute <- function(){
#        if(selectedChunk == 1 || any(executed == (selectedChunk - 1))){
            if(newCode){
                tempCode <- tclvalue(tkget(editViewer, "1.0", "end"))
                tempChunk <-
                    editVignetteCode(chunkList,
                                     selectedChunk,tempCode)
                result <- evalChunk(tempChunk, selectedChunk)
                newCode <<- FALSE
#                tkconfigure(backButton, state = "normal")
            }else{
                result <- evalChunk(chunkList, selectedChunk)
#                modButton(buts[[selectedChunk]], "libhtblue")
                tkconfigure(buts[[selectedChunk]], relief = "sunken",
                            state = "active")
                if(selectedChunk < length(buts)){
                    tkconfigure(buts[[selectedChunk + 1]], state = "normal")
                }

            }
            tkdelete(resultViewer, "", "end")
            tkconfigure(resultViewer, state = "normal")
            tkinsert(resultViewer, "end", result)
            tkconfigure(resultViewer, state = "disabled")
#            modButton(buts[[selectedChunk]], "blue")
#            executed <<- unique(c(executed, selectedChunk))
#        }else{
#             tkmessageBox(title = "Execution failed",
#                     message = "Code chunks need to be executed in order")
#        }
    }

    # keeps track of code modification done
    codeChanged <- function(){
        newCode <<- TRUE
    }

    # Cleans the boxes for code chunk and result of execution
    clear <- function(){
        tkdelete(editViewer, "1.0", "end")
        tkconfigure(resultViewer, state = "normal")
        tkdelete(resultViewer, "1.0", "end")
        tkconfigure(resultViewer, state = "disabled")
        for(i in 1:length(buts)){
            tkconfigure(buts[[i]], state = "disabled", relief = "raised")
#            modButton(buts[[i]], "green")
        }
        tkconfigure(buts[[1]], state = "normal")
        tkconfigure(execButton, state = "disabled")
        tkconfigure(clearButton, state = "disabled")
        executed <<- NULL
        codeVersion[[1]] <<- chunkList
#        tkconfigure(backButton, state = "disabled")
    }

    # Changes the background colour of a widget
    modButton <- function(but, what){
        tkconfigure(but, background = what)
    }

    # Initilizes the buttons for code chunks
    popChunks <- function(){
        chunkFrame <- tkframe(chunkCanv)
        if(!is.null(nameNCode)){
            k <- 1
            for(i in names(nameNCode)){
                # Create button functions
                tempBut <- substitute(buts[[j]], list(j = k))

                fun <- function() {}
                body <- list(as.name("{"),
                             substitute(showCode(j),
                                        list(j = k)),
                             substitute(selectedChunk <<- j,
                                        list(j = k)))
                body(fun) <- as.call(body)
                assign(paste("chunkList",k,sep=""), fun)

                buts[[k]] <<- tkbutton(chunkFrame, text= i, width = 23,
                                       state = "disabled",
                                       command = get(paste("chunkList",
                                       k, sep = "")))
                tkpack(buts[[k]])
                tkbind(buts[[k]], "<Double-Button-1>", execute)
                k <- k + 1
            }
            tkconfigure(buts[[1]], state = "normal")
        }
        tkcreate(chunkCanv, "window", 5, 5, anchor = "nw",
                 window = chunkFrame)
    }

    base <- tktoplevel()
    tktitle(base) <- title
    # Write package and vignette names
    namesFrame <- tkframe(base)
    pNvNames <- paste("Package:", packName, "   Vignette:",
                      gsub(paste(".*", .Platform$file.sep, "(.*)",
                                 sep = ""), "\\1", vigPath))
    tkpack(tklabel(namesFrame, text = pNvNames))
    tkgrid(namesFrame, columnspan = 2, pady = 10)

    # Create the viewer for code chunks
    chunkFrame <- tkframe(base)
    tkpack(tklabel(chunkFrame, text = chunkOrNot))
    chunkCanv <-  makeViewer(chunkFrame, vWidth = 200, vHeight = 455,
                      hScroll = FALSE, vScroll = TRUE, what = "canvas")

    # Create the viewers for code and results of execution
    editFrame <- tkframe(base)
    tkpack(tklabel(editFrame, text = "R Source Code"))
    eViewerFrame <- tkframe(editFrame)
    editViewer <- makeViewer(eViewerFrame, vWidth = 50, vHeight = 14,
                      hScroll = TRUE, vScroll = TRUE, what = "text")
    tkbind(editViewer, "<KeyRelease>", codeChanged)
    tkpack(eViewerFrame)
    tkpack(tklabel(editFrame, text = "Results of Execution"))
    rViewerFrame <- tkframe(editFrame)
    resultViewer <-  makeViewer(rViewerFrame, vWidth = 50, vHeight = 12,
                      hScroll = TRUE, vScroll = TRUE, what = "text")
    tkpack(rViewerFrame)

    tkgrid(chunkFrame, editFrame, pady = 10, padx = 10)

    # Put the buttons in
    butFrame <- tkframe(base)
    pdfButton <- tkbutton(butFrame, text = "View PDF", width = 12,
                          command = viewPDF)
    execButton <- tkbutton(butFrame, text = "Execute Code", width = 12,
                           command = execute)
    tkconfigure(execButton, state = "disabled")
#    backButton <- tkbutton(butFrame, text = "<Back", width = 12,
#                           command = back)
#    tkconfigure(backButton, state = "disabled")
    clearButton <- tkbutton(butFrame, text = "Clear", width = 12,
                            command = clear)
    tkconfigure(clearButton, state = "disabled")
    tkpack(pdfButton, execButton, clearButton, side = "left")
    tkgrid(butFrame, columnspan = 2, pady = 10)
    # Put end button separately to avoid accidents
    endButton <- tkbutton(base, text = "End", width = 12,
                          command = end)
    tkgrid(endButton, columnspan = 2, pady = 10)

    popChunks()

    if(is.null(pdfPath) || is.na(pdfPath)){
        tkconfigure(pdfButton, state = "disabled")
    }
#    tkconfigure(resultViewer, state = "disabled")

    tkwait.window(base)
}

.getNameNCode <- function(chunkList){
    chunkNum <- numChunks(chunkList)
    nameNCode <- list()
    for(i in 1:chunkNum){
        name <- chunkName(getChunk(chunkList, i))
        if(length(name) == 0){
            name <- paste("Code chunk", i)
        }
        nameNCode[[name]] <- chunk(getChunk(chunkList, i))
    }
    return(nameNCode)
}
