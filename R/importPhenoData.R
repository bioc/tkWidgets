# Functions that guide users through the steps of inporting a
# phenoData object. Used by the affy package.

importPhenoData <- function(sampleNames = NULL){

    if(!require("Biobase", character.only = TRUE)){
        tkmessageBox(title = paste("Dependency error"),
                     message = paste("This widget requires Biobase",
                     "that is not available in your system.",
                     "Please install Biobase and try again"),
                     icon = "error",
                     type = "ok")
        stop()
    }

    varLabels <- NULL
    phenodata <- NULL
    newPhenoData <- NULL

    end <- function(){
        tkgrab.release(base)
        tkdestroy(base)
    }
    on.exit(end())

    cancel <- function(){
        newPhenoData <- NULL
        end()
    }

    getPData <- function(what, desc = NULL){
        if(what == "file"){
            pdata <- importWizard()[["data"]]
            if(is.null(pdata)){
                return(invisible())
            }
            #colnames(pdata) <- paste("Covar", 1:ncol(pdata), sep = "")
            if(is.null(sampleNames)){
                rownames(pdata) <- paste("Sample", 1:nrow(pdata), sep = "")
            }
            varList <- list()
            varList[colnames(pdata)] <- ""
        }else if (what == "df"){
            pdata <- getOBJWidget("data.frame")
            if(is.null(pdata)){
                return(invisible())
            }
            varList <- list()
            varList[colnames(pdata)] <- ""
        }else if (what == "pd"){
            pdata <- getOBJWidget("phenoData")
            if(is.null(pdata)){
                return(invisible())
            }
            varList <- varLabels(pdata)
            pdata <- pData(pdata)
        }else{
            sampleNCov <- getSNCNums(sampleNames)
            if(is.null(sampleNCov)){
                return(invisible())
            }
            pdata <- data.frame(matrix("", nrow = sampleNCov$samples,
                                       ncol = sampleNCov$covars))
            colnames(pdata) <- paste("Covar", 1:ncol(pdata), sep = "")
            if(is.null(sampleNames)){
                rownames(pdata) <- paste("Sample", 1:nrow(pdata), sep = "")
            }
            varList <- list()
            varList[colnames(pdata)] <- ""
        }
        if(!is.null(sampleNames)){
            pdata <- writePDRowNames(pdata, sampleNames)
        }
        tempData <- createPData(pdata, varList)
        if(!is.null(tempData)){
            #names(varList) <- colnames(tempData)
            #phenoList <- getCovarDesc(varList, pdata)
            #tempPheno <- constPhenoData(tempData, varList)
            #if(!is.null(tempPheno)){
            #    newPhenoData <<- tempPheno
            newPhenoData <<- tempData
                filename <- getName4Data("phenodata", objType = "phenoData")
                if(!is.null(filename)){
                    .GlobalEnv[[filename]] <- newPhenoData
                }
                end()
            #}
        }
    }
    readFile <- function(){
        getPData("file")
    }

    readDF <- function(){
        getPData("df")
    }

    readPheno <- function(){
        getPData("pd")
    }

    createNew <- function(){
        getPData("new")
    }

    base <- tktoplevel()
    tktitle(base) <- "BioC Read phenoData"

    tkpack(tklabel(base, text = paste("Please make a selection using",
                         "the buttons below:"),
                         font = "Helvetica 11 bold"), side = "top",
                   expand = FALSE, pady = 8, padx = 5)

    # Frame for read from file
    fileFrame <- tkframe(base, borderwidth = 2, relief = "groove")
    tkpack(tkbutton(fileFrame, text = "Read From File", width = 18,
                command = readFile), side = "left", expand = FALSE)
    tkpack(tklabel(fileFrame, text = paste("Create a phenoData object",
                              "using a specified file")),
           side = "left", expand = FALSE)
    tkpack(fileFrame, side = "top", anchor = "w", pady = 2, padx = 5)
    # frame for read from data frame
    dfFrame <- tkframe(base, borderwidth = 2, relief = "groove")
    tkpack(tkbutton(dfFrame, text = "Read From Object", width = 18,
                command = readDF), side = "left", expand = FALSE)
    tkpack(tklabel(dfFrame, text = paste("Create a phenoData object",
                              "using an existing data frame")),
           side = "left", expand = FALSE)
    tkpack(dfFrame, side = "top", anchor = "w", pady = 2, padx = 5)
    # Frame for editing phenoData
    epFrame <- tkframe(base, borderwidth = 2, relief = "groove")
    tkpack(tkbutton(epFrame, text = "Edit phenoData", width = 18,
                command = readPheno), side = "left", expand = FALSE)
    tkpack(tklabel(epFrame, text = paste("Editing an existing phenoData",
                              "object")),
           side = "left", expand = FALSE)
    tkpack(epFrame, side = "top", anchor = "w", pady = 2, padx = 5)

    # Frame for creating new phenoData
    newFrame <- tkframe(base, borderwidth = 2, relief = "groove")
    tkpack(tkbutton(newFrame, text = "Create New phenoData", width = 18,
                command = createNew), side = "left", expand = FALSE)
    tkpack(tklabel(newFrame, text = "Create a new phenoData object"),
           side = "left", expand = FALSE)
    tkpack(newFrame, side = "top", anchor = "w", pady = 2, padx = 5)

    tkpack(tkbutton(base, text = "Cancel", command = cancel, width = 15),
           side = "top", anchor = "center", expand = FALSE, pady = 10)

    tkgrab.set(base)
    tkwait.window(base)

    return(newPhenoData)
}

# Read a data frame from a specified file
getPDFromFile <- function(){

    pdata <- NULL

    fileName <- getNameWidget("file")
    if(is.null(fileName)){
        return(NULL)
    }
    args <- guess.sep(fileName)
    # Try read.table by figuring out the args
    options(show.error.messages = FALSE)
    tryMe <- try(read.table(file = fileName, sep = args[["separator"]],
                            header = args[["header"]]))
    options(show.error.messages = TRUE)
    if(inherits(tryMe, "try-error")){
        # If fail, try this
        pdata <- readFileByUserArgs(fileName)
    }else{
        pdata <- tryMe
    }
    return(pdata)
}

# A widget to read in the name of an object
getOBJWidget <- function(type = NULL){
    toReturn <- NULL
    fileName <- tclVar("")

    end <- function(){
        if(tclvalue(fileName) == ""){
            tkmessageBox(title = paste("No name entered"),
                         message = paste("You have not entered a name yet",
                         "Please enter a name"),
                         icon = "error",
                         type = "ok")
        }else{
            if(objExists(tclvalue(fileName))){
                toReturn <<- get(tclvalue(fileName))
                tkgrab.release(base)
                tkdestroy(base)
            }else{
                tkmessageBox(title = paste("Reading Error"),
                    message = paste("I can't continue because:",
                    "\n 1. You may have entered an invalid name or",
                    "\n 2. the name of the object is not of",
                    "class data.frame/phenoData.\nPlease try again."),
                          icon = "error",
                          type = "ok")
            }
        }
    }

    cancel <- function(){
        tkgrab.release(base)
        tkdestroy(base)
        toReturn <<- NULL
    }
    on.exit(cancel())

    browse <- function(){
        filter <- function(x, env = .GlobalEnv){
            if(class(env[[x]]) == type)
                return(TRUE)
            else
                return(FALSE)
        }
        obj <- objectBrowser(nSelect = 1, fun = filter)
        if(!is.null(obj)){
            toReturn <<- obj[[1]]
        }
        tkgrab.release(base)
        tkdestroy(base)
    }

    base <- tktoplevel()
    tktitle(base) <- "BioC Input Widget"

    readFrame <- tkframe(base)
    tkpack(tklabel(readFrame, text = "Object name:", width = 14,
                   justify = "left"), side = "left", expand = FALSE)
    tkpack(tkentry(readFrame, width = 50, textvariable = fileName),
           side = "left", expand = TRUE, fill = "x")
    tkpack(tkbutton(readFrame, text = "Browse", command = browse),
           side = "left", expand = FALSE)
    tkpack(readFrame, side = "top", padx = 5, pady = 10, expand = TRUE,
           fill = "x")

    butFrame <- tkframe(base)
    backBut <- tkbutton(butFrame, text = "Continue", width = 8, command = end)
    contBut <- tkbutton(butFrame, text = "Cancel", width = 8,
                    command = cancel)
    tkgrid(backBut, contBut, padx = 20)
    tkpack(butFrame, expand = FALSE, fill = "x", padx = 5, pady = 5)

    tkgrab.set(base)

    tkwait.window(base)

    return(toReturn)
}

# Check to see if an object exists in .GlobalEnv
objExists <- function(name, type = NULL){
    if(name %in% ls(.GlobalEnv)){
        if(!is.null(type)){
            if(class(.GlobalEnv[[name]]) == type){
                return(TRUE)
            }else{
                return(FALSE)
            }
        }else{
            return(TRUE)
        }
    }else{
        return(FALSE)
    }
}

# Get the number of samples and covariates from a user through a widget
getSNCNums <- function(sampleNames){
    sampleNCov <- NULL

    covarNum <- tclVar(1)
    if(!is.null(sampleNames)){
        sampleNum <- tclVar(length(sampleNames))
    }else{
        sampleNum <- tclVar(1)
    }

    end <- function(){
        sampleNCov <<- list(samples = as.numeric(tclvalue(sampleNum)),
                            covars = as.numeric(tclvalue(covarNum)))
        tkgrab.release(base)
        tkdestroy(base)
    }

    cancel <- function(){
        tkgrab.release(base)
        tkdestroy(base)
        sampleNCov <<- NULL
    }

    base <- tktoplevel()
    tktitle(base) <- "BioC Input Widgets"

    numFrame <- tkframe(base)
    dropdownList(numFrame, as.character(1:20), sampleNum, 3,
                 tclvalue(sampleNum), TRUE)
    numLab <- tklabel(base, text = "Number of samples: ")
    tkgrid(numLab, numFrame, padx = 5, pady = 5)
    tkgrid.configure(numLab, sticky = "w")

    numFrame <- tkframe(base)
    dropdownList(numFrame, as.character(1:20), covarNum, 3, "1", TRUE)
    covLab <- tklabel(base, text = "Number of covariates: ")
    tkgrid.configure(covLab, numFrame)
    tkgrid.configure(covLab, sticky = "w")

    butFrame <- tkframe(base)
    backBut <- tkbutton(butFrame, text = "Continue", width = 8, command = end)
    contBut <- tkbutton(butFrame, text = "Cancel", width = 8,
                    command = cancel)
    tkgrid(backBut, contBut, padx = 20)
    tkgrid(butFrame, columnspan = 2, padx = 5, pady = 10)

    tkgrab.set(base)
    tkwait.window(base)

    return(sampleNCov)
}

# Put sample names as row names of the data frame
writePDRowNames <- function(pdata, sampleNames){
    if(is.null(pdata)){
        return(pdata)
    }
    options(show.error.messages = FALSE)
    tryMe <- try(rownames(pdata) <- sampleNames)
    options(show.error.messages = TRUE)
    if(inherits(tryMe, "try-error")){
        ok <- tkmessageBox(title = "Sample mis-match",
                     message = paste("Length of sample names and",
                         "row numbers of data frame do not match.",
                         "\nContinue anyway?"),
                     icon = "question",
                     type = "yesno")
        if(tclvalue(ok) == "no"){
            return(NULL)
        }
    }else{
        if(!is.null(sampleNames)){
            rownames(pdata) <- sampleNames
        }
    }
    return(pdata)
}

# This widget is called by importPhenoData when uses decide to create
# a phenoData object based on a file, a data frame, or phenoData object
createPData <- function(pdata, varList){
    newPhenoData <- NULL
    phenoList <- NULL

    end <- function(){
        newPhenoData <<- NULL
        tkgrab.release(base)
        tkdestroy(base)
    }
    on.exit(end())
    # When the continus button is clicked, create a phenoData object
    cont <- function(){
        newPData <- convert2PData(phenoList)
        names(varList) <- colnames(newPData)
        phenoList <- getCovarDesc(varList)
        if(!is.null(phenoList)){
            options(show.error.messages = FALSE)
            tryMe <- try(new("phenoData", pData=data.frame(newPData),
                             varLabels=phenoList))
            options(show.error.messages = TRUE)
            if(inherits(tryMe, "try-error")){
                tkmessageBox(title = paste("Data Error"),
                       message = paste("I can't create a phenoData object.",
                       "Perhaps the numbers of covariates and their",
                       "descriptions do not match."),
                       icon = "error",
                       type = "ok")
            }else{
                newPhenoData <<- tryMe
                #newPhenoData <<- newPData
                tkgrab.release(base)
                tkdestroy(base)
            }
        }
    }
    # When user decides to add new samples or covariates, reconstruct
    # pdata and update the table for user inputs

    base <- tktoplevel()
    tktitle(base) <- "BioC PhenoData Wizard"

    tkpack(tklabel(base, text = "Enter and Editor Data",
                   font = "Helvetica 11 bold"), side = "top",
           pady = 5, padx = 5, expand = FALSE)
    tkpack(tklabel(base, text = paste("Please enter pheno data using",
                        "the table below:")))
    noteFrame <- tkframe(base, borderwidth = 2, relief = "groove")
    tkpack(tklabel(noteFrame, text = paste("Cells in the first row",
                              "show covariate names and cells in the",
                         "the first column show \n sample names.",
                         "Values in all cells (including covariate/sample",
                         "names) are edit-able."),
                   justify = "left"),
                  side = "top", expand = FALSE, pady = 5)
    tkpack(noteFrame, side = "top", expand = FALSE, padx = 5, pady = 5)

    # A text widget to keep phenoData entries
    dataFrame <- tkframe(base)
    dataText <- makeViewer(dataFrame, vWidth = 85, vHeight = 16,
                           hScroll = TRUE,
                           vScroll = TRUE, what = "text", side = "left")
    tkpack(dataText, side = "top", expand = TRUE, fill = "both")
    tkpack(dataFrame, side = "top", expand = TRUE, fill = "both",
           padx = 5)

    butFrame <- tkframe(base)
    backBut <- tkbutton(butFrame, text = "Cancel", width = 8, command = end)
    contBut <- tkbutton(butFrame, text = "Continue", width = 8,
                    command = cont)
    tkgrid(contBut, backBut, padx = 20)
    tkpack(butFrame, expand = FALSE, fill = "x", padx = 5, pady = 5)

    tkgrab.set(base)

    phenoList <- writePhenoTable(base, dataText, pdata)


    tkwait.window(base)

    return(newPhenoData)
}
# Write data contained by pdata to the text widget containing the
# table for user inputs
writePhenoTable <- function(base, textWidget, pdata){
    tkconfigure(textWidget, state = "normal")
    phenoMat <- makePhenoData(pdata)
    values <- list()
    tkdelete(textWidget, "0.0", "end")
    #tempEntry <- list()
    for(i in 1:nrow(phenoMat)){
        tempList <- list()
        for(j in 1:ncol(phenoMat)){
            if(i == 1 || j == 1 ){
                style <- "raised"
            }else{
                style <- "sunken"
            }
            if(i == 1  && j == 1 ){
                state <- "disabled"
            }else{
                state <- "normal"
            }
            tempList[[j]] <- tclVar(phenoMat[i, j])
            tempEntry <- tkentry(base, textvariable = tempList[[j]],
                                state = state, width = 11, relief = style)
            tkwindow.create(textWidget, "end", window = tempEntry)
        }
        tkinsert(textWidget, "end", "\n")
        values[[i]] <- tempList
    }
    tkconfigure(textWidget, state = "disabled")
    return(values)
}

# Constructs a matrix containing user input data
makePhenoData <- function(pdata){
        temp <- rbind(c("", colnames(pdata)),
                      cbind(rownames(pdata), as.matrix(pdata)))
    return(as.matrix(temp))
}

# Conver values in a matrix containing user input data to a phenoData
# object
convert2PData <- function(phenoList){
    pdata <- NULL
    varlist <- list()
    cnames <- NULL
    for(i in 1:length(phenoList)){
        tempP <- NULL
        temp <- phenoList[[i]]
        for(j in 1:length(temp)){
            tempP <- c(tempP, tclvalue(temp[[j]]))
        }
        pdata <- rbind(pdata, tempP)
    }
    #pdata <- pdata[, pdata[1,] != "Delete"]
    #pdata <- pdata[pdata[,1] != "Delete",]
    #varlist <- as.list(pdata[2, 2:ncol(pdata)])
    rnames <- pdata[2:nrow(pdata), 1]
    cnames <- pdata[1, 2:ncol(pdata)]
    pdata <- pdata[-1, -1]
    if(is.null(nrow(pdata))){
        pdata <- data.frame(matrix(pdata, ncol = 1))
    }
    rownames(pdata) <- rnames
    colnames(pdata) <- cnames
    #names(varlist) <- as.character(1:length(varlist))
#    return(new("phenoData", pData=data.frame(pdata), varLabels=varlist))
    return(pdata)
}

# A widget that takes user inputs for covariate descriptions
# varList - a list with names being covariate names and values
# being a short description of the covariate
getCovarDesc <- function(varList){
    toReturn <- list()
    covarDesc <- list()

    end <- function(){
        temp <- sapply(covarDesc, tclvalue)
        toReturn[names(temp)] <<- temp
        tkgrab.release(base)
        tkdestroy(base)
    }
    on.exit(end())

    cancel <- function(){
        toReturn <<- NULL
        tkgrab.release(base)
        tkdestroy(base)
    }
    base <- tktoplevel()
    tktitle(base) <- "BioC PhenoData Wizard"

    tkpack(tklabel(base, text = "Describe Variables",
                   font = "Helvetica 11 bold"), side = "top",
           pady = 5)
    tkpack(tklabel(base, text = paste("Please enter/edit a short textual",
                         "description for each covariate")), pady = 2)

    # A text widget to keep description entries
    dataFrame <- tkframe(base)
    dataText <- makeViewer(dataFrame, vWidth = 85, vHeight = 16,
                           hScroll = TRUE,
                           vScroll = TRUE, what = "text", side = "left")
    tkpack(dataText, side = "top", expand = TRUE, fill = "both")
    tkpack(dataFrame, side = "top", expand = TRUE, fill = "both",
           padx = 5, pady = 5)

    butFrame <- tkframe(base)
    backBut <- tkbutton(butFrame, text = "Cancel", width = 8,
                        command = cancel)
    contBut <- tkbutton(butFrame, text = "Continue", width = 8,
                    command = end)
    tkgrid(contBut, backBut, padx = 20)
    tkpack(butFrame, expand = FALSE, fill = "x", padx = 5, pady = 5)

    for(i in names(varList)){
        tkinsert(dataText, "end", paste(i, " "))
        covarDesc[[i]] <- tclVar(varList[[i]])
        tempEntry <- tkentry(base, textvariable = covarDesc[[i]],
                             width = 60)
        tkwindow.create(dataText, "end", window = tempEntry)
        tkinsert(dataText, "end", "\n")
    }
    tkconfigure(dataText, state = "disabled")

    tkgrab.set(base)

    tkwait.window(base)

    return(toReturn)
}








