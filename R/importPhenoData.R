# Functions that quide users through the steps of inporting a
# phenoData object. Used by the affy package.

importPhenoData <- function(sampleNames = NULL){

    fileName <- tclVar()
    objName <- tclVar()
    phenoName <- tclVar()
    pdata <- NULL
    varLabels <- NULL
    phenodata <- NULL
    newPhenoData <- NULL

    end <- function(){
         tkdestroy(base)
    }
    on.exit(end())

    cancel <- function(){
        end()
    }
    # Read from file using read.table and create a phenoData.
    readFile <- function(){
        if(tclvalue(fileName) == ""){
            showIOError("file")
        }else{
            args <- formals("read.table")[c("file", "header", "sep",
                                        "nrows", "skip", "strip.white",
                                        "comment.char")]
            #args[["fill"]] <- !args[["blank.lines.skip"]]
            args[["file"]] <- tclvalue(fileName)
            args <- argsWidget(args,
                               inst = "Arguments for function read.table")
            #args[["row.names"]] <- sampleNames
            options(show.error.messages = FALSE)
            tryMe<- try(do.call("read.table", as.list(args)))
            options(show.error.messages = TRUE)
            if(inherits(tryMe, "try-error")){
                tkmessageBox(title = paste(what, "read.table error"),
                     message = paste("read.table failed because of",
                                    tryMe[1]),
                     icon = "error",
                     type = "ok")
            }else{
                pdata <<- sNames4rNames(tryMe, sampleNames)
                if(!is.null(pdata)){
                    newPhenoData <<- createPhenoData(pdata, sampleNames)
                    if(!is.null(newPhenoData)){
                        end()
                    }
                }
            }
        }
    }
    # Read from an existing data from in .GlobalEnv and create a phenoData
    readObj <- function(){
        if(tclvalue(objName) == ""){
            showIOError("data frame")
        }else{
            if(is.null(pdata)){
                pdata <<- .GlobalEnv[[tclvalue(objName)]]
            }
            pdata <<- sNames4rNames(pdata, sampleNames)
            if(!is.null(pdata)){
                newPhenoData <<- createPhenoData(pdata, sampleNames)
                if(!is.null(newPhenoData)){
                    end()
                }
            }
        }
    }
    # Modify pData of an existing phenoData in .GlobalEnv and create a
    # phenoData. If no name is provided, create a new one
    editObj <- function(){
        if(tclvalue(phenoName) == ""){
            yesno <- tkmessageBox(title = "New phenoData",
                     message = "No name provided. Create new?",
                     icon = "question",
                     type = "yesno")
            if(tclvalue(yesno) == "yes"){
                if(!is.null(sampleNames)){
                    pdata <<- sNames4rNames(data.frame(matrix("",
                                    ncol = 1, nrow = length(sampleNames))),
                                            sampleNames)
                }else{
                    pdata <<- data.frame(matrix("", ncol = 1, nrow = 1))
                }
                colnames(pdata) <<- paste("Covar1")
                newPhenoData <<- createPhenoData(pdata, sampleNames)
                if(!is.null(newPhenoData)){
                    end()
                }
            }
        }else{
            if(is.null(phenodata)){
                if(tclvalue(phenoName) != ""){
                    phenodata <<- .GlobalEnv[[tclvalue(phenoName)]]
                    pdata <<- pData(phenodata)
                }
            }else{
                pdata <<- pData(phenodata)
            }
            pdata <<- sNames4rNames(pdata, sampleNames)
            newPhenoData <<- createPhenoData(pdata, sampleNames)
            if(!is.null(newPhenoData)){
                end()
            }
        }
    }
    # Get (browse) the name of a file to be read in by read.table
    brows <- function(){
        tclvalue(fileName) <<- tclvalue(tkcmd("tk_getOpenFile"))
    }
    # Get (browse) the name of an existing data frame
    browseObj <- function(){
        filter <- function(x, env = .GlobalEnv){
            if(is.data.frame(env[[x]]))
                return(TRUE)
            else
                return(FALSE)
        }
        obj <- objectBrowser(nSelect = 1, fun = filter)
        if(!is.null(obj)){
            tclvalue(objName) <<- names(obj)
            pdata <<- obj[[1]]
        }
    }
    # Get (browse) the name of an existing phenoData object
    browsePheno <- function (){
        filter <- function(x, env = .GlobalEnv){
            if(class(env[[x]]) == "phenoData")
                return(TRUE)
            else
                return(FALSE)
        }
        obj <- objectBrowser(nSelect = 1, fun = filter)
        if(!is.null(obj)){
            tclvalue(phenoName) <<- names(obj)
            phenodata <<- obj[[1]]
        }
    }

    base <- tktoplevel()
    tktitle(base) <- "BioC Read PhenoData"

    tkpack(tklabel(base, text = paste("Welcome! I will guide you through",
                        "the steps needed to input your phenoData")),
                  side = "top", expand = FALSE, padx = 5, pady = 5)
    noteFrame <- tkframe(base, borderwidth = 2, relief = "groove")
    tkpack(tklabel(noteFrame, text = paste(".Read From File: create a",
                              "phenoData using a file name typed/browsed in.",
                              "\n.Read From Object: create a phenoData",
                              "using an existing data frame in .GlobalEnv.",
                              "\n Type/browse in the name of the data frame.",
                              "\n.Edit/Creat: Edit an existing phenoData",
                              "(name needed) in .GlobalEnv or create",
                              "\n a new phenoData (name not needed)."),
                   justify = "left"),
                  side = "top", expand = FALSE, pady = 5)
    tkpack(noteFrame, side = "top", expand = FALSE, pady = 5, padx = 5)
    # Interface for reading from a file
    readFrame <- tkframe(base)
    fileBut <- tkbutton(readFrame, text = "Read From File", width = 16,
                    command = readFile)
    tkpack(fileBut, side = "left", expand = FALSE, padx = 5)
    tkpack(tklabel(readFrame, text = "File name:", width = 14,
                   justify = "left"), side = "left", expand = FALSE)
    tkpack(tkentry(readFrame, width = 50, textvariable = fileName),
           side = "left", expand = TRUE, fill = "x")
    tkpack(tkbutton(readFrame, text = "Browse", command = brows),
           side = "left", expand = FALSE)
    tkpack(readFrame, side = "top", padx = 5, expand = TRUE,
           fill = "x")

    # Interface for reading from an R object
    objFrame <- tkframe(base)
    objBut <- tkbutton(objFrame, text = "Read From Object", width = 16,
                    command = readObj)
    tkpack(objBut, side = "left", expand = FALSE, padx = 5)
    tkpack(tklabel(objFrame, text = "Object name:", width = 14,
                   justify = "left"), side = "left", expand = FALSE)
    tkpack(tkentry(objFrame, width = 50, textvariable = objName),
           side = "left", expand = TRUE, fill = "x")
    tkpack(tkbutton(objFrame, text = "Browse", command = browseObj),
           side = "left", expand = FALSE)
    tkpack(objFrame, side = "top", padx = 5, expand = TRUE,
           fill = "x")

    # Interface for editing/creating a phenoData
    editFrame <- tkframe(base)
    editBut <- tkbutton(editFrame, text = "Edit/Create", width = 16,
                    command = editObj)
    tkpack(editBut, side = "left", expand = FALSE, padx = 5)
    tkpack(tklabel(editFrame, text = "phenoData name:", width = 14,
                   justify = "left"), side = "left", expand = FALSE)
    tkpack(tkentry(editFrame, width = 50, textvariable = phenoName),
           side = "left", expand = TRUE, fill = "x")
    tkpack(tkbutton(editFrame, text = "Browse", command = browsePheno),
           side = "left", expand = FALSE)
    tkpack(editFrame, side = "top", padx = 5, expand = TRUE,
           fill = "x")
    tkpack(tkbutton(base, text = "Cancel", command = cancel, width = 15),
           side = "top", anchor = "center", expand = FALSE, pady = 10)

    tkwait.window(base)

    return(newPhenoData)
}

# Shows an erro message when no name has been entered
showIOError <- function(what){
    tkmessageBox(title = paste(what, "name missing"),
                 message = paste("Please enter a", what, "name first"),
                 icon = "error",
                 type = "ok")
    return(invisible())
}

# Put sample names as row names of the data frame
sNames4rNames <- function(pdata, sampleNames){
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
                         "\nContinue?"),
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
createPhenoData <- function(pdata, sampleNames){
    phenoObj <- NULL
    phenoList <- NULL
    addNum <- tclVar()
    rowOrCol <- tclVar()

    if(is.null(pdata)){
        covarNum <- 1
    }else{
        covarNum <- ncol(pdata)
    }

    end <- function(){
        tkgrab.release(base)
        tkdestroy(base)
    }
    on.exit(end())
    # When the continus button is clicked, create a phenoData object
    cont <- function(){
        phenoObj <<- convert2PData(phenoList)
        end()
    }
    # When user decides to add new samples or covariates, reconstruct
    # pdata and update the table for user inputs
    changeCovar <- function(){
        if(tclvalue(rowOrCol) == "Covariates"){
            covarNum <<- covarNum + as.numeric(tclvalue(addNum))
        }
        pdata <<- modPData(pdata, tclvalue(rowOrCol),
                           as.numeric(tclvalue(addNum)))
        phenoList <<- writePhenoTable(base, dataText, pdata, sampleNames,
                                 covarNum)
    }

    base <- tktoplevel()
    tktitle(base) <- "BioC PhenoData Wizard"

    tkpack(tklabel(base, text = paste("Please enter pheno data using",
                        "the table below:")), pady = 5)
    noteFrame <- tkframe(base, borderwidth = 2, relief = "groove")
    tkpack(tklabel(noteFrame, text = paste(".Cells in the first column/row",
                         "with Ratain/Delete indicates whether a column/row",
                        "\n will be ratained or deleteded.",
                         "\n.The second row shows covariate names and",
                         "the second column shows sample names.",
                         "\n.Empty cells right below covariate names are for",
                         "entries for desciptions of covariates.",
                         "\n.Values in all cells are editable."),
                   justify = "left"),
                  side = "top", expand = FALSE, pady = 5)
    tkpack(noteFrame, side = "top", expand = FALSE, padx = 5)
    # Dropdown list for covariate numbers
    covarDrop <- tkframe(base)
    tkpack(tklabel(covarDrop, text = "Add "), side = "left", expand = FALSE)
    numFrame <- tkframe(covarDrop)
    dropdownList(numFrame, as.character(0:20), addNum, 3, "0", TRUE)
    tkpack(numFrame, side = "left", expand = FALSE, fill = "x")
    tkpack(tklabel(covarDrop, text = " new "), side = "left",
           expand = FALSE)
    rowFrame <- tkframe(covarDrop)
    dropdownList(rowFrame, c("Samples", "Covariates"), rowOrCol, 10,
                 "Samples", TRUE)
    tkpack(rowFrame, side = "left", expand = TRUE, fill = "x")
    tkpack(tkbutton(covarDrop, text = "Apply", width = 7,
                    command = changeCovar), expand = FALSE)
    tkpack(covarDrop, side = "top", expand = TRUE, fill = "x",
           padx = 5, pady = 5)

    # A text widget to keep phenoData entries
    dataFrame <- tkframe(base)
    dataText <- makeViewer(dataFrame, vWidth = 85, vHeight = 15,
                           hScroll = TRUE,
                           vScroll = TRUE, what = "text", side = "left")
    tkpack(dataText, side = "top", expand = TRUE, fill = "both")
    tkpack(dataFrame, side = "top", expand = TRUE, fill = "both",
           padx = 5)

    butFrame <- tkframe(base)
    backBut <- tkbutton(butFrame, text = "Back", width = 8, command = end)
    contBut <- tkbutton(butFrame, text = "Continue", width = 8,
                    command = cont)
    tkgrid(backBut, contBut, padx = 20)
    tkpack(butFrame, expand = FALSE, fill = "x", padx = 5, pady = 5)

    tkgrab.set(base)

    phenoList <- writePhenoTable(base, dataText, pdata, sampleNames,
                                 covarNum)

    tkwait.window(base)

    return(phenoObj)
}
# Write data contained by pdata to the text widget containing the
# table for user inputs
writePhenoTable <- function(base, textWidget, pdata, sampleNames,
                            covarNum){
    phenoMat <- makePhenoData(pdata, sampleNames, covarNum)
    values <- list()
    tkdelete(textWidget, "0.0", "end")
    #tempEntry <- list()
    for(i in 1:nrow(phenoMat)){
        tempList <- list()
        for(j in 1:ncol(phenoMat)){
            if(i < 3 || j < 3 || (i == 3 && j == 2)){
                style <- "raised"
            }else{
                style <- "sunken"
            }
            if(i < 4 && j < 3){
                state <- "disabled"
            }else{
                state <- "normal"
            }
            tempList[[j]] <- tclVar(phenoMat[i, j])
            tempEntry <- tkentry(base, textvariable = tempList[[j]],
                                state = state, width = 11, relief = style)
            if((i > 2 && j == 1) || (i == 1 && j > 2)){
                fun <- function() {}
                body <- list(as.name("{"),
                             substitute(temp <- tclvalue(tkget(tempEntry))),
                             substitute(tkdelete(tempEntry, 0, "end")),
                             substitute(tkinsert(tempEntry, "end",
                               ifelse(temp == "Retain", "Delete", "Retain"))))
                body(fun) <- as.call(body)
                tkbind(tempEntry, "<B1-ButtonRelease>", fun)
            }
            tkwindow.create(textWidget, "end", window = tempEntry)
        }
        tkinsert(textWidget, "end", "\n")
        values[[i]] <- tempList
    }
    return(values)
}

# Constructs a matrix containing user input data
makePhenoData <- function(pdata, sampleNames, covarNum){
    if(!is.null(pdata)){
        #if(covarNum == ncol(pdata)){
            temp <- rbind(c("", "", rep("Retain", ncol(pdata))),
                          c("", "", colnames(pdata)), rep("",
                                                    (ncol(pdata) + 2)),
                          as.matrix(cbind(rep("Retain",
                                              length(rownames(pdata))),
                                                  rownames(pdata), pdata)))
        #}else if(covarNum > ncol(pdata)){
         #   temp <- rbind(c("", "", rep("Retain", covarNum)),
         #                 c("", "", colnames(pdata),
         #                   rep("", nrow(pdata)),
         #               paste("CoVar", (ncol(pdata)+1):covarNum, sep = "")),
         #                 as.matrix(cbind(rownames(pdata),
         #                pdata, matrix("", ncol = covarNum - ncol(pdata),
         #                              nrow = length(sampleNames)))))
        #}else{
        #    temp <- rbind(c("", colnames(pdata)[1:covarNum]),
        #                  rep("", nrow(pdata)),
        #                  as.matrix(cbind(rownames(pdata),
        #                 pdata[, 1:covarNum])))

        #}
    }else{
        temp <- rbind(c("", "", rep("Retain", covarNum)),
                      c("", "", paste("Covar", 1:covarNum, sep = "")),
                      rep("", covarNum + 2),
                      as.matrix(cbind(rep("Retain", length(sampleNames)),
                                      sampleNames, matrix("",
                      ncol = covarNum, nrow = length(sampleNames)))))
    }
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
    pdata <- pdata[, pdata[1,] != "Delete"]
    pdata <- pdata[pdata[,1] != "Delete",]
    varlist <- as.list(pdata[2, 3:ncol(pdata)])
    rnames <- pdata[4:nrow(pdata), 2]
    cnames <- pdata[2, 3:ncol(pdata)]
    pdata <- pdata[-(1:3), -(1:2)]
    if(is.null(nrow(pdata))){
        pdata <- data.frame(matrix(pdata, ncol = 1))
    }
    rownames(pdata) <- rnames
    colnames(pdata) <- cnames
    names(varlist) <- as.character(1:length(varlist))

    return(new("phenoData", pData=data.frame(pdata), varLabels=varlist))

}

# Function that add new rows/columns to pdata
modPData <- function(pdata, rowOrCol, addNum){
    if(rowOrCol == "Covariates" && addNum > 0){
        temp <- cbind(pdata,
                      matrix("", nrow  = nrow(pdata),
                             ncol = addNum))
        colnames(temp) <- c(colnames(pdata), paste("Covar",
                       (ncol(pdata) + 1):(ncol(pdata) + addNum), sep = ""))

    }else{
        temp <- rbind(as.matrix(pdata), matrix("",nrow = addNum,
                                               ncol = ncol(pdata)))
        rownames(temp) <- c(rownames(pdata), cbind(paste("Sample",
                      (nrow(pdata) + 1):(nrow(pdata) + addNum), sep = "")))
    }
    return(temp)
}







