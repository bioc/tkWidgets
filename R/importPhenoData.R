# Functions that quide users through the steps of inporting a
# phenoData object. Used by the affy package.

importPhenoData <- function(sampleNames){

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
            args[["row.names"]] <- sampleNames
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
                    newPhenodata <<- createPhenoData(pdata, sampleNames)
                    end()
                }
            }
        }
    }
    readObj <- function(){
        if(tclvalue(objName) == ""){
            showIOError("data frame")
        }else{
            if(is.null(pdata)){
                pdata <<- get(tclvalue(objName), env = .GlobalEnv)
            }
            pdata <- sNames4rNames(pdata, sampleNames)
            if(!is.null(pdata)){
                newPhenodata <<- createPhenoData(pdata, sampleNames)
                end()
            }
        }
    }
    editObj <- function(){
        #if(tclvalue(phenoName) == ""){
        #    showIOError("phenoData")
        #}else{
            if(is.null(phenodata)){
                if(tclvalue(phenoName) != ""){
                    phenodata <<- get(tclvalue(phenoName), env = .GlobalEnv)
                    pdata <<- pData(phenodata)
                }
            }else{
                pdata <<- pData(phenodata)
            }
            newPhenoData <<- createPhenoData(pdata, sampleNames)
        #}
    }
    brows <- function(){
        tclvalue(fileName) <<- tclvalue(tkcmd("tk_getOpenFile"))
    }
    browseObj <- function(){
        filter <- function(x, env = .GlobalEnv){
            if(is.data.frame(get(x, env = env)))
                return(TRUE)
            else
                return(FALSE)
        }
        pData <<- objectBrowser(nSelect = 1, fun = filter)
        if(!is.null(pData)){
            tclvalue(objName) <<- names(pData)
        }
    }
    browsePheno <- function (){
        filter <- function(x){
            if(class(get(x)) == "phenoData")
                return(TRUE)
            else
                return(FALSE)
        }
        phenodata <<- objectBrowser(nSelect = 1, fun = filter)
        if(!is.null(phenodata)){
            tclvalue(phenoName) <<- names(phenoData)
        }
    }

    base <- tktoplevel()
    tktitle(base) <- "BioC Read PhenoData"

    tkpack(tklabel(base, text = paste("Welcome! I will guide you through",
                        "the steps needed to input your phenoData")),
                  side = "top", expand = FALSE, padx = 5, pady = 5)
    tkpack(tklabel(base, text = "Please choose from the options below:"),
                   side = "top", expand = FALSE, padx = 5, pady =5)
    # Interface for reading from a file
    readFrame <- tkframe(base)
    fileBut <- tkbutton(readFrame, text = "Read From File", width = 16,
                    command = readFile)
    tkpack(fileBut, side = "left", expand = FALSE, padx = 5)
    tkpack(tklabel(readFrame, text = "File name:", width = 12),
           side = "left", expand = FALSE)
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
    tkpack(tklabel(objFrame, text = "Object name:", width = 12),
           side = "left", expand = FALSE)
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
    tkpack(tklabel(editFrame, text = "phenoData name:", width = 12),
           side = "left", expand = FALSE)
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
    options(show.error.messages = FALSE)
    tryMe <- try(rownames(pdata) <- sampleNames)
    options(show.error.messages = TRUE)
    if(inherits(tryMe, "try-error")){
        ok <- tkmessageBox(title = "Sample mis-match",
                     message = paste("Length of sample names and",
                         "row numbers of data frame do not match"),
                     icon = "error",
                     type = "ok")
        return(NULL)
    }
    return(tryMe)
}
createPhenoData <- function(pdata, sampleNames){
    phenoObj <- NULL
    phenoList <- NULL

    if(is.null(pdata)){
        covarNum <- tclVar()
    }else{
        covarNum <- tclVar(ncol(pdata))
    }

    end <- function(){
         tkdestroy(base)
    }
    on.exit(end())

    cont <- function(){
        phenoObj <<- convert2PData(phenoList)
        end()
    }
    changeCovar <- function(){
        phenoList <- writePhenoTable(base, dataText, pdata, sampleNames,
                                 as.numeric(tclvalue(covarNum)))
    }

    base <- tktoplevel()
    tktitle(base) <- "BioC PhenoData Wizard"

    tkpack(tklabel(base, text = paste("Please enter pheno data using",
                        "the table below\n\nThe first column shows sample",
                         "names and the first row shows covariate names.",
                         "\nEmpty cells right below covariate names are for",
                         "entries for desciptions \nof covariates."),
                   justify = "left"),
                  side = "top", expand = FALSE, padx = 5, pady = 5)
    # Dropdown list for covariate numbers
    covarDrop <- tkframe(base)
    tkpack(tklabel(covarDrop, text = "Number of covariate:"),
           side = "left", expand = FALSE)
    drop <- tkframe(covarDrop)
    dropdownList(drop, 1:20, covarNum, 15,
                 ifelse(tclvalue(covarNum) == "", 1,
                        tclvalue(covarNum)), TRUE)
    tkpack(drop, side = "left", expand = TRUE, fill = "x")
    tkpack(tkbutton(covarDrop, text = "Apply", width = 7,
                    command = changeCovar), expand = FALSE)
    tkpack(covarDrop, side = "top", expand = TRUE, fill = "x", padx = 5)

    # A text widget to keep phenoData entries
    dataFrame <- tkframe(base)
    dataText <- makeViewer(dataFrame, vWidth = 72, vHeight = 15,
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

    phenoList <- writePhenoTable(base, dataText, pdata, sampleNames,
                                 as.numeric(tclvalue(covarNum)))

    tkwait.window(base)

    return(phenoObj)
}

writePhenoTable <- function(base, textWidget, pdata, sampleNames,
                            covarNum){
    phenoMat <- makePhenoData(pdata, sampleNames, covarNum)
    values <- list()
    tkdelete(textWidget, "0.0", "end")
    for(i in 1:nrow(phenoMat)){
        tempList <- list()
        for(j in 1:ncol(phenoMat)){
            if(i == 1 || j == 1){
                style <- "raised"
            }else{
                style <- "sunken"
            }
            tempList[[j]] <- tclVar(phenoMat[i, j])
            tempEntry <- tkentry(base, textvariable = tempList[[j]],
                                   width = 11, relief = style)
            tkwindow.create(textWidget, "end", window = tempEntry)
        }
        tkinsert(textWidget, "end", "\n")
        values[[i]] <- tempList
    }
    return(values)
}

makePhenoData <- function(pdata, sampleNames, covarNum){
    if(!is.null(pdata)){
        if(covarNum == ncol(pdata)){
            temp <- rbind(c("", colnames(pdata)), rep("", nrow(pdata)),
                          as.matrix(cbind(rownames(pdata), pdata)))
        }else if(covarNum > ncol(pdata)){
            temp <- rbind(c("", colnames(pdata), rep("", nrow(pdata)),
                        paste("CoVar", (ncol(pdata)+1):covarNum, sep = "")),
                          as.matrix(cbind(rownames(pdata),
                         pdata, matrix("", ncol = covarNum - ncol(pdata),
                                       nrow = length(sampleNames)))))
        }else{
            temp <- rbind(c("", colnames(pdata)[1:covarNum]),
                          rep("", nrow(pdata)),
                          as.matrix(cbind(rownames(pdata),
                         pdata[, 1:covarNum])))

        }
    }else{
        temp <- rbind(c("", paste("Covar", 1:covarNum, sep = "")),
                      rep("", nrow(pdata)),
                      as.matrix(cbind(sampleNames, matrix("",
                      ncol = covarNum, nrow = length(sampleNames)))))
    }
    return(as.matrix(temp))
}

convert2PData <- function(phenoList){
    pdata <- NULL
    varlist <- list()
    cnames <- NULL
    for(i in 1:length(phenoList)){
        temp <- phenoList[[i]]
        tempP <- NULL
        for(j in 1:length(temp)){
            if(i > 2){
                tempP <- c(tempP, tclvalue(temp[[j]]))
            }else if (i == 2 && j != 1){
                varlist[[length(varlist) + 1]] <- tclvalue(temp[[j]])
            }else if(i == 1 && j != 1){
                cnames <- c(cnames, tclvalue(temp[[j]]))
            }
        }
        pdata <- rbind(pdata, tempP)
    }
    rnames <- pdata[,1]
    pdata <- pdata[,2:ncol(pdata)]
    rownames(pdata) <- rnames
    colnames(pdata) <- cnames
    names(varlist) <- as.character(1:length(varlist))

    return(new("phenoData", pData=data.frame(pdata), varLabels=varlist))

}









