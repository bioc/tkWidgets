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
                pdata <<- tryMe
                newPhenodata <<- createPhenoData(pdata, sampleNames)
            }
        }
    }
    readObj <- function(){
        if(tclvalue(objName) == ""){
            showIOError("data frame")
        }else{
            if(is.null(pdata)){
                pdata <<- get(tclvalue(objName))
            }
            newPhenodata <<- createPhenoData(pdata, sampleNames)
        }
    }
    editObj <- function(){
        #if(tclvalue(phenoName) == ""){
        #    showIOError("phenoData")
        #}else{
            if(is.null(phenodata)){
                if(tclvalue(phenoName) != ""){
                    phenodata <<- get(tclvalue, env = .GlobalEnv)
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
        filter <- function(x){
            if(is.data.frame(get(x)))
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
}

# Shows an erro message when no name has been entered
showIOError <- function(what){
    tkmessageBox(title = paste(what, "name missing"),
                 message = paste("Please enter a", what, "name first"),
                 icon = "error",
                 type = "ok")
    return(invisible())
}

createPhenoData <- function(pdata, sampleNames){
    if(is.null(pdata)){
        covarNum <- tclVar()
    }else{
        covarNum <- tclVar(ncol(pdata) - 1)
    }

    end <- function(){
         tkdestroy(base)
    }
    on.exit(end())

    cont <- function(){
        end()
    }

    base <- tktoplevel()
    tktitle(base) <- "BioC PhenoData Wizard"

    tkpack(tklabel(base, text = paste("Please enter pheno data using",
                        "the widgets below")),
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
    tkpack(covarDrop, side = "top", expand = TRUE, fill = "x", padx = 5)

    # A text widget to keep phenoData entries
    dataFrame <- tkframe(base)
    dataText <- makeViewer(dataFrame, vWidth = 30, vHeight = 15,
                           hScroll = TRUE,
                           vScroll = TRUE, what = "text", side = "left",
                           text = "editable cells to be implemented")
    tkpack(dataText, side = "top", expand = TRUE, fill = "both")
    tkpack(dataFrame, side = "top", expand = TRUE, fill = "both",
           padx = 5)

    butFrame <- tkframe(base)
    backBut <- tkbutton(butFrame, text = "Back", width = 8, command = end)
    contBut <- tkbutton(butFrame, text = "Continue", width = 8,
                    command = cont)
    tkgrid(backBut, contBut, padx = 20)
    #tkgrid.configure(backBut, sticky = "w")
    #tkgrid.configure(contBut, sticky = "e")
    tkpack(butFrame, expand = FALSE, fill = "x", padx = 5, pady = 5)

    tkwait.window(base)

}








