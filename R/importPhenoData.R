# Functions that quide users through the steps of inporting a
# phenoData object. Used by the affy package.

importPhenoData <- function(){

    fileName <- tclVar()
    objName <- tclVar()
    phenoName <- tclVar()
    pdata <- NULL
    varLabels <- NULL
    phenodata <- NULL

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
            pdata <<- do.call("read.table", as.list(args))
            phenodata <<- createPhenoData(pdata)
        }
    }
    readObj <- function(){
        if(tclvalue(objName) == ""){
            showIOError("data frame")
        }else{
            if(is.null(pdata)){
                pdata <<- get(tclvalue(objName))
                phenodata <<- createPhenoData(pdata)
            }
        }
    }
    editObj <- function(){
        if(tclvalue(phenoName) == ""){
            showIOError("phenoData")
        }else{
            if(is.null(phenoData)){
                phenodata <<- get(tclvalue(phenoName), env = .GlobalEnv)
            }
            pdata <<- pData(phenodata)
            phenodata <- createPhenoData(pdata)
        }
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
    tkpack(tklabel(editFrame, text = "Object name:", width = 12),
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


showIOError <- function(what){
    tkmessageBox(title = paste(what, "name missing"),
                 message = paste("Please enter a", what, "name first"),
                 icon = "error",
                 type = "ok")
    return(invisible())
}

createPhenoData <- function(pdata){
    pdata <- as.data.frame(pdata)

}




