# These are the functions used by widgetRender

WName <- function(x) x$Name
WValue <- function(x) x$Value
"WValue<-" <- function(x, value) {x$Value <- value; x}
WtoText <- function(x) x$toText
WfromText <- function(x) x$fromText
WcanEdit <- function(x) x$canEdit
WbuttonText <- function(x) x$buttonText
WbuttonFun <- function(x) x$buttonFun

WwList <- function(x) x$wList
WListByName <- function(x, name) {
    eval(substitute(listByName <- x$aName, list(aName = name)))
    return (listByName)
}
WpreFun <- function(x) x$preFun
WpostFun <- function(x) x$postFun

