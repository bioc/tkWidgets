# These are the functions used by widgetRender

WName <- function(x) x$Name
WValue <- function(x) x$Value

"WValue<-" <- function(x, value) {x$Value <- value; x}

WtoText <- function(x) x$toText

WfromText <- function(x) x$fromText
WcanEdit <- function(x) x$canEdit
WbuttonText <- function(x) x$buttonText
WbuttonFun <- function(x) x$buttonFun

WpreFun <- function(x) x$preFun
WpostFun <- function(x) x$postFun

