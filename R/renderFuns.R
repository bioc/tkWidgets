# These are the functions used by widgetRender

WName <- function(x) x$Name
WValue <- function(x) x$Value
"WValue<-" <- function(x, value) {x$Value <- value; x}
WtoText <- function(x) x$toText
WfromText <- function(x) x$fromText
WcanEdit <- function(x) x$canEdit
WbuttonText <- function(x) x$buttonText
WbuttonFun <- function(x) x$buttonFun

#get and set the widget list
WwList <- function(x) x$wList
"WwList<-" <- function(x, value) {x$wList <- value; x}

#get and set index' pWidgets value
WLValue <- function(x, index) WValue(WwList(x)[[index]])
"WLValue<-" <- function(x, index, value)
    {WValue(WwList(x)[[index]]) <- value; x}

WpreFun <- function(x) x$preFun
WpostFun <- function(x) x$postFun
WEnd <- function(x) x$end

