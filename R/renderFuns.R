##they provide a well defined interface whose underlying implementation 
##can easily be altered for other widget packages
# These are the functions used by widgetRender
##FIXME: could you document what they do please?

WName <- function(x) x$Name
WValue <- function(x) x$Value
"WValue<-" <- function(x, value) {x$Value <- value; x}
WtoText <- function(x) x$toText
WfromText <- function(x) x$fromText
WcanEdit <- function(x) x$canEdit
WbuttonText <- function(x) x$buttonText
WbuttonFun <- function(x) x$buttonFun

#get and set the widget list
##FIXME: shouldn't these be getWidgetList and setWidgetList?
WwList <- function(x) x$wList
"WwList<-" <- function(x, value) {x$wList <- value; x}

##what about getIndexValuepWidget and setIndexValuepWidget
#get and set index' pWidgets value
WLValue <- function(x, index) WValue(WwList(x)[[index]])
"WLValue<-" <- function(x, index, value)
    {WValue(WwList(x)[[index]]) <- value; x}


WpreFun <- function(x) x$preFun
WpostFun <- function(x) x$postFun
WRButtons <- function(x) x$buttons
WEnd <- function(x) x$end

