# Function widgetRender takes a list of a widget list that has a name,
# value, ... buttonText, and buttonFun as shown below plus a
# prefunction, postfunction, and a few buttons. The functions listed
# here provide a defined interface for accessing and manipulate these
# values. The functions are currently temporary and will be modified
# for better names.

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
"WwList<-" <- function(x, value) {x$wList[names(value)] <- value; x}

##what about getIndexValuepWidget and setIndexValuepWidget
#get and set index' pWidgets value
WLValue <- function(x, index) WValue(WwList(x)[[index]])
"WLValue<-" <- function(x, index, value)
    {WValue(WwList(x)[[index]]) <- value; x}


WpreFun <- function(x) x$preFun
WpostFun <- function(x) x$postFun
WRButtons <- function(x) x$buttons
WEnd <- function(x) x$end

