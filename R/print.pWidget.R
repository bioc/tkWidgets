# This function prints the values for the Name, Value, canEdit, and
# buttonText of a value containing widget element on the widget
# created by using the function widgetRender.
#
# Copyright J. Zhang 2002, all rights reserved.
#
print.pWidget <- function (x){
    print(paste("Name = ", WName(x), ";", sep = ""))
    print(paste("Value = ", WValue(x), ";", sep = ""))
    print(paste("canEdit = ", WcanEdit(x), ";", sep = ""))
    if(!is.null(WbuttonFun(x)))
       print(paste("buttonText = ", WbuttonText(x), ";", sep = ""))
}
