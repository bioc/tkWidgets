# This function provides the interface for the inputs for making the
# connection to a database.
#
# Copyright 2002, J. Zhang, all rights reserved
#

dbArgsWidget <- function(){

    # Sets the working environment
    PWEnv <- new.env(hash = TRUE, parent = emptyenv())
    # Defines the widget components
    label1 <- label(wName = "label1", wValue = "Database: ", wWidth = 15,
                    wEnv = PWEnv)
    db <- entryBox(wName = "db", wWidth = 20, wEnv = PWEnv)
    label2 <- label(wName = "label2", wValue = "User name: ", wWidth = 15,
                    wEnv = PWEnv)
    un <- entryBox(wName = "un", wWidth = 20, wEnv = PWEnv)
    label3 <- label(wName = "label3", wValue = "Password: ", wWidth = 15,
                    wEnv = PWEnv)
    pw <- entryBox(wName = "pw", wWidth = 20, wEnv = PWEnv)
    label4 <- label(wName = "label4", wValue = "Host: ", wWidth = 15,
                    wEnv = PWEnv)
    hs <- entryBox(wName = "hs", wValue = "localhost", wWidth = 20,
                   wEnv = PWEnv)
    label5 <- label(wName = "label5", wValue = "Table name: ", wWidth = 15,
                    wEnv = PWEnv)
    tn <- entryBox(wName = "tn", wWidth = 20, wEnv = PWEnv)
    # Makes a list with the layout defined
    pWidgets <- list(dbName = list(label1 = label1, db = db),
                     tableName = list(lable5 = label5, tn = tn),
                     userName = list(label2 = label2, un = un),
                     password = list(label3 = label3, pw = pw),
                     host = list(label4 = label4, hs = hs))
    # Constructs the interface
    widget <- widget(wTitle = "BioC DB Inputs Widget", pWidgets,
                     funs = list(), preFun = function() {},
                     postFun = function() {}, env = PWEnv)

    # Returns the input values
    if(wValue(pWidgets(widget)[["dbName"]][["db"]]) == ""){
       stop("Database name can not be an empty string!")
    }
    inputs <- list(dbname = wValue(pWidgets(widget)[["dbName"]][["db"]]),
                   host = wValue(pWidgets(widget)[["host"]][["hs"]]))
    if(wValue(pWidgets(widget)[["userName"]][["un"]]) != ""){
        inputs[["user"]] <- wValue(pWidgets(widget)[["userName"]][["un"]])
    }
    if(wValue(pWidgets(widget)[["password"]][["pw"]]) != ""){
        inputs[["password"]] <- wValue(pWidgets(widget)[["password"]][["pw"]])
    }
    if(wValue(pWidgets(widget)[["tableName"]][["tn"]]) != ""){
        inputs[["tableName"]] <- wValue(pWidgets(widget)[["tableName"]][["tn"]])
    }

    return(inputs)
}
