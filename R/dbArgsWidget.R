# This function provides the interface for the inputs for making the
# connection to a database.
#
# Copyright 2002, J. Zhang, all rights reserved
#

dbArgsWidget <- function(){

    # Sets the working environment
    PWEnv <- new.env(hash = TRUE, parent = NULL)
    # Defines the widget components
    label1 <- label(name = "label1", value = "Database: ", width = 10,
                    env = PWEnv)
    db <- entryBox(name = "db", width = 20, env = PWEnv)
    label2 <- label(name = "label2", value = "User name: ", width = 10,
                    env = PWEnv)
    un <- entryBox(name = "un", width = 20, env = PWEnv)
    label3 <- label(name = "label3", value = "Password: ", width = 10,
                    env = PWEnv)
    pw <- entryBox(name = "pw", width = 20, env = PWEnv)
    label4 <- label(name = "label4", value = "Host: ", width = 10,
                    env = PWEnv)
    hs <- entryBox(name = "hs", value = "localhost", width = 20,
                   env = PWEnv)
    # Makes a list with the layout defined
    pWidgets <- list(dbName = list(label1 = label1, db = db),
                     userName = list(label2 = label2, un = un),
                     password = list(label3 = label3, pw = pw),
                     host = list(label4 = label4, hs = hs))
    # Constructs the interface
    widget <- widget(wTitle = "BioC DB Inputs Widget", pWidgets,
                     funs = list(), preFun = function() {},
                     postFun = function() {}, env = PWEnv)

    # Returns the input values
    if(value(pWidgets(widget)[["dbName"]][["db"]]) == ""){
       stop("Database name can not be an empty string!")
    }
    inputs <- list(dbname = value(pWidgets(widget)[["dbName"]][["db"]]),
                   host = value(pWidgets(widget)[["host"]][["hs"]]))
    if(value(pWidgets(widget)[["userName"]][["un"]]) != ""){
        inputs[["user"]] <- value(pWidgets(widget)[["userName"]][["un"]])
    }
    if(value(pWidgets(widget)[["password"]][["pw"]]) != ""){
        inputs[["password"]] <- value(pWidgets(widget)[["password"]][["pw"]])
    }

    return(inputs)
}
