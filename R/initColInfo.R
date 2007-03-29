# Functions that initializes or constructs a colInfo class/object
# name - the name for a column
# type - data type for a column
# dropOrNot - a boolean indicating whether the column will be droped

    setClass("colInfo", representation(colName = "character",
                                       colType = "character",
                                       dropOrNot = "logical"))
    # Set the get methods
    setGeneric("colName",
               function(object) standardGeneric("colName"))
    setMethod("colName", "colInfo",
              function(object) object@colName)
    
    setGeneric("colType",
               function(object) standardGeneric("colType"))
    setMethod("colType", "colInfo",
              function(object) object@colType)
    
    setGeneric("dropOrNot",
               function(object) standardGeneric("dropOrNot"))
    setMethod("dropOrNot", "colInfo",
              function(object) object@dropOrNot)
    # Define the replace methods
    setGeneric("colName<-", function(object, value)
                   standardGeneric("colName<-"))
    setReplaceMethod("colName", "colInfo", function(object, value){
                  object@colName <- value; object})
    
    setGeneric("colType<-", function(object, value)
                   standardGeneric("colType<-"))
    setReplaceMethod("colType", "colInfo", function(object, value){
                  object@colType <- value; object})
  
    setGeneric("dropOrNot<-", function(object, value)
                   standardGeneric("dropOrNot<-"))
    setReplaceMethod("dropOrNot", "colInfo", function(object, value){
                  object@dropOrNot <- value; object})

colInfo <- function(name, type, drop){
    new("colInfo", colName = name, colType = type, dropOrNot = drop)
}
