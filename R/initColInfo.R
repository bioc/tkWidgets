# Functions that initializes or constructs a colInfo class/object
# name - the name for a column
# type - data type for a column
# dropOrNot - a boolean indicating whether the column will be droped

    setClass("colInfo", representation(name = "character",
                                       type = "character",
                                       dropOrNot = "logical"))
    # Set the get methods
    if(!isGeneric("name")){
        setGeneric("name",
                   function(object) standardGeneric("name"))
    }
    setMethod("name", "colInfo",
              function(object) object@name)
    if(!isGeneric("type")){
        setGeneric("type",
                   function(object) standardGeneric("type"))
    }
    setMethod("type", "colInfo",
              function(object) object@type)
    if(!isGeneric("dropOrNot")){
        setGeneric("dropOrNot",
                   function(object) standardGeneric("dropOrNot"))
    }
    setMethod("dropOrNot", "colInfo",
              function(object) object@dropOrNot)
    # Define the replace methods
    if(!isGeneric("name<-")){
        setGeneric("name<-", function(object, value)
                   standardGeneric("name<-"))
    }
    setReplaceMethod("name", "colInfo", function(object, value){
                  object@name <- value; object})
    if(!isGeneric("type<-")){
        setGeneric("type<-", function(object, value)
                   standardGeneric("type<-"))
    }
    setReplaceMethod("type", "colInfo", function(object, value){
                  object@type <- value; object})
    if(!isGeneric("dropOrNot<-")){
        setGeneric("dropOrNot<-", function(object, value)
                   standardGeneric("dropOrNot<-"))
    }
    setReplaceMethod("dropOrNot", "colInfo", function(object, value){
                  object@dropOrNot <- value; object})

colInfo <- function(name, type, drop){
    new("colInfo", name = name, type = type, dropOrNot = drop)
}
