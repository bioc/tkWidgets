# Functions that initializes or constructs a colInfo class/object
# name - the name for a column
# type - data type for a column
# dropOrNot - a boolean indicating whether the column will be droped

.initColInfo <- function(where){
    setClass("colInfo", representation(name = "character",
                                       type = "character",
                                       dropOrNot = "logical"),
                                       where = where)
    # Set the get methods
    if(!isGeneric("name")){
        setGeneric("name",
                   function(object) standardGeneric("name"),
                   where = where)
    }
    setMethod("name", "colInfo",
              function(object) object@name, where = where)
    if(!isGeneric("type")){
        setGeneric("type",
                   function(object) standardGeneric("type"),
                   where = where)
    }
    setMethod("type", "colInfo",
              function(object) object@type, where = where)
    if(!isGeneric("dropOrNot")){
        setGeneric("dropOrNot",
                   function(object) standardGeneric("dropOrNot"),
                   where = where)
    }
    setMethod("dropOrNot", "colInfo",
              function(object) object@dropOrNot, where = where)
    # Define the replace methods
    if(!isGeneric("name<-")){
        setGeneric("name<-", function(object, value)
                   standardGeneric("name<-"), where = where)
    }
    setReplaceMethod("name", "colInfo", function(object, value){
                  object@name <- value; object}, where = where)
    if(!isGeneric("type<-")){
        setGeneric("type<-", function(object, value)
                   standardGeneric("type<-"), where = where)
    }
    setReplaceMethod("type", "colInfo", function(object, value){
                  object@type <- value; object}, where = where)
    if(!isGeneric("dropOrNot<-")){
        setGeneric("dropOrNot<-", function(object, value)
                   standardGeneric("dropOrNot<-"), where = where)
    }
    setReplaceMethod("dropOrNot", "colInfo", function(object, value){
                  object@dropOrNot <- value; object}, where = where)
    return(invisible("done"))
}

colInfo <- function(name, type, drop){
    new("colInfo", name = name, type = type, dropOrNot = drop)
}
