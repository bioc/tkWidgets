# This function gets a vector containing the names of R objects and
# returns a list of lists with name and object pairs. If a name is for
# a package, the content of the package will be associated with the
# package name.
#
# J. Zhang, copyright 2002, all rights reserved.
#
objNameToList <- function (objNames, env){
    returnList <- list()
    for(i in 1:length(objNames)){
        # if it is a package, the list contains the contents of
        # the package
        if(regexpr("^package", objNames[i]) > 0){
            returnList[[objNames[i]]] <- package.contents(gsub("(^package:)",
                                                          "\\", objNames[i]))
        }else{
            returnList[[objNames[i]]] <- get(objNames[i], env = env)
        }
    }
    return(returnList)
}
