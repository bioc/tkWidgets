# This function reads the formal arguments of an R function and then
# generates an XML document containing those arguments
#
# Copyright 2002, J. Zhang, all rights reserved
#

args2XML<- function(fun, xml.name = "", full.names = NULL,
                      priority = NULL){

    if(xml.name == ""){
        xml.name = paste(deparse(substitute(fun)), "arg.xml", sep = "")
    }

    writeXML <- function(text){
        write(text, file = xml.name, append = TRUE)
    }

    args <- formals(fun)
    write(paste("<!DOCTYPE tkWidgets: SYSTEM \"http://",
                "www.bioconductor.org/datafiles/dtds/args.dtd\">",
                sep = ""), file = xml.name)
    writeXML(paste("<tkWidgets:Arguments xmlns:AnnBuilder='http://",
                "www.bioconductor.org/tkWidgets/'>", sep = ""))

    for(i in 1:length(args)){
        writeXML("<tkWidgets:Argument>")
        writeXML(paste("<tkWidgets:ArgName value = \"",
                       names(args[i]), "\"/>", sep = ""))
        writeXML(paste("<tkWidgets:ArgVal value = \"", args[i],
                       "\"/>", sep = ""))
        writeXML(paste("<tkWidgets:FullName value = \"",
                       ifelse(is.null(full.names), "", full.names[i]),
                       "\" />", sep = ""))
        writeXML(paste("<tkWidgets:Priority value = \"",
                       ifelse(is.null(priority), "", priority[i]),
                       "\" />", sep = ""))
        write("</tkWidgets:Argument>")
    }

    writeXML("</tkWidgets:Arguments>")

    return(xml.name)
}
