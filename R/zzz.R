
.onLoad <- function(libname, pkgname, where) {
    require("methods", quietly=TRUE) || stop("Package methods unavailable")
    require("widgetTools", quietly = TRUE) ||
                     stop("Package widgetTools unavailable!")
    if(.Platform$OS.type == "windows" && require(Biobase) && interactive()
        && .Platform$GUI ==  "Rgui"){
        addVigs2WinMenu("tkWidgets")
    }
}
