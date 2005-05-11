
.onLoad <- function(libname, pkgname) {
    require("methods", quietly=TRUE) || stop("Package methods unavailable")
    require("widgetTools", quietly = TRUE) ||
                     stop("Package widgetTools unavailable!")
    require("DynDoc", quietly=TRUE) || stop("Package DynDoc unavailable")
    if(.Platform$OS.type == "windows" && require(Biobase) && interactive()
        && .Platform$GUI ==  "Rgui"){
        addVigs2WinMenu("tkWidgets")
    }
}
