# A function to create a light wight tk widget with a text.
# x and y - the location for the left upper coner of the widget to
#           appear on the screen
getLightTW <- function(x, y, text){
    on.exit(end())

    end <- function(){
        tkdestroy(base)
    }
    # Takes out the frame and title bar
    tkwm.overrideredirect(base <- tktoplevel(), TRUE)
    # Put the TW in the right place
    tkwm.geometry(base, paste("+", x, "+", y, sep = ""))
    text <- tklabel(base, text = text)
    tkpack(text)
    tkbind(base, "<ButtonPress>", end)

    tkwait.window(base)

    return(invisible())
}
