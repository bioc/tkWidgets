tkMIAME <- function(){
  tt <- tktoplevel(width = 30)
  tkconfigure(tt, background = "white")
  tktitle(tt) <- "MIAME Information"

  tbar.fr <- tkframe(tt,relief="sunken",borderwidth=5,width="5i")
  win.fr <- tkframe(tt,relief="sunken",borderwidth=5,width="5i",height="7i")
  tkconfigure(win.fr, background = "white")
  tkpack(tbar.fr,side="top",fill="x")
  tkpack(win.fr,side="top", fill="both")

  space.label1 <- tklabel(win.fr, text = "")
  tkconfigure(space.label1, background = "white")
  space.label2 <- tklabel(win.fr, text = "")
  tkconfigure(space.label2, background = "white")
  space.label3 <- tklabel(win.fr, text = "")
  tkconfigure(space.label3, background = "white")
  space.label4 <- tklabel(win.fr, text = "")
  tkconfigure(space.label4, background = "white")
  space.label5 <- tklabel(win.fr, text = "")
  tkconfigure(space.label5, background = "white")
  space.label6 <- tklabel(win.fr, text = "")
  tkconfigure(space.label6, background = "white")
  space.label7 <- tklabel(win.fr, text = "")
  tkconfigure(space.label7, background = "white")
  space.label8 <- tklabel(win.fr, text = "")
  tkconfigure(space.label8, background = "white")
  space.label9 <- tklabel(win.fr, text = "")
  tkconfigure(space.label9, background = "white")
  space.label10 <- tklabel(win.fr, text = "")
  tkconfigure(space.label10, background = "white")

  ExpName.label <- tklabel(win.fr, text="Experimenter's Name: ")
  tkconfigure(ExpName.label, background = "white")
  ExpName.var <- tclVar()
  tclvalue(ExpName.var) <- ""
  ExpName.entry <- tkentry(win.fr, width = 25,textvariable = ExpName.var)
  tkconfigure(ExpName.entry, background = "white")

  LabName.label <- tklabel(win.fr, text="Laboratory: ")
  tkconfigure(LabName.label, background = "white")
  LabName.var <- tclVar()
  tclvalue(LabName.var) <- ""
  LabName.entry <- tkentry(win.fr, width = 25,textvariable = LabName.var)
  tkconfigure(LabName.entry, background = "white")

  Contact.label <- tklabel(win.fr, text="Contact Information: ")
  tkconfigure(Contact.label, background = "white")
  Contact.txt <- tktext(win.fr, width = 25, height =5)
  Contact.var <- tclVar()
  tkconfigure(Contact.txt, background = "white")

  ExpTitle.label <- tklabel(win.fr, text="Experiment Title: ")
  tkconfigure(ExpTitle.label, background = "white")
  ExpTitle.var <- tclVar()
  tclvalue(ExpTitle.var) <- ""
  ExpTitle.entry <- tkentry(win.fr, width = 25,textvariable = ExpTitle.var)
  tkconfigure(ExpTitle.entry, background = "white")

  Desc.label <- tklabel(win.fr, text="Experiment Description: ")
  tkconfigure(Desc.label, background = "white")
  Desc.txt <- tktext(win.fr, width = 25, height =10)
  Desc.var <- tclVar()
  tkconfigure(Desc.txt, background = "white")


  URL.label <- tklabel(win.fr, text="URL: ")
  tkconfigure(URL.label, background = "white")
  URL.var <- tclVar()
  tclvalue(URL.var) <- ""
  URL.entry <- tkentry(win.fr, width = 25,textvariable = URL.var)
  tkconfigure(URL.entry, background = "white")



  Exit.but <- tkbutton(win.fr, text = "Exit", command = function(){
    tclvalue(Contact.var) <- tclvalue(tkget(Contact.txt,"0.0","end"))
    tclvalue(Desc.var) <- tclvalue(tkget(Desc.txt,"0.0","end"))
    tkdestroy(tt)
  })

  tkconfigure(Exit.but, background = "white")


  tkgrid(space.label1, row =1)
  tkgrid(space.label2, row =2)
  tkgrid(ExpName.label, row = 3, col = 1, sticky ="e")
  tkgrid(ExpName.entry, row = 3, col =2, sticky = "w")
  tkgrid(space.label3, row =4)
  tkgrid(LabName.label, row = 5, col = 1, sticky = "e")
  tkgrid(LabName.entry, row = 5, col =2, sticky = "w")
  tkgrid(space.label4, row =6)
  tkgrid(Contact.label, row = 7, col = 1, sticky = "e")
  tkgrid(Contact.txt, row = 7, col =2, sticky = "w")
  tkgrid(space.label5, row =8)
  tkgrid(ExpTitle.label, row = 9, col = 1, sticky = "e")
  tkgrid(ExpTitle.entry, row = 9, col =2, sticky = "w")
  tkgrid(space.label6, row =10)
  tkgrid(Desc.label, row = 11, col = 1, sticky = "e")
  tkgrid(Desc.txt, row = 11, col =2, sticky = "w")
  tkgrid(space.label7, row =12)
  tkgrid(URL.label, row = 13, col = 1, sticky = "e")
  tkgrid(URL.entry, row = 13, col =2, sticky = "w")
  tkgrid(space.label8, row =14)
  tkgrid(space.label9, row =15)
  tkgrid(Exit.but, row = 16, col =2, sticky = "e")
  tkgrid(space.label10, row =17)

  tkwait.window(tt)

  miame.lst <- list(tclvalue(ExpName.var), tclvalue(LabName.var),tclvalue(Contact.var),tclvalue(ExpTitle.var),tclvalue(Desc.var),tclvalue(URL.var))
  names(miame.lst) <- c("ExperimentName","LabName","ContactInfo","ExperimentTitle","Description","URL")

  return(miame.lst)
}



# pW1 <- list(Name="Experiments's Name: ", Value="", toText=NULL, fromText=NULL, canEdit=TRUE, buttonFun = NULL, buttonText = NULL)

# pW2 <- list(Name="Laboratory: ", Value="", toText=NULL, fromText=NULL, canEdit=TRUE, buttonFun = NULL, buttonText = NULL)

# pW3 <- list(Name="Contact Information: ", Value="", toText=NULL, fromText=NULL, canEdit=TRUE, buttonFun = NULL, buttonText = NULL)

# pW4 <- list(Name="Experimental Title: ", Value="", toText=NULL, fromText=NULL, canEdit=TRUE, buttonFun = NULL, buttonText = NULL)

# pW5 <- list(Name="Experiment Description: ", Value="", toText=NULL, fromText=NULL, canEdit=TRUE, buttonFun = NULL, buttonText = NULL)

# pW6 <- list(Name="URL: ", Value="", toText=NULL, fromText=NULL, canEdit=TRUE, buttonFun = NULL, buttonText = NULL)

# widget1 <- list(wList = list(a = pW1, b = pW2, c = pW3, d = pW4, e = pW5, f = pW6), preFun = NULL, postFun = NULL)

# x <- widgetRender(widget1, "MIAME Information")

