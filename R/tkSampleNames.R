tkSampleNames <- function(...,filenames=character(0)){


  auxnames <- as.list(substitute(list(...)))[-1]
  filenames <- .Primitive("c")(filenames, auxnames)

  NumSamples <- length(filenames)

  sample.names <- array("",NumSamples)

  for(i in 1:NumSamples){
    s2 <-strsplit(strsplit(filenames[[i]],"/")[[1]][length(strsplit(filenames[[i]],"/")[[1]])],"\\.")[[1]]
    sample.names[i] <- s2[1]
  }

  tt <- tktoplevel(width = 30)
  tkconfigure(tt, background = "white")
  tktitle(tt) <- "Sample Information"

  tbar.fr <- tkframe(tt,relief="sunken",borderwidth=5,width="5i")
  win.fr <- tkframe(tt,relief="sunken",borderwidth=5,width="5i",height="7i")
  tkconfigure(win.fr, background = "white")
  tkpack(tbar.fr,side="top",fill="x")
  tkpack(win.fr,side="top", fill="both")


  array.name.lst <- list()
  array.entry.lst <- list()


  var.lst <- list()
  for(i in 1:NumSamples) {
    var.lst[[i]] <-tclVar()
    tclvalue(var.lst[[i]]) <- sample.names[i]
  }

  desc.lst <- list()
  for(i in 1:NumSamples) {
    desc.var <- tclVar("")
    desc.lst <- c(desc.lst,list(desc.var))
  }


  array.var <- list()

  space.label1 <- tklabel(win.fr, text = "")
  tkconfigure(space.label1, background = "white")
  space.label2 <- tklabel(win.fr, text = "")
  tkconfigure(space.label2, background = "white")
  space.label3 <- tklabel(win.fr, text = "")
  tkconfigure(space.label3, background = "white")
  tkgrid(space.label1,row = 1)

  array.name.lst <- tklabel(win.fr, text="File PathNames")
  tkconfigure(array.name.lst, background = "white")
  tkgrid(array.name.lst, row = 2, column = 1)

  array.entry.lst <- tklabel(win.fr, text="Sample Names")
  tkconfigure(array.entry.lst, background = "white")
  tkgrid(array.entry.lst, row = 2, column = 2)

  desc.entry.lst <- tklabel(win.fr, text="Description")
  tkconfigure(desc.entry.lst, background = "white")
  tkgrid(desc.entry.lst, row = 2, column = 3)



  for(i in 1:NumSamples){
    array.name.lst[[i]] <-tklabel(win.fr, text=filenames[[i]])
    tkconfigure(array.name.lst[[i]], background = "white")
    tkgrid(array.name.lst[[i]], row = (i+2), column = 1)
    tkgrid.configure(array.name.lst[[i]], sticky = "e")

    array.entry.lst [[i]] <- tkentry(win.fr, width = 30, textvariable
    = var.lst[[i]])
    tkconfigure(array.entry.lst[[i]], background = "white")
    tkgrid(array.entry.lst[[i]], row = (i+2), column = 2)
    tkgrid.configure(array.entry.lst[[i]], sticky = "e")

    desc.entry.lst [[i]] <- tkentry(win.fr, width = 30, textvariable = desc.lst[[i]])
    tkconfigure(desc.entry.lst[[i]], background = "white")
    tkgrid(desc.entry.lst[[i]], row = (i+2), column = 3)
    tkgrid.configure(desc.entry.lst[[i]], sticky = "w")




  }


  tkgrid(space.label3,row=(NumSamples+7))
  finish.but <- tkbutton(win.fr, text = "Continue", command = function(){tkdestroy(tt)})
  tkconfigure(finish.but, background = "white")
  tkgrid(finish.but, row = (NumSamples + 8), column = 2)
  tkgrid.configure(finish.but, sticky="e")

  tkwait.window(tt)

  sam.vec <- matrix("",nrow=NumSamples,ncol=2)
  colnames(sam.vec) <- c("Sample Names","Description")
  for(i in 1:NumSamples){
    sam.vec[i,1] <- tclvalue(var.lst[[i]])
    sam.vec[i,2] <- tclvalue(desc.lst[[i]])}
  return(sam.vec)
}



