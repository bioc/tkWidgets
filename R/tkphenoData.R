tkphenoData <- function(sampleNames){
  eerieEnv <- new.env()

  sample.length <- length(sampleNames)

#############################################
#hierarchy of widgets
#############################################

  NumWidgets <- 5 #number of widgets
  hierarchy <- list()
  for(i in 1:NumWidgets){
    hierarchy[[i]] <-function(){}
  }


#############################################
###third widget -- Main widget
#############################################
  hierarchy[[3]] <- function(){



    tt3 <- tktoplevel(width = 30)
    tkconfigure(tt3, background = "white")
    tktitle(tt3) <- "Pheno Data"

    tbar.fr <- tkframe(tt3,relief="sunken",borderwidth=5,width="5i")
    win.fr <- tkframe(tt3,relief="sunken",borderwidth=5,width="5i",height="7i")
    tkconfigure(win.fr, background = "white")
    tkpack(tbar.fr,side="top",fill="x")
    tkpack(win.fr,side="top", fill="both")

    assign("NumSamples", sample.length,eerieEnv)
    assign("NumCovar",get("NumCovariates",eerieEnv),eerieEnv)
    assign("NewNumCovariates",as.numeric(tclvalue(get("NewNumCovariates.var",eerieEnv))),eerieEnv)
    assign("OldNumCovariates1",as.numeric(tclvalue(get("OldNumCovariates1.var",eerieEnv))),eerieEnv)

    array.name.lst <-list()
    array.entry.lst <- list()

    cb.lst <- list()
    if(!is.na(match("var.lst",ls(eerieEnv)))){
      assign("var.lst", get("var.lst",eerieEnv),eerieEnv)}
    else{
      assign("var.lst", list(),eerieEnv)
      for(i in 1:get("NumSamples",eerieEnv)) {
        assign("var",tclVar(sampleNames[i]),eerieEnv)
        assign("var.lst",c(get("var.lst",eerieEnv),list(get("var",eerieEnv))),eerieEnv)
        ## assign(tclvalue(get("var.lst",eerieEnv)[[i]]),sampleNames[i],eerieEnv)
      }}
    if(get("OldNumCovariates1",eerieEnv) != get("NumCovariates",eerieEnv)){
      assign("cov.lst",list(),eerieEnv)

      for(i in 1:get("NumSamples",eerieEnv)){
        assign("dummy.lst",list(),eerieEnv)
        for(j in 1:get("NumCovar",eerieEnv)){
          assign("dummy.var",tclVar(),eerieEnv)
          assign("dummy.lst",c(get("dummy.lst",eerieEnv),list(get("dummy.var",eerieEnv))),eerieEnv)}
        assign("cov.lst",c(get("cov.lst",eerieEnv),list(get("dummy.lst",eerieEnv))),eerieEnv)
      }


      ##for(i in 1:get("NumSamples",eerieEnv)){
      ##  assign(get("cov.lst",eerieEnv)[[i]],list(),eerieEnv)
      ##  for(j in 1:get("NumCovar",eerieEnv)){
      ##       assign("array.cov",tclVar(),eerieEnv)
      ##       assign(get("cov.lst",eerieEnv)[[i]],c(get("cov.lst",eerieEnv)[[i]],list(get("array.cov",eerieEnv))),eerieEnv)
      ##     }
    }
    else{
      if(!is.na(match("cov.lst",ls(eerieEnv)))){
        assign("cov.lst","cov.lst",eerieEnv)}
      else{
        assign("cov.lst",list(),eerieEnv)

        for(i in 1:get("NumSamples",eerieEnv)){
          assign("dummy.lst",list(),eerieEnv)
          for(j in 1:get("NumCovar",eerieEnv)){
            assign("dummy.var",tclVar(),eerieEnv)
            assign("dummy.lst",c(get("dummy.lst",eerieEnv),list(get("dummy.var",eerieEnv))),eerieEnv)}
          assign("cov.lst",c(get("cov.lst",eerieEnv),list(get("dummy.lst",eerieEnv))),eerieEnv)
        }


        ##for(i in 1:get("NumSamples",eerieEnv)){
        ##  assign(get("cov.lst",eerieEnv)[[i]],list(),eerieEnv)
        ##  for(j in 1:get("NumCovar",eerieEnv)){
        ##       assign("array.cov",tclVar(),eerieEnv)
        ##       assign(get("cov.lst",eerieEnv)[[i]],c(get("cov.lst",eerieEnv)[[i]],list(get("array.cov",eerieEnv))),eerieEnv)
        ##q()

      }}
    ##array.var <<- list()

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
    tkgrid(space.label1,row = 1)

    array.entry.lst <- tklabel(win.fr, text="Sample Names")
    tkconfigure(array.entry.lst, background = "white")
    tkgrid(array.entry.lst, row = 2, col = 2)


    for(j in 1:get("NumCovar",eerieEnv)) {
      cb.lst <- tklabel(win.fr, text = tclvalue(get("cov.name.lst",eerieEnv)[[j]]))
      tkconfigure(cb.lst, background = "white")
      tkgrid(cb.lst, row = 2, col = (j+3))
    }

    for(i in 1:get("NumSamples",eerieEnv)){
      array.name.lst <-tklabel(win.fr, text=paste("Array",i))
      tkconfigure(array.name.lst, background = "white")
      tkgrid(array.name.lst, row = (i+2), col = 1)


      array.entry.lst [[i]]<- tkentry(win.fr, width = 30, textvariable = get("var.lst",eerieEnv)[[i]])
      tkconfigure(array.entry.lst[[i]], background = "white")
      tkgrid(array.entry.lst[[i]], row = (i+2), col = 2)
      tkgrid.configure(array.entry.lst[[i]], sticky = "e")

      for(j in 1:get("NumCovar",eerieEnv)){
        cb.lst[[j]] <- tkentry(win.fr, width=6,textvariable = get("cov.lst",eerieEnv)[[i]][[j]])
        tkconfigure(cb.lst[[j]], background = "white")
        tkgrid(cb.lst[[j]], row = (i+2), col=(j+3))
      }
    }

    tkgrid(space.label3,row=(get("NumSamples",eerieEnv)+3))
    tkgrid(space.label4,row=(get("NumSamples",eerieEnv)+4))
    tkgrid(space.label5,row=(get("NumSamples",eerieEnv)+5))
    back.but <- tkbutton(win.fr, width = 8, text ="Back", command = function(){
      tkdestroy(tt3)
      hierarchy[[2]]()})
    tkconfigure(back.but, background = "white")
    finish.but <- tkbutton(win.fr, text = "Finish", command = function(){

      for(i in 1:get("NumSamples",eerieEnv)){
        for(j in 1:get("NumCovar",eerieEnv)){
          if(tclvalue(get("cov.lst",eerieEnv)[[i]][[j]])==""){
            assign("error", "Missing Entry",eerieEnv)
          }
        }
      }

      if(get("error",eerieEnv)=="Missing Entry"){
        assign("error","just for a change!!",eerieEnv)
        error.fr <-tkframe(tt3, borderwidth = 5, width = "5i", height = "2i")
        tkconfigure(error.fr, background = "yellow")
        tkpack(error.fr,side="top", fill="both")
        error.lbl <- tklabel(error.fr, text = "Error! Missing Entry in Phenodata")
        tkconfigure(error.lbl, background = "yellow")
        tkpack(error.lbl)
        error.but <- tkbutton(error.fr,text="OK",command = function(){tkdestroy(error.fr)})
        tkconfigure(error.but, background = "yellow")
        tkpack(error.but)
      }

      else{
        tkdestroy(tt3)
        tkdestroy(win.fr)
      }

    })

    tkconfigure(finish.but, background = "white")
    tkgrid(back.but, row = (get("NumSamples",eerieEnv) + 6),col = 2)
    tkgrid(finish.but, row = (get("NumSamples",eerieEnv) + 6), col = 3)
    tkgrid.configure(back.but, sticky = "e")
    tkgrid.configure(finish.but, sticky = "w")


    tkwait.window(tt3)

    pd.matrix <- matrix(NA,nrow=length(sampleNames),ncol=get("NumCovar",eerieEnv))
    rownames(pd.matrix) <- sampleNames

    c.names <- array("",get("NumCovar",eerieEnv))
    for(j in 1:get("NumCovar",eerieEnv)){
      c.names[j] <- tclvalue(get("cov.name.lst",eerieEnv)[[j]])}
    colnames(pd.matrix) <- c.names
    for(i in 1:length(sampleNames)){
      for(j in 1:get("NumCovar",eerieEnv)){
        pd.matrix[i,j] <- tclvalue(get("cov.lst",eerieEnv)[[i]][[j]])}}

    desc.matrix <- matrix("",nrow=get("NumCovar",eerieEnv),ncol=1)
    rownames(desc.matrix) <- c.names
    colnames(desc.matrix) <- "Description"

    for(i in 1:get("NumCovar",eerieEnv)){
      desc.matrix[i,1] <- tclvalue(get("desc.lst",eerieEnv)[[i]])}
    pd.info <- list(pd.matrix,desc.matrix)
    names(pd.info) <- c("pData", "varLabels")

    assign("pd.info",pd.info,eerieEnv)

}





##############################################
### second widget
##############################################

  hierarchy[[2]] <- function(){

    assign("backCount1",0,eerieEnv)

    if(!is.na(match("NewNumCovariates.var",ls(eerieEnv)))){
      assign("OldNumCovariates.var",get("NewNumCovariates.var",eerieEnv),eerieEnv)
    }



    tt2 <- tktoplevel(width = 70)
    tktitle(tt2) <- "Covariate Names"
    tkconfigure(tt2, background = "white")

    tbar.fr <- tkframe(tt2,relief="sunken",borderwidth=5,width="5i")
    win.fr <- tkframe(tt2,relief="sunken",borderwidth=5,width="5i",height="7i")
    tkconfigure(win.fr, background = "white")
    tkpack(tbar.fr,side="top",fill="x")
    tkpack(win.fr,side="top", fill="both")

    assign("NumCovariates", as.numeric(tclvalue(get("NumCovariates.var",eerieEnv))),eerieEnv)
    assign("OldNumCovariates",as.numeric(tclvalue(get("OldNumCovariates.var",eerieEnv))),eerieEnv)

    if(!is.na(match("OldNumCovariates1.var",ls(eerieEnv)))){
      assign("OldNumCovariates1.var", get("OldNumCovariates1.var",eerieEnv),eerieEnv)}
    else{
      assign("OldNumCovariates1.var", tclVar(0),eerieEnv)
    }
    assign("cov.name.lst",list(),eerieEnv)
    if(get("OldNumCovariates",eerieEnv) != get("NumCovariates",eerieEnv)){
      for(i in 1:get("NumCovariates",eerieEnv)){
        assign("cov.name", tclVar(""),eerieEnv)
        assign("cov.name.lst",c(get("cov.name.lst",eerieEnv),list(get("cov.name",eerieEnv))),eerieEnv)
      }}
    else{
      if(!is.na(match("cov.name.lst",ls(eerieEnv)))){
        assign("cov.name.lst", cov.name.label,eerieEnv)}
      else{
        for(i in 1:get("NumCovariates",eerieEnv)){
          assign("cov.name", tclVar(""),eerieEnv)
          assign("cov.name.lst",c(get("cov.name.lst",eerieEnv),list(get("cov.name",eerieEnv))),eerieEnv)
        }  }}

    cov.entry.lst <- list()

    assign("desc.lst",list(),eerieEnv)
    if(get("OldNumCovariates",eerieEnv) != get("NumCovariates",eerieEnv)){
      for(i in 1:get("NumCovariates",eerieEnv)){
        assign("descvar",tclVar(""),eerieEnv)
        assign("desc.lst",c(get("desc.lst",eerieEnv),list(get("descvar",eerieEnv))),eerieEnv)}
    }
    else{
      if(!is.na(match("desc.lst",ls(eerieEnv)))){
        assign("desc.lst",desc.lst,eerieEnv)}
      else{
        for(i in 1:get("NumCovariates",eerieEnv)){
          assign("descvar",tclVar(""),eerieEnv)
          assign("desc.lst",c(get("desc.lst",eerieEnv),list(get("descvar",eerieEnv))),eerieEnv)}
      }}

    desc.entry <- list()

    space.label1 <- tklabel(win.fr, text = "")
    tkconfigure(space.label1, background = "white")
    space.label2 <- tklabel(win.fr, text = "")
    tkconfigure(space.label2, background = "white")
    space.label3 <- tklabel(win.fr, text = "")
    tkconfigure(space.label3, background = "white")


    tkgrid(space.label1, row = 1)

    for(i in 1:get("NumCovariates",eerieEnv)){

      cov.label <-tklabel(win.fr, text=paste("Cov",i))
      tkconfigure(cov.label, background = "white")
      tkgrid(cov.label, row = (i+2), col = 1)

      cov.entry.lst[[i]] <- tkentry(win.fr, width = 30, textvariable = get("cov.name.lst",eerieEnv)[[i]])
      tkconfigure(cov.entry.lst[[i]], background = "white")
      tkgrid(cov.entry.lst[[i]], row = (i+2), col = 2)

      covtop.label <-tklabel(win.fr, text="Covariate Names")
      tkconfigure(covtop.label, background = "white")
      tkgrid(covtop.label, row=2, col=2)

      desc.label <- tklabel(win.fr, text = "Description")
      tkconfigure(desc.label, background = "white")
      tkgrid(desc.label, row=2, col=3)

      desc.entry[[i]] <- tkentry(win.fr, width = 30, textvariable = get("desc.lst",eerieEnv)[[i]])
      tkconfigure(desc.entry[[i]], background = "white")
      tkgrid(desc.entry[[i]], row = (i+2), col = 3)
    }


    but1 <- tkbutton(win.fr, width = 8,text = "Back", command = function(){
      assign("backCount1",1,eerieEnv)
      tkdestroy(tt2)
      hierarchy[[1]]()})
    tkconfigure(but1, background = "white")
    but2 <- tkbutton(win.fr,text = "Continue",command= function(){

      for(i in 1:get("NumCovariates",eerieEnv)){
        if(tclvalue(get("cov.name.lst",eerieEnv)[[i]]) == ""){
          assign("error","Missing Entry",eerieEnv)}
      }

      if(get("error",eerieEnv)=="Missing Entry"){
        assign("error", "just for a change1!!",eerieEnv)
        error.fr <-tkframe(tt2, borderwidth = 5, width = "5i", height = "2i")
        tkconfigure(error.fr, background = "yellow")
        tkpack(error.fr,side="top", fill="both")
        error.lbl <- tklabel(error.fr, text = "Error! Missing Entry in Covariate Names")
        tkconfigure(error.lbl, background = "yellow")
        tkpack(error.lbl)
        error.but <- tkbutton(error.fr,text="OK",command = function(){tkdestroy(error.fr)})
        tkconfigure(error.but, background = "yellow")
        tkpack(error.but)
      }else{
        assign("NewNumCovariates.var",get("OldNumCovariates.var",eerieEnv),eerieEnv)
        assign("OldNumCovariates.var", get("NumCovariates.var",eerieEnv),eerieEnv)
        tkdestroy(tt2)
        hierarchy[[3]]()
      }})

    tkconfigure(but2, background = "white")

    tkgrid(space.label2,row = (get("NumCovariates",eerieEnv) + 5))
    tkgrid(space.label3,row = (get("NumCovariates",eerieEnv) + 6))
    tkgrid(but1,row = (get("NumCovariates",eerieEnv) + 7),col = 2)
    tkgrid(but2,row = (get("NumCovariates",eerieEnv) + 7), col = 3)
    tkgrid.configure(but1,sticky="e")
    tkgrid.configure(but2,sticky="w")


    tkwait.window(tt2)

    assign("OldNumCovariates1", as.numeric(tclvalue(get("OldNumCovariates1.var",eerieEnv))),eerieEnv)
  }

###############################################
###first widget
###############################################


  hierarchy[[1]] <- function(){


    tt1 <- tktoplevel(width = "5i")
    tkconfigure(tt1, background = "white")
    tktitle(tt1) <- "Number of Covariates"

    tbar.fr <- tkframe(tt1,relief="sunken",borderwidth=5,width="5i")
    win.fr <- tkframe(tt1,relief="sunken",borderwidth=5,width="5i",height="7i")
    tkconfigure(win.fr, background = "white")
    tkpack(tbar.fr,side="top",fill="x")
    tkpack(win.fr,side="top", fill="both")

    NumCovariates.label <- tklabel(win.fr, text="Enter the Number Of Covariates")
    tkconfigure(NumCovariates.label, background = "white")

    ##taking care of the back button( ie. once some value is assigned it remains unless changed by th user)
    if(!is.na(match("OldNumCovariates.var",ls(eerieEnv)))){
      assign("OldNumCovariates.var",get("OldNumCovariates.var",eerieEnv),eerieEnv)}
    else{
      assign("OldNumCovariates.var", tclVar(0),eerieEnv)
    }
    if(!is.na(match("NumCovariates.var",ls(eerieEnv)))){
      assign(" NumCovariates.var",get("NumCovariates.var",eerieEnv),eerieEnv)}
    else{
      assign("NumCovariates.var", tclVar(),eerieEnv)
    }

    NumCovariates.entry <- tkentry(win.fr, width = 6,textvariable = get("NumCovariates.var",eerieEnv))
    tkconfigure(NumCovariates.entry, background = "white")

    but1 <- tkbutton(win.fr,text = "Continue",command=function(){
      assign("error", "none", eerieEnv)
      if(tclvalue(get("NumCovariates.var",eerieEnv)) < 1){
        assign("error","Error!! Number of covariates should be atleast one",eerieEnv)
        error.fr <-tkframe(tt1, borderwidth = 5, width = "5i", height = "2i")
        tkconfigure(error.fr, background = "yellow")
        tkpack(error.fr,side="top", fill="both")
        error.lbl <- tklabel(error.fr, text = get("error",eerieEnv))
        tkconfigure(error.lbl, background = "yellow")
  tkpack(error.lbl)
        error.but <- tkbutton(error.fr,text="OK",command = function(){tkdestroy(error.fr)})
        tkconfigure(error.but, background = "yellow")
        tkpack(error.but)
      }
      else {
        tkdestroy(win.fr)
        tkdestroy(tt1)
      }})

    tkconfigure(but1, background = "white")

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


    tkgrid(space.label1, row =1)
    tkgrid(space.label2, row =2)
    tkgrid(NumCovariates.label, row = 3, col = 1)
    tkgrid(NumCovariates.entry, row = 3, col =2)
    tkgrid(space.label3, row =4)
    tkgrid(space.label4, row =5)
    tkgrid(but1, row = 6, col = 1)
    tkgrid(space.label5, row =7)

    tkwait.window(tt1)
    hierarchy[[2]]()



  }


  hierarchy[[1]]()



  return(get("pd.info",eerieEnv))
}







