plot.meta4diag = function(x, var.type="var1", add=FALSE, overlay.prior = TRUE, save = FALSE, width=5, height=5, ...){

  if(is.logical(save)){
    if(save){
      mainDir <- getwd()
      subDir <- "meta4diagPlot"
      if (file.exists(subDir)){
        file.name = paste(subDir,"/",var.type,".pdf",sep="")
      } else {
        dir.create(file.path(mainDir, subDir))
        file.name = paste(subDir,"/",var.type,".pdf",sep="")
      }
    }
  }else{
    if(is.character(save)){
      mainDir <- getwd()
      subDir <- "meta4diagPlot"
      if (!file.exists(subDir)){
        dir.create(file.path(mainDir, subDir))
      }
      name_temp = unlist(strsplit(basename(save), "[.]"))
      fileform = name_temp[length(name_temp)]
      if(fileform %in% c("pdf","eps","jpg","png")){
        file.name = paste(subDir,"/",save,sep="")
      }else{
        stop("Please give the correct file name!")
      }
    }else{
      stop("Argument \"save\" could be FALSE, TRUE or a file name.")
    }
  }
  
  fixed.name = rownames(x$summary.fixed)
  hyper.name = rownames(x$summary.hyperpar)
  fullnames = c(fixed.name, hyper.name, "var1", "var2", "rho")
  if(!(var.type %in% fullnames)){
    stop(paste("Please give the correct \"type\" name, which should be ", paste(fullnames,collapse=", ")," var1, var2, rho.",sep=""))
  }
  
  if(var.type=="var1"){var.type="var_phi"}
  if(var.type=="var2"){var.type="var_psi"}
  if(var.type=="rho"){var.type="cor"}
  
  
  fixed.prior = data.frame(x=seq(-10,10,len=100),y=dnorm(seq(-10,10,len=100),mean=0,sd=sqrt(1000)))
  fullmarginals = append(x$marginals.fixed,x$marginals.hyperpar)
  ind = which(fullnames==var.type)
  if(var.type=="var_phi" || var.type=="var_psi"){
    marginals.plot = fullmarginals[[ind]]
  }else{
    marginals.plot = INLA::inla.smarginal(fullmarginals[[ind]])
  }
  
  
  xlabnames = c(fixed.name, rownames(x$summary.hyperpar))
  
  if(add){
    lines(marginals.plot, ...)
  }else{
    if(is.logical(save)){
      if(save){
        pdf(file.name, width=width, height=height)
      }
    }else{
      if(fileform=="eps"){
        setEPS()
        postscript(file.name, width=width, height=height)
      }else if(fileform=="pdf"){
        pdf(file.name, width=width, height=height)
      }else if(fileform=="jpg"){
        jpeg(filename = file.name,
             width = width, height = height, units = "in")
      }else{
        png(filename = file.name,
            width = width, height = height, units = "in")
      }
    }
    
    plot(marginals.plot, type="l", xlab=xlabnames[ind], ylab="",xaxs = "r",xaxt="s",yaxt="s",bty="o",...)
    if(!x$misc$wishart.flag){
      if(overlay.prior){
        if(var.type %in% c("var_phi","var_psi","cor")){
          if(var.type=="var_phi"){
            lines(x$priors.density[[1]],lty=2,col="darkgray")
          }else if(var.type == "var_psi"){
            lines(x$priors.density[[2]],lty=2,col="darkgray")
          }else{
            lines(x$priors.density[[3]],lty=2,col="darkgray")
          }
        }else{
          lines(fixed.prior,lty=2,col="darkgray")
        }
      }
    }
    if(save==TRUE || is.character(save)==TRUE){
      dev.off()
    }
  }
}