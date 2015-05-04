meta4diag = function(data=NULL, model.type = 1, var.prior = "invgamma",var2.prior, cor.prior = "normal",
                     var.par = c(0.25, 0.025),var2.par, cor.par = c(0,5),
                     wishart.par = c(4, 1, 1, 0),
                     init = c(0.01,0.01,0), link="logit", level=c(0.025,0.5,0.975),
                     verbose = FALSE, covariates = NULL,nsample=FALSE){
  
  if(!is.element("INLA", installed.packages()[,1])){
    install.packages("INLA",dependencies=TRUE, repos="http://www.math.ntnu.no/inla/R/stable")
    require("INLA", quietly = TRUE)
  }
  if (!(sum(search()=="package:INLA")==1)){
    require("INLA", quietly = TRUE)
  }
  
  ################ check data
  if(!is.data.frame(data)){
    stop("Data MUST be a data frame!!!")
  }
  ################ check model.type
  if(length(model.type)!=1){
    stop("Argument \"model.type\" can ONLY be ONE integer of c(1,2,3,4)!!!")
  }
  if(!is.numeric(model.type)){
    stop("Argument \"model.type\" can ONLY be ONE integer of c(1,2,3,4)!!!")
  }else{ # model.type is numerical
    if(!(model.type %in% c(1,2,3,4))){
      stop("Argument \"model.type\" can ONLY be ONE integer of c(1,2,3,4)!!!")
    }
  }
  
  I = dim(data)[1]
  ################
  variables.names = colnames(data)
  if(!("studynames" %in% variables.names)){
    data$studynames = paste("study[",c(1:I),"]",sep="")
  }
  
  ################ check link
  if(!is.character(link)){
    stop("Argument \"link\" can ONLY be character. The options are \"logit\", \"probit\" and \"cloglog\"!!!")
  }
  if(length(link)!=1){
    stop("Argument \"link\" can ONLY be character. The options are \"logit\", \"probit\" and \"cloglog\"!!!")
  }
  ################ check verbose
  if(!is.logical(verbose)){
    stop("Argument \"verbose\" can ONLY be logic, either \"TRUE\" or \"FALSE\"!!!")
  }
  
  ################ Make prior, and in the makePrior function, check var.prior, var.par, cor.prior, cor.par and init
  outpriors = makePriors(var.prior=var.prior, cor.prior=cor.prior, var.par=var.par, cor.par=cor.par, init=init)
  
  ################ Make data, and in the makedata function, check covariates and compare
  outdata = makeData(data = data, model.type = model.type, covariates = covariates)
  
  ################
  model = runModel(outdata=outdata, outpriors=outpriors, model.type=model.type, link=link, level=level, verbose = verbose)
  
  ##########################  construct the result
  res = makeObject(outdata, outpriors, model, nsample=nsample)
  
  return(res)
}

print.meta4diag = function(x,...){
  cat('Time used: \n')
  print(x$cpu.used)
  cat("\n")
  cat("\n")
  cat(paste('Model:Binomial-Normal Bivariate Model for ', paste(x$names.fitted,collapse=" & "),". \n",sep=""))
  cat(paste('Data contains ', dim(x$data)[1], " primary studies. \n",sep=""))
  cat("\n")
  cat("\n")
  if(x$misc$modality.flag){
    cat(paste("Data has Modality variable with level ", x$misc$modality.level, ". \n",sep=""))
    if(x$misc$covariates.flag){
      cat("Covariates contained. \n")
    } else{
      cat("Covariates not contained. \n")
    }  
  }else{ # no modality
    cat("Data has no Modality variable. \n")
    if(x$misc$covariates.flag){
      cat("Covariates contained. \n")
    } else{
      cat("Covariates not contained. \n")
    }
  }
  cat("\n")
  cat("\n")
  cat(paste("Model using link function ",x$misc$link,".\n",sep=""))
  cat("\n")
  cat("\n")
  cat("Marginals can be plotted with setting variable names to ") 
  cat("\n")
  cat(paste(paste(rownames(x$summary.fixed),collapse=", "), "var1, var2 and rho",sep=""))
}

summary.meta4diag = function(object,...){
  cat('Time used: \n')
  print(object$cpu.used)
  cat('\n')
  cat('\n')
  cat('Fixed effects: \n')
  fixed = rbind(round(object[["summary.fixed"]],4), rep("-------",6),
              round(object[["summary.summarized.fixed"]],4))
  rownames(fixed)  = c(rownames(object[["summary.fixed"]]), " ", rownames(object[["summary.summarized.fixed"]]))
  print(fixed)
  cat('\n')
  cat('\n')
  cat('Model hyperpar: \n')
  hyperpar = round(object[["summary.hyperpar"]],4)
  rownames(hyperpar)  = paste(rownames(object[["summary.hyperpar"]])," ",sep="")
  print(hyperpar)
  cat('\n')
  cat(paste('Correlation between ',paste(rownames(object[["summary.summarized.fixed"]]),collapse=" and ")," is ",round(object$summarized.fixed.correlation.matrix[1,2],4),".",sep=""))
  cat('\n')
  cat('\n')
  mlik = t(as.matrix(object$mlik[2,]))
  rownames(mlik) = "Marginal log-likelihood: "
  colnames(mlik) = ""
  print(mlik)
  summarym4d = list()
  summarym4d$cpu.time = object$cpu.used
  summarym4d$fixed = object[["summary.fixed"]]
  summarym4d$summarized.fixed = object[["summary.summarized.fixed"]]
  summarym4d$random = object[["summary.hyperpar"]]
  summarym4d$mlik = object$mlik[2,]
  summarym4d$var.type = c(rownames(object$summary.fixed),"var1","var2","rho")
  return(summarym4d)
}

plot.meta4diag = function(x, var.type="var1", add=FALSE, overlay.prior = TRUE, save = FALSE, width=5, height=5, ...){
  if(is.logical(save)){
    if(save){
      mainDir <- getwd()
      subDir <- "meta4diag-Plot"
      if (file.exists(subDir)){
        file.name = paste(subDir,"/",var.type,".pdf",sep="")
      } else {
        dir.create(file.path(mainDir, subDir))
        file.name = paste(subDir,"/",var.type,".pdf",sep="")
      }
      save.flag = TRUE
    }else{save.flag = FALSE}
  } else if(is.character(save)){
    name_temp = unlist(strsplit(basename(save), "[.]"))
    if(length(name_temp)!=2){
      save.flag = FALSE
      stop("Please give the correct file name!")
    }else{
      fileform = name_temp[2]
      if(fileform %in% c("pdf","eps","jpg","png")){
        save.flag = TRUE
        file.name = paste(subDir,"/",save,sep="")
      }else{
        save.flag = FALSE
        stop("Please give the correct file name!")
      }
    }
  } else{stop("Argument \"save\" could be FALSE, TRUE or a file name.")}
  
  fixed.name = rownames(x$summary.fixed)
  hyper.name = c("var1", "var2", "rho")
  fullnames = c(fixed.name,hyper.name)
  if(!(var.type %in% fullnames)){
    stop(paste("Please give the correct \"type\" name, which should be ", paste(fullnames,collapse=", "),sep=""))
  }
  fixed.prior = data.frame(x=seq(-10,10,len=100),y=dnorm(seq(-10,10,len=100),mean=0,sd=1))
  fullmarginals = append(x$marginals.fixed,x$marginals.hyperpar)
  ind = which(fullnames==var.type)
  marginals.plot = INLA::inla.smarginal(fullmarginals[[ind]])
  
  xlabnames = c(fixed.name, rownames(x$summary.hyperpar))
  
  if(add){
    lines(marginals.plot, ...)
  }else{
    if(save.flag){
      if(fileform=="eps"){
        setEPS()
        postscript(file.name, width=width, height=height,...)
      }else if(fileform=="pdf"){
        pdf(file.name, width=width, height=height,...)
      }else if(fileform=="jpg"){
        jpeg(filename = file.name,
             width = width, height = height, units = "in")
      }else{
        png(filename = file.name,
             width = width, height = height, units = "in")
      }
    }
    par(mar=c(5.1, 4.1, 4.1, 2.1))
    plot(marginals.plot, type="l", xlab=xlabnames[ind], ylab="",xaxs = "r",family="sans",xaxt="s",yaxt="s",bty="o", ...)
    if(overlay.prior){
      nom = length(fullnames)
      if(ind<=(nom-3)){
        lines(fixed.prior,lty=2,col="darkgray")
      }else if(ind==(nom-2) || ind==(nom-1)){
        lines(x$priors.density[[1]],lty=2,col="darkgray")
      }else if(ind==nom){
        lines(x$priors.density[[2]],lty=2,col="darkgray")
      }else{
        stop("Wrong var.type!")
      }
    }
    if(save.flag){
      dev.off()
    }
  }
}



# .ROC = function(x, add=FALSE){
#   modelnames = x$names.model
#   lm = length(modelnames)
#   is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
#   if(length(model)!=1){stop("Argument \"model\" should be a inteeger.")}
#   if(!is.numeric(model)){
#     stop(paste("Argument \"model\" should be in c(1:",lm,").",sep=""))
#   }
#   if(!is.wholenumber(model)){
#     stop(paste("Argument \"model\" should be in c(1:",lm,").",sep=""))
#   }
#   fitname = x$names.fitted
#   fullname = paste("summary.predict.",fitname,sep="")
#   fitfullname = paste("summary.fitted.",fitname,sep="")
#   t = seq(0, 2*pi, by = 2*pi/100)
#   par(mfrow=c(2,2),mar=c(5.1,5.1,2.1,1.1))
#   for(i in 1:lm){
#     mean.A = x[[fullname[1]]][[i]][,1]
#     sd.A = x[[fullname[1]]][[i]][,2]
#     mean.B = x[[fullname[2]]][[i]][,1]
#     sd.B = x[[fullname[2]]][[i]][,2]
#     r = x$mean.correlation[i]
#     
#     I = length(mean.A)
#     f = qf(0.95, 2, I-2)
#     c = sqrt(2*f)
#     
#     A = mean.A[1] + sd.A[1]*c*cos(t)
#     B = mean.B[1] + sd.B[1]*c*cos(t + acos(r))
#     confidence.A = .invlogit(A)
#     confidence.B = .invlogit(B)
#     plot(confidence.B, confidence.A, type="l",xlim=c(0,1),ylim=c(0,1),xlab=fitname[2],ylab=fitname[1])
#     points(x[[fitfullname[2]]][[i]][,1],x[[fitfullname[1]]][[i]][,1],pch=1)
#     for(j in 2:I){
#       A = mean.A[j] + sd.A[j]*c*cos(t)
#       B = mean.B[j] + sd.B[j]*c*cos(t + acos(r))
#       confidence.A = .invlogit(A)
#       confidence.B = .invlogit(B)
#       lines(confidence.B, confidence.A)
#     }
#   }
# }