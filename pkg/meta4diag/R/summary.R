summary.meta4diag = function(object,...){
  summarym4d = list()
  summarym4d$cpu.used = object$cpu.used
  summarym4d$summary.fixed = object[["summary.fixed"]]
  if(!object$misc$covariates.flag){
    summarym4d$summary.expected.logits = object[["summary.expected.logits"]]
  }else{
    summarym4d$summary.expected.study.specific.logits = object[["summary.expected.study.specific.logits"]]
  }
  summarym4d$summary.hyperpar = object[["summary.hyperpar"]]
  summarym4d$mlik = object$mlik[2,]
  summarym4d$var.type = c(rownames(object$summary.fixed),"var1", "var2", "rho")
  if(!object$misc$covariates.flag){
    summarym4d$correlation.expected.logits = object[["correlation.expected.logits"]]
  }
  summarym4d$modality.name = object$misc$modality.name
  summarym4d$modality.level = object$misc$modality.level
  class(summarym4d) = "summary.meta4diag"
  return(summarym4d)
}

print.summary.meta4diag = function(x,...){
  cat('Time used: \n')
  print(x$cpu.used)
  cat('\n')
  cat('\n')
  cat('Fixed effects: \n')
  fixed = round(x[["summary.fixed"]],4)
  print(fixed)
  if(!is.null(x[["summary.expected.logits"]])){
    cat('\n')
    cat('\n')
    cat('-------------------')
    cat('\n')
    summarised.fixed = round(x[["summary.expected.logits"]],4)
    print(summarised.fixed)
  }
  cat('\n')
  cat('\n')
  cat('Model hyperpar: \n')
  hyperpar = round(x[["summary.hyperpar"]],4)
  rownames(hyperpar)  = paste(rownames(x[["summary.hyperpar"]])," ",sep="")
  print(hyperpar)
  if(!is.null(x[["summary.expected.logits"]])){
    cat('\n')
    cat('\n')
    cat('-------------------')
    cat('\n')
    if(is.null(x$modality.name)){
      cat(paste('Correlation between ',paste(rownames(x[["summary.expected.logits"]]),collapse=" and ")," is ",round(x$correlation.expected.logits,4),".",sep=""))
    }else{
      paired.length = 0.5*dim(summarised.fixed)[1]
      for(i in 1:paired.length){
        cat(paste('Correlation between ',paste(rownames(x[["summary.expected.logits"]])[c(i, (i+paired.length))],collapse=" and ")," is ",round(x$correlation.expected.logits[i],4),". \n",sep=""))
      }
    }
  }
  
  cat('\n')
  cat('\n')
  mlik = round(x$mlik,4)
  names(mlik) = ""
  cat(paste("Marginal log-likelihood: ",mlik,sep=""))
  cat('\n')
  cat('\n')
  cat('Variable names for marginal plotting: \n')
  cat("      ")
  cat(paste(x$var.type,collapse=", "))
}

