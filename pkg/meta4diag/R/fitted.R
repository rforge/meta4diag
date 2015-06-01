fitted.meta4diag = function(object, accuracy.type="sens",...){
  accuracy.type = tolower(accuracy.type)
  suitable.set = c("sens", "TPR", "spec", "TNR", "FPR", "FNR", "LRpos", "LRneg", "RD", "LLRpos", "LLRneg", "LDOR", "DOR")
  if(!(accuracy.type %in% tolower(suitable.set))){
    stop(paste("Please give the correct accuracy.type, which could be ",paste(suitable.set, collapse=", "),".",sep=""))
  }
  if(!object$misc$sample.flag){
    if(accuracy.type %in% tolower(c("LRpos", "LRneg", "RD", "LLRpos", "LLRneg", "LDOR", "DOR"))){
      stop("The statistics is not the default return. Please let \"nsample=TRUE\" in the \"meta4diag()\" function.")
    }
  }
  
  cat('Diagnostic accuracies ')
  if(accuracy.type=="sens" || accuracy.type=="tpr"){
    cat('true positive rate (sensitivity): \n')
    a = object[["summary.fitted.(Se)"]]
  }
  if(accuracy.type=="spec" || accuracy.type=="tnr"){
    cat('true negative rate (specificity): \n')
    a = object[["summary.fitted.(Sp)"]]
  }
  if(accuracy.type=="fpr"){
    cat('false positive rate (1-specificity): \n')
    a = object[["summary.fitted.(1-Sp)"]]
  }
  if(accuracy.type=="fnr"){
    cat('false negative rate (1-sensitivity): \n')
    a = object[["summary.fitted.(1-Se)"]]
  }
  if(accuracy.type=="lrpos"){
    cat('positive likelihood ratio (LR+): \n')
    a = object[["summary.fitted.LRpos"]]
  }
  if(accuracy.type=="lrneg"){
    cat('negative likelihood ratio (LR-): \n')
    a = object[["summary.fitted.LRneg"]]
  }
  if(accuracy.type=="dor"){
    cat('diagnostic odds ratio (DOR): \n')
    a = object[["summary.fitted.DOR"]]
  }
  if(accuracy.type=="ldor"){
    cat('log diagnostic odds ratio (LDOR): \n')
    a = object[["summary.fitted.LDOR"]]
  }
  if(accuracy.type=="rd"){
    cat('risk difference (RD): \n')
    a = object[["summary.fitted.RD"]]
  }
  if(accuracy.type=="llrpos"){
    cat('log positive likelihood ratio (LLR+): \n')
    a = object[["summary.fitted.LLRpos"]]
  }
  if(accuracy.type=="llrneg"){
    cat('log negative likelihood ratio (LLR-): \n')
    a = object[["summary.fitted.LLRneg"]]
  }
  return(a)
}
