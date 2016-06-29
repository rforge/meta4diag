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
  if(length(accuracy.type)!=1){
    stop("Input should be Only one accuracy.")
  }
  #cat('Diagnostic accuracies ')
  if(accuracy.type=="sens" || accuracy.type=="tpr"){
    #cat('true positive rate (sensitivity): \n')
    a = object[["summary.predictor.(Se)"]]
  }
  if(accuracy.type=="spec" || accuracy.type=="tnr"){
    #cat('true negative rate (specificity): \n')
    a = object[["summary.predictor.(Sp)"]]
  }
  if(accuracy.type=="fpr"){
    #cat('false positive rate (1-specificity): \n')
    a = object[["summary.predictor.(1-Sp)"]]
  }
  if(accuracy.type=="fnr"){
    #cat('false negative rate (1-sensitivity): \n')
    a = object[["summary.predictor.(1-Se)"]]
  }
  if(accuracy.type=="lrpos"){
    #cat('positive likelihood ratio (LR+): \n')
    a = object[["summary.study.specific.LRpos"]]
  }
  if(accuracy.type=="lrneg"){
    #cat('negative likelihood ratio (LR-): \n')
    a = object[["summary.study.specific.LRneg"]]
  }
  if(accuracy.type=="dor"){
    #cat('diagnostic odds ratio (DOR): \n')
    a = object[["summary.study.specific.DOR"]]
  }
  if(accuracy.type=="ldor"){
    #cat('log diagnostic odds ratio (LDOR): \n')
    a = object[["summary.study.specific.LDOR"]]
  }
  if(accuracy.type=="rd"){
    #cat('risk difference (RD): \n')
    a = object[["summary.study.specific.RD"]]
  }
  if(accuracy.type=="llrpos"){
    #cat('log positive likelihood ratio (LLR+): \n')
    a = object[["summary.study.specific.LLRpos"]]
  }
  if(accuracy.type=="llrneg"){
    #cat('log negative likelihood ratio (LLR-): \n')
    a = object[["summary.study.specific.LLRneg"]]
  }
  fm = list()
  fm$accuracy.type = accuracy.type
  fm$fitted.value = a
  class(fm) = "fitted.meta4diag"
  return(fm)
}


print.fitted.meta4diag = function(x,...){
  accuracy.type = tolower(x$accuracy.type)
  cat('Diagnostic accuracies ')
  if(accuracy.type=="sens" || accuracy.type=="tpr"){
    cat('true positive rate (sensitivity): \n')
  }
  if(accuracy.type=="spec" || accuracy.type=="tnr"){
    cat('true negative rate (specificity): \n')
  }
  if(accuracy.type=="fpr"){
    cat('false positive rate (1-specificity): \n')
  }
  if(accuracy.type=="fnr"){
    cat('false negative rate (1-sensitivity): \n')
  }
  if(accuracy.type=="lrpos"){
    cat('positive likelihood ratio (LR+): \n')
  }
  if(accuracy.type=="lrneg"){
    cat('negative likelihood ratio (LR-): \n')
  }
  if(accuracy.type=="dor"){
    cat('diagnostic odds ratio (DOR): \n')
  }
  if(accuracy.type=="ldor"){
    cat('log diagnostic odds ratio (LDOR): \n')
  }
  if(accuracy.type=="rd"){
    cat('risk difference (RD): \n')
  }
  if(accuracy.type=="llrpos"){
    cat('log positive likelihood ratio (LLR+): \n')
  }
  if(accuracy.type=="llrneg"){
    cat('log negative likelihood ratio (LLR-): \n')
  }
  print(round(x$fitted.value,3))
  cat("\n")
}
