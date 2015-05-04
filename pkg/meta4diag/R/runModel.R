runModel <- function(outdata, outpriors, model.type=1, link="logit", level = c(0.025, 0.5, 0.975), verbose=FALSE){
  
  if(!is.element("INLA", installed.packages()[,1])){
    install.packages("INLA",dependencies=TRUE, repos="http://www.math.ntnu.no/inla/R/stable")
    require("INLA", quietly = TRUE)
  }
  if (!(sum(search()=="package:INLA")==1)){
    require("INLA", quietly = TRUE)
  }
  
  N = dim(outdata$internaldata)[1]
  varnames = names(outdata$internaldata)
  nnv = c("studynames", "Y", "id", "Ntrials") 
  nnvn = unlist(lapply(nnv, function(x) which(varnames==x)))
  varnames = varnames[-nnvn]
  
  lc1.ind  = c(agrep("mu", varnames, max.distance=0), agrep("alpha", varnames, max.distance=0))
  lc2.ind  = c(agrep("nu", varnames, max.distance=0), agrep("beta", varnames, max.distance=0))
  
  lc1text = paste("inla.make.lincomb(",paste(varnames[lc1.ind], "=1",sep="", collapse = ", "),")",sep="")
  lc2text = paste("inla.make.lincomb(",paste(varnames[lc2.ind], "=1",sep="", collapse = ", "),")",sep="")
  
  if(model.type==1){names.fitted = c("(Se)","(Sp)")}
  if(model.type==2){names.fitted = c("(Se)","(1-Sp)")}
  if(model.type==3){names.fitted = c("(1-Se)","(Sp)")}
  if(model.type==4){names.fitted = c("(1-Se)","(1-Sp)")}
  
  names.summarized.fitted = paste("mean(logit",names.fitted,")",sep="")
  
  lc1 = eval(parse(text=lc1text))
  names(lc1) = names.summarized.fitted[1]
  lc2 = eval(parse(text=lc2text))
  names(lc2) = names.summarized.fitted[2]
  lc = c(lc1, lc2) 
  
  level = c(level,0.025, 0.5, 0.975)
  level = sort(unique(level))
  
  if(!outpriors$wishart.flag){
    prec1 = outpriors$prec1
    prec2 = outpriors$prec2
    cor = outpriors$cor
    
    fm <- as.formula(paste("Y ~ f(id, model=\"2diid\", hyper=list(prec1 = prec1,prec2 = prec2,cor = cor), n=N) + ", 
                           paste(varnames, collapse= " + "), "-1"))
    
    model = INLA::inla(fm, family="binomial", data=outdata$internaldata, Ntrials=Ntrials, quantiles=level,
                 verbose=verbose, lincomb = lc,  
                 control.inla=list(strategy="gaussian", 
                                   lincomb.derived.correlation.matrix = TRUE),
                 control.predictor=list(compute=TRUE),
                 control.family = list(link = link),
                 control.compute = list(dic = TRUE, waic = TRUE, cpo=TRUE, mlik = TRUE,config=TRUE))
    
    if(!model$ok){
      stop("Something wrong while running model with data! Please set verbose=TRUE to check!!!!")
    }
  }else{
    prec1 = outpriors$prec1
    
    fm <- as.formula(paste("Y ~ f(id, model=\"iid2d\", hyper=list(prec1 = prec1), n=N) + ", 
                           paste(varnames, collapse= " + "), "-1"))
    
    data = outdata$internaldata
    data_Wishart <- rbind(data[seq(1,N,by=2),], data[seq(2,N,by=2),])
    data_Wishart$id <- 1:N
    
    model = INLA::inla(fm, family="binomial", data=data_Wishart, Ntrials=Ntrials, quantiles=level,
                 verbose=verbose, lincomb = lc,  
                 control.inla=list(strategy="gaussian", 
                                   lincomb.derived.correlation.matrix = TRUE),
                 control.predictor=list(compute=TRUE),
                 control.family = list(link = link),
                 control.compute = list(dic = TRUE, waic = TRUE, cpo=TRUE, mlik = TRUE,config=TRUE))
    
    if(!model$ok){
      stop("Something wrong while running model with data! Please set verbose=TRUE to check!!!!")
    } 
  }
  
  model$model.type=model.type
  model$link = link
  model$level = level
  model$verbose=verbose

  return(model)
}