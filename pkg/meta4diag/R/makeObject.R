makeObject <- function(outdata, outpriors, model, nsample=5000){
  
  level = sort(unique(c(model$level,0.025,0.5,0.975)))
  effect.length = length(level) + 2
  est = list()
  if(outpriors$wishart.flag){
    temp = outdata$internaldata
    temp <- rbind(temp[seq(1,dim(temp)[1],by=2),], temp[seq(2,dim(temp)[1],by=2),])
    temp$id <- 1:dim(temp)[1]
    est$outdata = temp
  }else{
    est$outdata = outdata$internaldata
  }
  est$data = outdata$originaldata
  colnames(est$data) = tolower(colnames(est$data))
  est$priors.density = outpriors$density
  
  
  ########## model type
  if(model$model.type==1){
    est$names.fitted = c("Se","Sp")
    est$names.transf.fitted = c("1-Se","1-Sp")
  }
  if(model$model.type==2){
    est$names.fitted = c("Se","1-Sp")
    est$names.transf.fitted = c("1-Se","Sp")
  }
  if(model$model.type==3){
    est$names.fitted = c("1-Se","Sp")
    est$names.transf.fitted = c("Se","1-Sp")
  }
  if(model$model.type==4){
    est$names.fitted = c("1-Se","1-Sp")
    est$names.transf.fitted = c("Se","Sp")
  }
  
  n.marginal.point = dim(model$marginals.fixed[[1]])[1]
  ########## model cpu & call
  est$cpu.used = model[["cpu.used"]]
  est$call = model[["call"]]
  
  ######## fixed
  est$summary.fixed = model[["summary.fixed"]][,c(1:effect.length)]
  est$marginals.fixed = model[["marginals.fixed"]]
  
  ######## summarized.fixed
  names.summarized.fixed = paste("mean(logit(",est$names.fitted,"))",sep="")
  est$summary.summarized.fixed = model[["summary.lincomb.derived"]][,c(2:(effect.length+1))]
  est$marginals.summarized.fixed = model[["marginals.lincomb.derived"]]
  
  
  ####### summarized.fitted
  names.summarized.fitted = paste("mean(",est$names.fitted,")",sep="")
  marginals.summarized.fitted.temp = lapply(names.summarized.fixed, function(y){
    INLA::inla.tmarginal(function(x) .invlogit(x), model[["marginals.lincomb.derived"]][[y]], n=n.marginal.point)
  })
  names(marginals.summarized.fitted.temp) = names.summarized.fitted
  # est$marginals.summarized.fitted = marginals.summarized.fitted.temp
  
  se.marginals = marginals.summarized.fitted.temp[[1]]
  sp.marginals = marginals.summarized.fitted.temp[[2]]
  suminfo1 = .summary.marginal(se.marginals,level=level)
  suminfo2 = .summary.marginal(sp.marginals,level=level)
  summary.summarized.fitted.temp = rbind(suminfo1, suminfo2)
  summary.summarized.fitted.temp = as.matrix(summary.summarized.fitted.temp)
  rownames(summary.summarized.fitted.temp) = names.summarized.fitted
  
  ####### summarized.transf.fitted
  names.summarized.transf.fitted = paste("mean(",est$names.transf.fitted,")",sep="")
  marginals.summarized.transf.fitted.temp = lapply(names.summarized.fixed, function(y){
    INLA::inla.tmarginal(function(x) 1-.invlogit(x), model[["marginals.lincomb.derived"]][[y]], n=n.marginal.point)
  })
  names(marginals.summarized.transf.fitted.temp) = names.summarized.transf.fitted
  marginals.summarized.transf.fitted = marginals.summarized.transf.fitted.temp
  
  se.marginals = marginals.summarized.transf.fitted[[1]]
  sp.marginals = marginals.summarized.transf.fitted[[2]]
  suminfo1 = .summary.marginal(se.marginals,level=level)
  suminfo2 = .summary.marginal(sp.marginals,level=level)
  summary.summarized.transf.fitted.temp = rbind(suminfo1, suminfo2)
  summary.summarized.transf.fitted.temp = as.matrix(summary.summarized.transf.fitted.temp)
  rownames(summary.summarized.transf.fitted.temp) = names.summarized.transf.fitted
  
  summary.summarized.fitted.total = rbind(summary.summarized.fitted.temp,summary.summarized.transf.fitted.temp)
  est$summary.summarized.fitted = summary.summarized.fitted.total
  
  marginals.summarized.fitted.total = append(marginals.summarized.fitted.temp,marginals.summarized.transf.fitted)
  est$marginals.summarized.fitted = marginals.summarized.fitted.total
  
  ############# hyperpar: need transformation
  names.var = paste("var(logit(",est$names.fitted,"))",sep="")
  names.cor = "cor(logits)"
  tau1.marginals = model[["marginals.hyperpar"]][[1]]
  var1.marginals = INLA::inla.tmarginal(function(x) 1/x, tau1.marginals, n=n.marginal.point)
  tau2.marginals = model[["marginals.hyperpar"]][[2]]
  var2.marginals = INLA::inla.tmarginal(function(x) 1/x, tau2.marginals, n=n.marginal.point)  
  marginals.hyperpar.temp = list()
  marginals.hyperpar.temp[[names.var[1]]] = var1.marginals
  marginals.hyperpar.temp[[names.var[2]]] = var2.marginals
  marginals.hyperpar.temp[[names.cor]] = model[["marginals.hyperpar"]][[3]]
  est$marginals.hyperpar = marginals.hyperpar.temp
  
  var1.marginals = est$marginals.hyperpar[[1]]
  var2.marginals = est$marginals.hyperpar[[2]]
  suminfo1 = .summary.marginal(var1.marginals,level=level)
  suminfo2 = .summary.marginal(var2.marginals,level=level)
  
  summary.hyperpar.temp = rbind(suminfo1, suminfo2, model[["summary.hyperpar"]][3,c(1:effect.length)])
  summary.hyperpar.temp = as.matrix(summary.hyperpar.temp)
  rownames(summary.hyperpar.temp) = c(names.var,names.cor)
  est$summary.hyperpar = summary.hyperpar.temp
  
  #############
  est$summarized.fixed.correlation.matrix = model[["misc"]]$lincomb.derived.correlation.matrix
  est$summarized.fixed.covariance.matrix = model[["misc"]]$lincomb.derived.covariance.matrix
  
  
  if(outpriors$wishart.flag){
    fitted1.ind = seq(1,dim(est$data)[1],by=1)
    fitted2.ind = seq((dim(est$data)[1]+1),dim(est$outdata)[1],by=1)
  }else{
    fitted1.ind = seq(1,dim(est$outdata)[1],by=2)
    fitted2.ind = seq(2,dim(est$outdata)[1],by=2)
  }
  
  ############# predict
  studynames = as.character(est$outdata$studynames[fitted1.ind])
  summary.predict.temp1 = as.matrix(model[["summary.linear.predictor"]][fitted1.ind,c(1:effect.length)])
  rownames(summary.predict.temp1) = studynames
  summary.predict.temp2 = as.matrix(model[["summary.linear.predictor"]][fitted2.ind,c(1:effect.length)])
  rownames(summary.predict.temp2) = studynames
  est[[paste("summary.predict.(",est$names.fitted[1],")",sep="")]] = summary.predict.temp1
  est[[paste("summary.predict.(",est$names.fitted[2],")",sep="")]] = summary.predict.temp2
  
  marginals.predict.temp1 = lapply(fitted1.ind, function(y){model[["marginals.linear.predictor"]][[y]]})
  names(marginals.predict.temp1) = studynames
  marginals.predict.temp2 = lapply(fitted2.ind, function(y){model[["marginals.linear.predictor"]][[y]]})
  names(marginals.predict.temp2) = studynames
  est[[paste("marginals.predict.(",est$names.fitted[1],")",sep="")]] = marginals.predict.temp1
  est[[paste("marginals.predict.(",est$names.fitted[2],")",sep="")]] = marginals.predict.temp2
  
  ############# transform to other accurary that not fitted here directly
  transfunc = function(x) 1-.invlogit(x)
  marginals.transf.fitted1 = lapply(marginals.predict.temp1, function(x){INLA::inla.tmarginal(transfunc, x, n=n.marginal.point)})
  marginals.transf.fitted2 = lapply(marginals.predict.temp2, function(x){INLA::inla.tmarginal(transfunc, x, n=n.marginal.point)})
  
  suminfo1 = do.call(rbind,lapply(marginals.transf.fitted1, function(x) .summary.marginal(x,level=level)))
  suminfo2 = do.call(rbind,lapply(marginals.transf.fitted2, function(x) .summary.marginal(x,level=level)))
  
  est[[paste("summary.fitted.(",est$names.transf.fitted[1],")",sep="")]] = suminfo1
  est[[paste("summary.fitted.(",est$names.transf.fitted[2],")",sep="")]] = suminfo2
  
  ############# fitted
  summary.fitted.temp1 = as.matrix(model[["summary.fitted.values"]][fitted1.ind,c(1:effect.length)])
  rownames(summary.fitted.temp1) = studynames
  summary.fitted.temp2 = as.matrix(model[["summary.fitted.values"]][fitted2.ind,c(1:effect.length)])
  rownames(summary.fitted.temp2) = studynames
  est[[paste("summary.fitted.(",est$names.fitted[1],")",sep="")]] = summary.fitted.temp1
  est[[paste("summary.fitted.(",est$names.fitted[2],")",sep="")]] = summary.fitted.temp2
  
  marginals.fitted.temp1 = lapply(fitted1.ind, function(y){model[["marginals.fitted.values"]][[y]]})
  names(marginals.fitted.temp1) = studynames
  marginals.fitted.temp2 = lapply(fitted2.ind, function(y){model[["marginals.fitted.values"]][[y]]})
  names(marginals.fitted.temp2) = studynames
  est[[paste("marginals.fitted.(",est$names.fitted[1],")",sep="")]] = marginals.fitted.temp1
  est[[paste("marginals.fitted.(",est$names.fitted[2],")",sep="")]] = marginals.fitted.temp2
  
  
  ############# samples
  if(is.logical(nsample)){
    if(nsample==FALSE){
      est$misc$sample.flag = FALSE
    }else{
      est$misc$sample.flag = TRUE
      message("Argument \"nsample\" set to TRUE, we will give 5000 samples!")
      est$misc$nsample = 5000
    }
  }else if(is.numeric(nsample)){
    est$misc$sample.flag = TRUE
    if(abs(nsample - round(nsample)) < .Machine$double.eps^0.5){
      est$misc$nsample = nsample
    }else{
      message("Argument \"nsample\" should be a integer, we round the given number to interger.")
      est$misc$nsample = round(nsample, digits = 0)
    }
  }else{
    message("Argument \"nsample\" should be TRUE, FALSE or a integer, we set it to FALSE!")
  }
  if(est$misc$sample.flag){
    options(warn=-1)
    samples = INLA::inla.posterior.sample(n = nsample, model)
    options(warn=0)
    predictors.samples = do.call(cbind,lapply(c(1:nsample), function(x) samples[[x]]$latent[1:dim(outdata$internaldata)[1]]))
    fixed.samples = do.call(cbind,lapply(c(1:nsample), function(x){
      length.latent = length(samples[[x]]$latent)
      a = samples[[x]]$latent[(length.latent-dim(model$summary.fixed)[1]+1):length.latent]
      return(a)
    }))
    fixed.names = rownames(model$summary.fixed)
    rownames(fixed.samples) = fixed.names
    ind.fitted1  = c(agrep("mu", fixed.names, max.distance=0), agrep("alpha", fixed.names, max.distance=0))
    ind.fitted2  = c(agrep("nu", fixed.names, max.distance=0), agrep("beta", fixed.names, max.distance=0))
    if(length(ind.fitted1)==1){
      mean.logit.fitted1.samples = fixed.samples[ind.fitted1,]
      mean.logit.fitted2.samples = fixed.samples[ind.fitted2,]
    }else{
      mean.logit.fitted1.samples = colSums(fixed.samples[ind.fitted1,])
      mean.logit.fitted2.samples = colSums(fixed.samples[ind.fitted2,])
    }
    
    mean.fitted1.samples = .invlogit(mean.logit.fitted1.samples)
    mean.fitted2.samples = .invlogit(mean.logit.fitted2.samples)
    
    if(model$model.type==1){
      mean.LRpos.samples = mean.fitted1.samples/(1-mean.fitted2.samples)
      mean.LRneg.samples = (1-mean.fitted1.samples)/mean.fitted2.samples
      mean.DOR.samples = mean.LRpos.samples/mean.LRneg.samples
    }
    if(model$model.type==2){
      mean.LRpos.samples = mean.fitted1.samples/mean.fitted2.samples
      mean.LRneg.samples = (1-mean.fitted1.samples)/(1-mean.fitted2.samples)
      mean.DOR.samples = mean.LRpos.samples/mean.LRneg.samples
    }
    if(model$model.type==3){
      mean.LRpos.samples = (1-mean.fitted1.samples)/(1-mean.fitted2.samples)
      mean.LRneg.samples = mean.fitted1.samples/mean.fitted2.samples
      mean.DOR.samples = mean.LRpos.samples/mean.LRneg.samples
    }
    if(model$model.type==4){
      mean.LRpos.samples = (1-mean.fitted1.samples)/mean.fitted2.samples
      mean.LRneg.samples = mean.fitted1.samples/(1-mean.fitted2.samples)
      mean.DOR.samples = mean.LRpos.samples/mean.LRneg.samples
    }
    
    summary.LRpos.temp = .summary.samples(mean.LRpos.samples, level=level)
    summary.LRneg.temp = .summary.samples(mean.LRneg.samples, level=level)
    summary.DOR.temp = .summary.samples(mean.DOR.samples, level=level)
    
    summary.summarized.statistics = rbind(summary.LRpos.temp, summary.LRneg.temp, summary.DOR.temp)
    rownames(summary.summarized.statistics) = c("mean(LRpos)","mean(LRneg)","mean(DOR)")
    est$summary.summarized.statistics = summary.summarized.statistics
    
    ############# fitted samples to calculate LRpos, LRneg, DOR for each study
    fitted1.samples = .invlogit(predictors.samples[fitted1.ind,])
    fitted2.samples = .invlogit(predictors.samples[fitted2.ind,])
    
    if(model$model.type==1){
      LRpos.samples = fitted1.samples/(1-fitted2.samples)
      LRneg.samples = (1-fitted1.samples)/fitted2.samples
      DOR.samples = LRpos.samples/LRneg.samples
    }
    if(model$model.type==2){
      LRpos.samples = fitted1.samples/fitted2.samples
      LRneg.samples = (1-fitted1.samples)/(1-fitted2.samples)
      DOR.samples = LRpos.samples/LRneg.samples
    }
    if(model$model.type==3){
      LRpos.samples = (1-fitted1.samples)/(1-fitted2.samples)
      LRneg.samples = fitted1.samples/fitted2.samples
      DOR.samples = LRpos.samples/LRneg.samples
    }
    if(model$model.type==4){
      LRpos.samples = (1-fitted1.samples)/fitted2.samples
      LRneg.samples = fitted1.samples/(1-fitted2.samples)
      DOR.samples = LRpos.samples/LRneg.samples
    }
    
    summary.fitted.LRpos.temp = t(apply(LRpos.samples, 1, function(x) .summary.samples(x, level=level)))
    rownames(summary.fitted.LRpos.temp) = studynames
    summary.fitted.LRneg.temp = t(apply(LRneg.samples, 1, function(x) .summary.samples(x, level=level)))
    rownames(summary.fitted.LRneg.temp) = studynames
    summary.fitted.DOR.temp = t(apply(DOR.samples, 1, function(x) .summary.samples(x, level=level)))
    rownames(summary.fitted.DOR.temp) = studynames
    est$summary.fitted.LRpos = summary.fitted.LRpos.temp
    est$summary.fitted.LRneg = summary.fitted.LRneg.temp
    est$summary.fitted.DOR = summary.fitted.DOR.temp
    
    if(model$model.type==1){
      est$Se.samples = fitted1.samples
      est$Sp.samples = fitted2.samples
      est[["mean(Se).samples"]] = mean.fitted1.samples
      est[["mean(Sp).samples"]] = mean.fitted2.samples
    }
    if(model$model.type==2){
      est$Se.samples = fitted1.samples
      est$Sp.samples = 1-fitted2.samples
      est[["mean(Se).samples"]] = mean.fitted1.samples
      est[["mean(Sp).samples"]] = 1-mean.fitted2.samples
    }
    if(model$model.type==3){
      est$Se.samples = 1-fitted1.samples
      est$Sp.samples = fitted2.samples
      est[["mean(Se).samples"]] = 1-mean.fitted1.samples
      est[["mean(Sp).samples"]] = mean.fitted2.samples
    }
    if(model$model.type==4){
      est$Se.samples = 1-fitted1.samples
      est$Sp.samples = 1-fitted2.samples
      est[["mean(Se).samples"]] = 1-mean.fitted1.samples
      est[["mean(Sp).samples"]] = 1-mean.fitted2.samples
    }
  }
  
  variables.names = colnames(est$data)
  ############# scores
  est$waic = model[["waic"]]
  est$mlik = model[["mlik"]]
  est$cpo = model[["cpo"]]
  est$dic = model[["dic"]]
  
  ############# save inla result
  est$inla.result = model
  ############# save general setting
  est$misc$var.prior.list = outpriors$original.setting$var1
  est$misc$var2.prior.list = outpriors$original.setting$var2
  est$misc$cor.prior.list = outpriors$original.setting$cor
  
  est$misc$covariates = outdata$covariates.setting
  if(is.null(outdata$covariates.setting) || outdata$covariates.setting==FALSE){
    est$misc$covariates.flag=FALSE
  } else{
    est$misc$covariates.flag=TRUE
  }
  if(is.null(outdata$originaldata$modality)){
    est$misc$modality.flag=FALSE
  }else{
    est$misc$modality.flag=TRUE
    est$misc$modality.place.in.data = which(variables.names=="modality")
    est$misc$modality.level = length(unique(outdata$originaldata$modality))
  }
  est$misc$link = model$link
  est$misc$model.type = model$model.type
  
  ################### make new class
  class(est) = 'meta4diag'
  return(est)
  
}