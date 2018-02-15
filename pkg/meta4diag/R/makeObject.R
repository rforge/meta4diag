makeObject <- function(model, nsample=FALSE,seed = 0L){
  if(requireNamespace("INLA", quietly = TRUE)){
    if (!(sum(search()=="package:INLA"))==1){
      stop("INLA need to be loaded! \n
Please use the following command to load INLA,\n
library(INLA) \n")
    }
    
    
    ##### read a bit the previous settings
    outdata = model$inherent.outdata
    outpriors = model$inherent.priors
    
    model.type = outdata$model.type
    
    covariates.flag = outdata$covariates.flag
    modality.flag = outdata$modality.flag
    
    quantiles = sort(unique(c(model$quantiles,0.025,0.5,0.975)))
    effect.length = length(quantiles) + 2
    
    #### construct est
    est = list()
    est$data = outdata$originaldata
    est$outdata = outdata$internaldata
    colnames(est$data) = tolower(colnames(est$data))
    est$priors.density = outpriors$density
    
    
    
    ########## model type, names.fitted - which are modelled directly in INLA
    if(model.type==1){
      est$names.fitted = c("Se","Sp")
      names.transf.fitted = c("1-Se","1-Sp")
    }
    if(model.type==2){
      est$names.fitted = c("Se","1-Sp")
      names.transf.fitted = c("1-Se","Sp")
    }
    if(model.type==3){
      est$names.fitted = c("1-Se","Sp")
      names.transf.fitted = c("Se","1-Sp")
    }
    if(model.type==4){
      est$names.fitted = c("1-Se","1-Sp")
      names.transf.fitted = c("Se","Sp")
    }
    
    n.marginal.point = dim(model$marginals.fixed[[1]])[1]
    ########## model cpu & call
    est$cpu.used = model[["cpu.used"]]
    est$call = model[["call"]]
    
    #############################################################################
    ######## fixed
    #############################################################################
    est$summary.fixed = model[["summary.fixed"]][,c(1:effect.length)]
    est$marginals.fixed = model[["marginals.fixed"]]
    
    #############################################################################
    ############# hyperpar: need transformation
    #############################################################################
    names.var = c("var_phi","var_psi")
    names.cor = "cor"
    tau1.marginals = model[["marginals.hyperpar"]][[1]]
    var1.marginals = INLA::inla.tmarginal(function(x) 1/x, tau1.marginals, n=1024)
    tau2.marginals = model[["marginals.hyperpar"]][[2]]
    var2.marginals = INLA::inla.tmarginal(function(x) 1/x, tau2.marginals, n=1024)  
    marginals.hyperpar.temp = list()
    marginals.hyperpar.temp[[names.var[1]]] = var1.marginals
    marginals.hyperpar.temp[[names.var[2]]] = var2.marginals
    marginals.hyperpar.temp[[names.cor]] = model[["marginals.hyperpar"]][[3]]
    est$marginals.hyperpar = marginals.hyperpar.temp
    
    var1.marginals = est$marginals.hyperpar[[1]]
    var2.marginals = est$marginals.hyperpar[[2]]
    suminfo1 = .summary.marginal(var1.marginals,level=quantiles)
    suminfo2 = .summary.marginal(var2.marginals,level=quantiles)
    
    summary.hyperpar.temp = rbind(suminfo1, suminfo2, model[["summary.hyperpar"]][3,c(1:effect.length)])
    summary.hyperpar.temp = as.matrix(summary.hyperpar.temp)
    rownames(summary.hyperpar.temp) = c(names.var,names.cor)
    est$summary.hyperpar = summary.hyperpar.temp
    
    #############################################################################
    ######## study names and modality level names
    #############################################################################
    studynames = as.character(outdata$originaldata$studynames)
    
    if(modality.flag){modalitynames = outdata$modality.level}
    #############################################################################
    ######## Expected g=="logit","probit","cloglog" E(g(..)) and E(g(..))
    #############################################################################
    if(!modality.flag){ # no modality
      if(!covariates.flag){ # no covariate
        est[[paste("summary.expected.",model$link,".accuracy",sep="")]] = model[["summary.lincomb.derived"]][,c(2:(effect.length+1))]
        est[[paste("marginals.expected.",model$link,".accuracy",sep="")]] = model[["marginals.lincomb.derived"]]
        # est[[paste("correlation.matrix.expected.",model$link,".accuracy",sep="")]] = model[["misc"]]$lincomb.derived.correlation.matrix
        # est[[paste("covariance.matrix.expected.",model$link,".accuracy",sep="")]] = model[["misc"]]$lincomb.derived.covariance.matrix
        
        correlation.temp = model[["misc"]]$lincomb.derived.correlation.matrix[1,2]
        names(correlation.temp) = "Cor(mu, nu)"
        est[[paste("correlation.expected.",model$link,".accuracy",sep="")]] = correlation.temp
        
        covariance.temp = model[["misc"]]$lincomb.derived.covariance.matrix[1,2]
        names(covariance.temp) = "Cov(mu, nu)"
        est[[paste("covariance.expected.",model$link,".accuracy",sep="")]] = covariance.temp
      } else{ # has covariate
        names.summarized.fixed1 = paste("mean(",model$link,".",est$names.fitted[1],".",studynames,")",sep="")
        names.summarized.fixed2 = paste("mean(",model$link,".",est$names.fitted[2],".",studynames,")",sep="")
        names.summarized.fixed = c(names.summarized.fixed1, names.summarized.fixed2)
        
        summary.temp = as.matrix(model[["summary.lincomb.derived"]][,c(2:(effect.length+1))])
        marginals.temp = model[["marginals.lincomb.derived"]]
        rownames(summary.temp) = names.summarized.fixed
        names(marginals.temp) = names.summarized.fixed
        est[[paste("summary.expected.",model$link,".accuracy",sep="")]] = summary.temp
        est[[paste("marginals.expected.",model$link,".accuracy",sep="")]] = marginals.temp
          
        correlation.temp = do.call(rbind, lapply(1:length(studynames), 
                                                            function(i) model[["misc"]]$lincomb.derived.correlation.matrix[i, i+length(studynames)]))
        colnames(correlation.temp) = paste("Cor(",paste("E[g(",est$names.fitted,")]",sep="", collapse=", "),")",sep="")
        correlation.temp = as.matrix(correlation.temp)
        rownames(correlation.temp) = studynames
        est[[paste("correlation.expected.",model$link,".accuracy",sep="")]] = correlation.temp
        
        covariance.temp = as.matrix(do.call(rbind, lapply(1:length(studynames), 
                                                                     function(i) model[["misc"]]$lincomb.derived.covariance.matrix[i, i+length(studynames)])))
        colnames(covariance.temp) = paste("Cov(",paste("E[g(",est$names.fitted,")]",sep="", collapse=", "),")",sep="")
        covariance.temp = as.matrix(covariance.temp)
        rownames(covariance.temp) = studynames
        est[[paste("covariance.expected.",model$link,".accuracy",sep="")]] = covariance.temp
      }
    }else{ # has modality
      um = modalitynames
      if(!covariates.flag){ # no covariate
        est[[paste("summary.expected.",model$link,".accuracy",sep="")]] = model[["summary.lincomb.derived"]][,c(2:(effect.length+1))]
        est[[paste("marginals.expected.",model$link,".accuracy",sep="")]] = model[["marginals.lincomb.derived"]]
        #est$correlation.matrix.expected.transf.accuracies = model[["misc"]]$lincomb.derived.correlation.matrix
        #est$covariance.matrix.expected.transf.accuracies = model[["misc"]]$lincomb.derived.covariance.matrix
        
        correlation.temp = do.call(rbind, lapply(1:length(um), 
                                                            function(i) model[["misc"]]$lincomb.derived.correlation.matrix[i, i+length(um)]))
        colnames(correlation.temp) = paste("Cor(",paste("E[g(",est$names.fitted,")]",sep="", collapse=", "),")",sep="")
        correlation.temp = as.matrix(correlation.temp)
        rownames(correlation.temp) = um
        est[[paste("correlation.expected.",model$link,".accuracy",sep="")]] = correlation.temp
        
        covariance.temp = do.call(rbind, lapply(1:length(um), 
                                                           function(i) model[["misc"]]$lincomb.derived.covariance.matrix[i, i+length(um)]))
        colnames(covariance.temp) = paste("Cov(",paste("E[g(",est$names.fitted,")]",sep="", collapse=", "),")",sep="")
        covariance.temp = as.matrix(covariance.temp)
        rownames(covariance.temp) = um
        est[[paste("covariance.expected.",model$link,".accuracy",sep="")]] = covariance.temp
      } else{ # has covariate
        names.summarized.fixed1 = paste("mean(",model$link,".",est$names.fitted[1],".",studynames,")",sep="")
        names.summarized.fixed2 = paste("mean(",model$link,".",est$names.fitted[2],".",studynames,")",sep="")
        names.summarized.fixed = c(names.summarized.fixed1, names.summarized.fixed2)
        
        summary.temp = as.matrix(model[["summary.lincomb.derived"]][,c(2:(effect.length+1))])
        marginals.temp = model[["marginals.lincomb.derived"]]
        rownames(summary.temp) = names.summarized.fixed
        names(marginals.temp) = names.summarized.fixed
        est[[paste("summary.expected.",model$link,".accuracy",sep="")]] = summary.temp
        est[[paste("marginals.expected.",model$link,".accuracy",sep="")]] = marginals.temp
        
        correlation.temp = do.call(rbind, lapply(1:length(studynames), 
                                                            function(i) model[["misc"]]$lincomb.derived.correlation.matrix[i, i+length(studynames)]))
        colnames(correlation.temp) = paste("Cor(",paste("E[g(",est$names.fitted,")]",sep="", collapse=", "),")",sep="")
        correlation.temp = as.matrix(correlation.temp)
        rownames(correlation.temp) = studynames
        est[[paste("correlation.expected.",model$link,".accuracy",sep="")]] = correlation.temp
        
        covariance.temp = as.matrix(do.call(rbind, lapply(1:length(studynames), 
                                                                     function(i) model[["misc"]]$lincomb.derived.covariance.matrix[i, i+length(studynames)])))
        colnames(covariance.temp) = paste("Cov(",paste("E[g(",est$names.fitted,")]",sep="", collapse=", "),")",sep="")
        covariance.temp = as.matrix(covariance.temp)
        rownames(covariance.temp) = studynames
        est[[paste("covariance.expected.",model$link,".accuracy",sep="")]] = covariance.temp
      }
    }
    
    #############################################################################
    ########## Expected accuracy E(Se) and E(Sp) if model.type=1
    #############################################################################
    if(!modality.flag){ # no modality
      if(!covariates.flag){ # no covariates
        ####### original----from model
        names.expected.accuracy.original = paste("mean(",est$names.fitted,")",sep="")
        
        names.temp = names(model[["marginals.lincomb.derived"]])
        marginals.expected.accuracy.original = lapply(names.temp, function(y){
          INLA::inla.tmarginal(model$inv.linkfunc, model[["marginals.lincomb.derived"]][[y]], n=n.marginal.point)
        })
        names(marginals.expected.accuracy.original) = names.expected.accuracy.original
        
        se.marginals = marginals.expected.accuracy.original[[1]]
        sp.marginals = marginals.expected.accuracy.original[[2]]
        suminfo1 = .summary.marginal(se.marginals,level=quantiles)
        suminfo2 = .summary.marginal(sp.marginals,level=quantiles)
        summary.expected.accuracy.original = rbind(suminfo1, suminfo2)
        summary.expected.accuracy.original = as.matrix(summary.expected.accuracy.original)
        rownames(summary.expected.accuracy.original) = names.expected.accuracy.original
        
        ####### transformed----from inla.tmarginal
        names.expected.accuracy.transform = paste("mean(",names.transf.fitted,")",sep="")
        
        marginals.expected.accuracy.transform = lapply(names.temp, function(y){
          INLA::inla.tmarginal(model$c.inv.linkfunc, model[["marginals.lincomb.derived"]][[y]], n=n.marginal.point)
        })
        names(marginals.expected.accuracy.transform) = names.expected.accuracy.transform
        
        se.marginals = marginals.expected.accuracy.transform[[1]]
        sp.marginals = marginals.expected.accuracy.transform[[2]]
        suminfo1 = .summary.marginal(se.marginals,level=quantiles)
        suminfo2 = .summary.marginal(sp.marginals,level=quantiles)
        summary.expected.accuracy.transform = rbind(suminfo1, suminfo2)
        summary.expected.accuracy.transform = as.matrix(summary.expected.accuracy.transform)
        rownames(summary.expected.accuracy.transform) = names.expected.accuracy.transform
        
        est$summary.expected.accuracy = rbind(summary.expected.accuracy.original, summary.expected.accuracy.transform) 
        est$marginals.expected.accuracy = append(marginals.expected.accuracy.original, marginals.expected.accuracy.transform)
      } else{
        ####### original----from model
        names.expected.accuracy.original1 = paste("mean(",est$names.fitted[1],".",studynames,")",sep="")
        names.expected.accuracy.original2 = paste("mean(",est$names.fitted[2],".",studynames,")",sep="")
        names.expected.accuracy.original = c(names.expected.accuracy.original1, names.expected.accuracy.original2)
        
        names.temp = names(model[["marginals.lincomb.derived"]])
        marginals.expected.accuracy.original = lapply(names.temp, function(y){
          INLA::inla.tmarginal(model$inv.linkfunc, model[["marginals.lincomb.derived"]][[y]], n=n.marginal.point)
        })
        names(marginals.expected.accuracy.original) = names.expected.accuracy.original
        
        suminfo = lapply(1:length(marginals.expected.accuracy.original), function(x) .summary.marginal(marginals.expected.accuracy.original[[x]],level=quantiles))
        summary.expected.accuracy.original = do.call(rbind, suminfo)
        rownames(summary.expected.accuracy.original) = names.expected.accuracy.original
        ####### transformed----from inla.tmarginal
        names.expected.accuracy.transform1 = paste("mean(",names.transf.fitted[1],".",studynames,")",sep="")
        names.expected.accuracy.transform2 = paste("mean(",names.transf.fitted[2],".",studynames,")",sep="")
        names.expected.accuracy.transform = c(names.expected.accuracy.transform1, names.expected.accuracy.transform2)
        
        marginals.expected.accuracy.transform = lapply(names.temp, function(y){
          INLA::inla.tmarginal(model$c.inv.linkfunc, model[["marginals.lincomb.derived"]][[y]], n=n.marginal.point)
        })
        names(marginals.expected.accuracy.transform) = names.expected.accuracy.transform
        
        suminfo = lapply(1:length(marginals.expected.accuracy.transform), function(x) .summary.marginal(marginals.expected.accuracy.transform[[x]],level=quantiles))
        summary.expected.accuracy.transform = do.call(rbind, suminfo)
        rownames(summary.expected.accuracy.transform) = names.expected.accuracy.transform
        
        est$summary.expected.accuracy = rbind(summary.expected.accuracy.original, summary.expected.accuracy.transform) 
        est$marginals.expected.accuracy = append(marginals.expected.accuracy.original, marginals.expected.accuracy.transform)
      }
    }else{ # has modality
      um = modalitynames
      if(!covariates.flag){ # no covariates
        ####### original----from model
        names.expected.accuracy.original = unlist(lapply(est$names.fitted,function(x) paste("mean(",x,".",um,")",sep="")))
        
        names.temp = names(model[["marginals.lincomb.derived"]])
        marginals.expected.accuracy.original = lapply(names.temp, function(y){
          INLA::inla.tmarginal(model$inv.linkfunc, model[["marginals.lincomb.derived"]][[y]], n=n.marginal.point)
        })
        names(marginals.expected.accuracy.original) = names.expected.accuracy.original
        
        suminfo = lapply(1:length(marginals.expected.accuracy.original), function(x) .summary.marginal(marginals.expected.accuracy.original[[x]],level=quantiles))
        summary.expected.accuracy.original = do.call(rbind, suminfo)
        rownames(summary.expected.accuracy.original) = names.expected.accuracy.original
        
        ####### transformed----from inla.tmarginal
        names.expected.accuracy.transform = unlist(lapply(names.transf.fitted,function(x) paste("mean(",x,".",um,")",sep="")))
        
        marginals.expected.accuracy.transform = lapply(names.temp, function(y){
          INLA::inla.tmarginal(model$c.inv.linkfunc, model[["marginals.lincomb.derived"]][[y]], n=n.marginal.point)
        })
        names(marginals.expected.accuracy.transform) = names.expected.accuracy.transform
        
        suminfo = lapply(1:length(marginals.expected.accuracy.transform), function(x) .summary.marginal(marginals.expected.accuracy.transform[[x]],level=quantiles))
        summary.expected.accuracy.transform = do.call(rbind, suminfo)
        rownames(summary.expected.accuracy.transform) = names.expected.accuracy.transform
        
        est$summary.expected.accuracy = rbind(summary.expected.accuracy.original, summary.expected.accuracy.transform) 
        est$marginals.expected.accuracy = append(marginals.expected.accuracy.original, marginals.expected.accuracy.transform)
      } else{ # has covariates
        ####### original----from model
        names.expected.accuracy.original1 = paste("mean(",est$names.fitted[1],".",studynames,")",sep="")
        names.expected.accuracy.original2 = paste("mean(",est$names.fitted[2],".",studynames,")",sep="")
        names.expected.accuracy.original = c(names.expected.accuracy.original1, names.expected.accuracy.original2)
        
        names.temp = names(model[["marginals.lincomb.derived"]])
        marginals.expected.accuracy.original = lapply(names.temp, function(y){
          INLA::inla.tmarginal(model$inv.linkfunc, model[["marginals.lincomb.derived"]][[y]], n=n.marginal.point)
        })
        names(marginals.expected.accuracy.original) = names.expected.accuracy.original
        
        suminfo = lapply(1:length(marginals.expected.accuracy.original), function(x) .summary.marginal(marginals.expected.accuracy.original[[x]],level=quantiles))
        summary.expected.accuracy.original = do.call(rbind, suminfo)
        rownames(summary.expected.accuracy.original) = names.expected.accuracy.original
        ####### transformed----from inla.tmarginal
        names.expected.accuracy.transform1 = paste("mean(",names.transf.fitted[1],".",studynames,")",sep="")
        names.expected.accuracy.transform2 = paste("mean(",names.transf.fitted[2],".",studynames,")",sep="")
        names.expected.accuracy.transform = c(names.expected.accuracy.transform1, names.expected.accuracy.transform2)
        
        marginals.expected.accuracy.transform = lapply(names.temp, function(y){
          INLA::inla.tmarginal(model$c.inv.linkfunc, model[["marginals.lincomb.derived"]][[y]], n=n.marginal.point)
        })
        names(marginals.expected.accuracy.transform) = names.expected.accuracy.transform
        
        suminfo = lapply(1:length(marginals.expected.accuracy.transform), function(x) .summary.marginal(marginals.expected.accuracy.transform[[x]],level=quantiles))
        summary.expected.accuracy.transform = do.call(rbind, suminfo)
        rownames(summary.expected.accuracy.transform) = names.expected.accuracy.transform
        
        est$summary.expected.accuracy = rbind(summary.expected.accuracy.original, summary.expected.accuracy.transform) 
        est$marginals.expected.accuracy = append(marginals.expected.accuracy.original, marginals.expected.accuracy.transform)
      }
    }
    

    if(outpriors$wishart.flag){
      fitted1.ind = seq(1,dim(est$data)[1],by=1)
      fitted2.ind = seq((dim(est$data)[1]+1),dim(est$outdata)[1],by=1)
    }else{
      fitted1.ind = seq(1,dim(est$outdata)[1],by=2)
      fitted2.ind = seq(2,dim(est$outdata)[1],by=2)
    }
    
    #######################################################################################
    ############# predictors: for transf accuracy, i.e. logit(se)....
    #######################################################################################
    # original---- from model
    summary.predict.temp1 = as.matrix(model[["summary.linear.predictor"]][fitted1.ind,c(1:effect.length)])
    rownames(summary.predict.temp1) = studynames
    summary.predict.temp2 = as.matrix(model[["summary.linear.predictor"]][fitted2.ind,c(1:effect.length)])
    rownames(summary.predict.temp2) = studynames
    est[[paste("summary.predictor.",model$link,"(",est$names.fitted[1],")",sep="")]] = summary.predict.temp1
    est[[paste("summary.predictor.",model$link,"(",est$names.fitted[2],")",sep="")]] = summary.predict.temp2
    
    marginals.predict.temp1 = lapply(fitted1.ind, function(y){model[["marginals.linear.predictor"]][[y]]})
    names(marginals.predict.temp1) = studynames
    marginals.predict.temp2 = lapply(fitted2.ind, function(y){model[["marginals.linear.predictor"]][[y]]})
    names(marginals.predict.temp2) = studynames
    est[[paste("marginals.predictor.",model$link,"(",est$names.fitted[1],")",sep="")]] = marginals.predict.temp1
    est[[paste("marginals.predictor.",model$link,"(",est$names.fitted[2],")",sep="")]] = marginals.predict.temp2
    
    #######################################################################################
    ############# predictors: for accuracy, i.e. se and sp if model=1
    #######################################################################################
    summary.fitted.temp1 = as.matrix(model[["summary.fitted.values"]][fitted1.ind,c(1:effect.length)])
    rownames(summary.fitted.temp1) = studynames
    summary.fitted.temp2 = as.matrix(model[["summary.fitted.values"]][fitted2.ind,c(1:effect.length)])
    rownames(summary.fitted.temp2) = studynames
    est[[paste("summary.predictor.(",est$names.fitted[1],")",sep="")]] = summary.fitted.temp1
    est[[paste("summary.predictor.(",est$names.fitted[2],")",sep="")]] = summary.fitted.temp2
    
    model.marginals.fitted.values = lapply(1:length(model[["marginals.fitted.values"]]), function(ind){
      y = model[["marginals.fitted.values"]][[ind]][,2]
      nan.index = which(is.nan(y))
      inf.index = which(is.infinite(y))
      index = c(nan.index, inf.index)
      if(length(index)!=0){
        mfv = model[["marginals.fitted.values"]][[ind]][-index,]
      }else{
        mfv = model[["marginals.fitted.values"]][[ind]]
      }
      return(mfv)
    })
    
    model.marginals.fitted.values.used = lapply(1:length(model[["marginals.fitted.values"]]), function(ind){
      x = model.marginals.fitted.values[[ind]][,1]
      zero.index = which(abs(x-0)<1e-8)
      one.index = which(abs(x-1)<1e-8)
      if(length(zero.index)!=0){zero.index = zero.index[-which(zero.index==max(zero.index))]}
      if(length(one.index)!=0){one.index = one.index[-which(one.index==min(one.index))]}
      index = c(zero.index, one.index)
      if(length(index)!=0){
        mfv = model.marginals.fitted.values[[ind]][-index,]
      }else{
        mfv = model.marginals.fitted.values[[ind]]
      }
      return(mfv)
    })
    
    marginals.fitted.temp1 = lapply(fitted1.ind, function(y){model.marginals.fitted.values.used[[y]]})
    names(marginals.fitted.temp1) = studynames
    marginals.fitted.temp2 = lapply(fitted2.ind, function(y){model.marginals.fitted.values.used[[y]]})
    names(marginals.fitted.temp2) = studynames
    est[[paste("marginals.predictor.(",est$names.fitted[1],")",sep="")]] = marginals.fitted.temp1
    est[[paste("marginals.predictor.(",est$names.fitted[2],")",sep="")]] = marginals.fitted.temp2
    
    #######################################################################################
    ############# predictors: for accuracy, i.e. 1-se and 1-sp if model=1
    #######################################################################################
    transfunc.fitted = function(x) 1-x
    
    
    marginals.transf.fitted.temp1 = lapply(marginals.fitted.temp1, function(x){INLA::inla.tmarginal(transfunc.fitted, x, n=n.marginal.point)})
    marginals.transf.fitted.temp2 = lapply(marginals.fitted.temp2, function(x){INLA::inla.tmarginal(transfunc.fitted, x, n=n.marginal.point)})
    est[[paste("marginals.predictor.","(",names.transf.fitted[1],")",sep="")]] = marginals.transf.fitted.temp1
    est[[paste("marginals.predictor.","(",names.transf.fitted[2],")",sep="")]] = marginals.transf.fitted.temp2
    
    suminfo1 = do.call(rbind,lapply(marginals.transf.fitted.temp1, function(x) .summary.marginal(x,level=quantiles)))
    suminfo2 = do.call(rbind,lapply(marginals.transf.fitted.temp2, function(x) .summary.marginal(x,level=quantiles)))
    
    est[[paste("summary.predictor.","(",names.transf.fitted[1],")",sep="")]] = suminfo1
    est[[paste("summary.predictor.","(",names.transf.fitted[2],")",sep="")]] = suminfo2
    
    # transform ----- from inla.tmarginal
    #transfunc.predict = function(x) model$linkfunc(x)
    
    #marginals.transf.predict.temp1 = lapply(marginals.transf.fitted.temp1, function(x){INLA::inla.tmarginal(transfunc.predict, x, n=n.marginal.point)})
    #marginals.transf.predict.temp2 = lapply(marginals.transf.fitted.temp2, function(x){INLA::inla.tmarginal(transfunc.predict, x, n=n.marginal.point)})
    #est[[paste("marginals.predictor.",model$link,"(",names.transf.fitted[1],")",sep="")]] = marginals.transf.predict.temp1
    #est[[paste("marginals.predictor.",model$link,"(",names.transf.fitted[2],")",sep="")]] = marginals.transf.predict.temp2
    
    #suminfo1 = do.call(rbind,lapply(marginals.transf.predict.temp1, function(x) .summary.marginal(x,level=quantiles)))
    #suminfo2 = do.call(rbind,lapply(marginals.transf.predict.temp2, function(x) .summary.marginal(x,level=quantiles)))
    
    #est[[paste("summary.predictor.",model$link,"(",names.transf.fitted[1],")",sep="")]] = suminfo1
    #est[[paste("summary.predictor.",model$link,"(",names.transf.fitted[2],")",sep="")]] = suminfo2
    #######################################################################################
    ############# samples
    #######################################################################################
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
        # message("Argument \"nsample\" should be a integer, we round the given number to interger.")
        nsample = round(nsample, digits = 0)
        est$misc$nsample = nsample
      }
    }else{
      message("Argument \"nsample\" should be TRUE, FALSE or a integer, we set it to FALSE!")
    }
    if(est$misc$sample.flag){
      ############################
      ## samples from posteriors
      ############################
      options(warn=-1)
      #set.seed(seed)
      samples = INLA::inla.posterior.sample(n = nsample, model, seed=seed)
      options(warn=0)
      predictors.samples = do.call(cbind,lapply(c(1:nsample), function(x) samples[[x]]$latent[1:dim(outdata$internaldata)[1]]))
      #predictors.samples = round(predictors.samples, 2)
      ##############################
      ## fixed and hyperpar samples
      ##############################
      fixed.samples = do.call(cbind,lapply(c(1:nsample), function(x){
        length.latent = length(samples[[x]]$latent)
        a = samples[[x]]$latent[(length.latent-dim(model$summary.fixed)[1]+1):length.latent]
        return(a)
      }))
      #fixed.samples = round(fixed.samples, 2)
      fixed.names = rownames(model$summary.fixed)
      rownames(fixed.samples) = fixed.names
      
      hyperpar.samples = do.call(cbind,lapply(c(1:nsample), function(x){
        a = samples[[x]]$hyperpar
        return(a)
      }))
      #hyperpar.samples = round(hyperpar.samples, 2)
      hyperpar.names = c("var_phi", "var_psi", "rho")
      rownames(hyperpar.samples) = hyperpar.names
      
      est$samples.fixed = fixed.samples
      est$samples.hyperpar = hyperpar.samples
      ##################
      ## if there is no covariate, the non-linear estimates are overall estimates
      ##################
      if(!covariates.flag){ #  no covariate
        ind.fitted1  = agrep("mu", fixed.names, max.distance=0)
        ind.fitted2  = agrep("nu", fixed.names, max.distance=0)
        if(!modality.flag){# and no modality
          mu.samples = fixed.samples[ind.fitted1,]
          nu.samples = fixed.samples[ind.fitted2,]
          
          overall.fitted1.samples = model$inv.linkfunc(mu.samples)
          overall.fitted2.samples = model$inv.linkfunc(nu.samples)
          
          if(model.type==1){
            overall.LRpos.samples = overall.fitted1.samples/(1-overall.fitted2.samples)
            overall.LRneg.samples = (1-overall.fitted1.samples)/overall.fitted2.samples
            overall.DOR.samples = overall.LRpos.samples/overall.LRneg.samples
            overall.RD.samples = overall.fitted1.samples - (1-overall.fitted2.samples)
            overall.LLRpos.samples = log(overall.LRpos.samples)
            overall.LLRneg.samples = log(overall.LRneg.samples)
            overall.LDOR.samples = log(overall.DOR.samples)
            
            est$samples.overall.Se = overall.fitted1.samples
            est$samples.overall.Sp = overall.fitted2.samples
          }
          if(model.type==2){
            overall.LRpos.samples = overall.fitted1.samples/overall.fitted2.samples
            overall.LRneg.samples = (1-overall.fitted1.samples)/(1-overall.fitted2.samples)
            overall.DOR.samples = overall.LRpos.samples/overall.LRneg.samples
            overall.RD.samples = overall.fitted1.samples - overall.fitted2.samples
            overall.LLRpos.samples = log(overall.LRpos.samples)
            overall.LLRneg.samples = log(overall.LRneg.samples)
            overall.LDOR.samples = log(overall.DOR.samples)
            
            est$samples.overall.Se = overall.fitted1.samples
            est$samples.overall.Sp = 1-overall.fitted2.samples
          }
          if(model.type==3){
            overall.LRpos.samples = (1-overall.fitted1.samples)/(1-overall.fitted2.samples)
            overall.LRneg.samples = overall.fitted1.samples/overall.fitted2.samples
            overall.DOR.samples = overall.LRpos.samples/overall.LRneg.samples
            overall.RD.samples = (1-overall.fitted1.samples) - (1-overall.fitted2.samples)
            overall.LLRpos.samples = log(overall.LRpos.samples)
            overall.LLRneg.samples = log(overall.LRneg.samples)
            overall.LDOR.samples = log(overall.DOR.samples)
            
            est$samples.overall.Se = 1-overall.fitted1.samples
            est$samples.overall.Sp = overall.fitted2.samples
          }
          if(model.type==4){
            overall.LRpos.samples = (1-overall.fitted1.samples)/overall.fitted2.samples
            overall.LRneg.samples = overall.fitted1.samples/(1-overall.fitted2.samples)
            overall.DOR.samples = overall.LRpos.samples/overall.LRneg.samples
            overall.RD.samples = (1-overall.fitted1.samples) - overall.fitted2.samples
            overall.LLRpos.samples = log(overall.LRpos.samples)
            overall.LLRneg.samples = log(overall.LRneg.samples)
            overall.LDOR.samples = log(overall.DOR.samples)
            
            est$samples.overall.Se = 1-overall.fitted1.samples
            est$samples.overall.Sp = 1-overall.fitted2.samples
          }
          
          summary.LRpos.temp = .summary.samples(overall.LRpos.samples, level=quantiles)
          summary.LRneg.temp = .summary.samples(overall.LRneg.samples, level=quantiles)
          summary.DOR.temp = .summary.samples(overall.DOR.samples, level=quantiles)
          summary.RD.temp = .summary.samples(overall.RD.samples, level=quantiles)
          summary.LDOR.temp = .summary.samples(overall.LDOR.samples, level=quantiles)
          summary.LLRpos.temp = .summary.samples(overall.LLRpos.samples, level=quantiles)
          summary.LLRneg.temp = .summary.samples(overall.LLRneg.samples, level=quantiles)
          
          summary.overall.statistics = rbind(summary.LRpos.temp, summary.LRneg.temp, summary.DOR.temp, summary.RD.temp, summary.LDOR.temp, summary.LLRpos.temp, summary.LLRneg.temp)
          rownames(summary.overall.statistics) = c("overall.LRpos","overall.LRneg","overall.DOR", "overall.RD", "overall.LDOR", "overall.LLRpos", "overall.LLRneg")
          est$summary.overall.statistics = summary.overall.statistics
        }else{ # no covariates, but modality
          mod.level = length(unique(outdata$originaldata[,outdata$modality.setting]))
          
          mu.samples = lapply(1:mod.level, function(i) fixed.samples[ind.fitted1[i],])
          nu.samples = lapply(1:mod.level, function(i) fixed.samples[ind.fitted2[i],])
          
          overall.fitted1.samples = lapply(1:mod.level, function(i)  model$inv.linkfunc(mu.samples[[i]]))
          overall.fitted2.samples = lapply(1:mod.level, function(i)  model$inv.linkfunc(nu.samples[[i]]))
          
          if(model.type==1){
            overall.LRpos.samples = lapply(1:mod.level, function(i) overall.fitted1.samples[[i]]/(1-overall.fitted2.samples[[i]]))
            overall.LRneg.samples = lapply(1:mod.level, function(i) (1-overall.fitted1.samples[[i]])/overall.fitted2.samples[[i]])
            overall.DOR.samples = lapply(1:mod.level, function(i) overall.LRpos.samples[[i]]/overall.LRneg.samples[[i]])
            overall.RD.samples = lapply(1:mod.level, function(i) overall.fitted1.samples[[i]] - (1-overall.fitted2.samples[[i]]))
            overall.LLRpos.samples = lapply(1:mod.level, function(i) log(overall.LRpos.samples[[i]]))
            overall.LLRneg.samples = lapply(1:mod.level, function(i) log(overall.LRneg.samples[[i]]))
            overall.LDOR.samples = lapply(1:mod.level, function(i) log(overall.DOR.samples[[i]]))
            
            est$samples.overall.Se = overall.fitted1.samples
            est$samples.overall.Sp = overall.fitted2.samples
          }
          if(model.type==2){
            overall.LRpos.samples = lapply(1:mod.level, function(i) overall.fitted1.samples[[i]]/overall.fitted2.samples[[i]])
            overall.LRneg.samples = lapply(1:mod.level, function(i) (1-overall.fitted1.samples[[i]])/(1-overall.fitted2.samples[[i]]))
            overall.DOR.samples = lapply(1:mod.level, function(i) overall.LRpos.samples[[i]]/overall.LRneg.samples[[i]])
            overall.RD.samples = lapply(1:mod.level, function(i) overall.fitted1.samples[[i]] - overall.fitted2.samples[[i]])
            overall.LLRpos.samples = lapply(1:mod.level, function(i) log(overall.LRpos.samples[[i]]))
            overall.LLRneg.samples = lapply(1:mod.level, function(i) log(overall.LRneg.samples[[i]]))
            overall.LDOR.samples = lapply(1:mod.level, function(i) log(overall.DOR.samples[[i]]))
            
            est$samples.overall.Se = overall.fitted1.samples
            est$samples.overall.Sp = lapply(1:mod.level, function(i) 1-overall.fitted2.samples[[i]])
          }
          if(model.type==3){
            overall.LRpos.samples = lapply(1:mod.level, function(i) (1-overall.fitted1.samples[[i]])/(1-overall.fitted2.samples[[i]]))
            overall.LRneg.samples = lapply(1:mod.level, function(i) overall.fitted1.samples[[i]]/overall.fitted2.samples[[i]])
            overall.DOR.samples = lapply(1:mod.level, function(i) overall.LRpos.samples[[i]]/overall.LRneg.samples[[i]])
            overall.RD.samples = lapply(1:mod.level, function(i) (1-overall.fitted1.samples[[i]]) - (1-overall.fitted2.samples[[i]]))
            overall.LLRpos.samples = lapply(1:mod.level, function(i) log(overall.LRpos.samples[[i]]))
            overall.LLRneg.samples = lapply(1:mod.level, function(i) log(overall.LRneg.samples[[i]]))
            overall.LDOR.samples = lapply(1:mod.level, function(i) log(overall.DOR.samples[[i]]))
            
            est$samples.overall.Se = lapply(1:mod.level, function(i) 1-overall.fitted1.samples[[i]])
            est$samples.overall.Sp = overall.fitted2.samples
          }
          if(model.type==4){
            overall.LRpos.samples = lapply(1:mod.level, function(i) (1-overall.fitted1.samples[[i]])/overall.fitted2.samples[[i]])
            overall.LRneg.samples = lapply(1:mod.level, function(i) overall.fitted1.samples[[i]]/(1-overall.fitted2.samples[[i]]))
            overall.DOR.samples = lapply(1:mod.level, function(i) overall.LRpos.samples[[i]]/overall.LRneg.samples[[i]])
            overall.RD.samples = lapply(1:mod.level, function(i) (1-overall.fitted1.samples[[i]]) - overall.fitted2.samples[[i]])
            overall.LLRpos.samples = lapply(1:mod.level, function(i) log(overall.LRpos.samples[[i]]))
            overall.LLRneg.samples = lapply(1:mod.level, function(i) log(overall.LRneg.samples[[i]]))
            overall.LDOR.samples = lapply(1:mod.level, function(i) log(overall.DOR.samples[[i]]))
            
            est$samples.overall.Se = lapply(1:mod.level, function(i) 1-overall.fitted1.samples[[i]])
            est$samples.overall.Sp = lapply(1:mod.level, function(i) 1-overall.fitted2.samples[[i]])
          }
          
          summary.LRpos.temp = lapply(1:mod.level, function(i) .summary.samples(overall.LRpos.samples[[i]], level=quantiles))
          summary.LRneg.temp = lapply(1:mod.level, function(i) .summary.samples(overall.LRneg.samples[[i]], level=quantiles))
          summary.DOR.temp = lapply(1:mod.level, function(i) .summary.samples(overall.DOR.samples[[i]], level=quantiles))
          summary.RD.temp = lapply(1:mod.level, function(i) .summary.samples(overall.RD.samples[[i]], level=quantiles))
          summary.LDOR.temp = lapply(1:mod.level, function(i) .summary.samples(overall.LDOR.samples[[i]], level=quantiles))
          summary.LLRpos.temp = lapply(1:mod.level, function(i) .summary.samples(overall.LLRpos.samples[[i]], level=quantiles))
          summary.LLRneg.temp = lapply(1:mod.level, function(i) .summary.samples(overall.LLRneg.samples[[i]], level=quantiles))
          
          summary.overall.statistics = lapply(1:mod.level, function(i) rbind(summary.LRpos.temp[[i]], 
                                                                                summary.LRneg.temp[[i]], 
                                                                                summary.DOR.temp[[i]], 
                                                                                summary.RD.temp[[i]], 
                                                                                summary.LDOR.temp[[i]], 
                                                                                summary.LLRpos.temp[[i]], 
                                                                                summary.LLRneg.temp[[i]]))
          for(j in 1:mod.level){
            rownames(summary.overall.statistics[[j]]) = c("overall.LRpos","overall.LRneg","overall.DOR", "overall.RD", "overall.LDOR", "overall.LLRpos", "overall.LLRneg")
          }
          names(summary.overall.statistics) = unique(outdata$originaldata[,outdata$modality.setting])
          est$summary.overall.statistics = summary.overall.statistics
        }
      }
      #############
      ############# study specific LRpos, DOR,......
      #############
      fitted1.samples = model$inv.linkfunc(predictors.samples[fitted1.ind,])
      fitted2.samples = model$inv.linkfunc(predictors.samples[fitted2.ind,])
      
      if(model.type==1){
        LRpos.samples = fitted1.samples/(1-fitted2.samples)
        LRneg.samples = (1-fitted1.samples)/fitted2.samples
        DOR.samples = LRpos.samples/LRneg.samples
        RD.samples = fitted1.samples - (1-fitted2.samples)
        LLRpos.samples = log(LRpos.samples)
        LLRneg.samples = log(LRneg.samples)
        LDOR.samples = log(DOR.samples)
        
        est$samples.study.specific.Se = fitted1.samples
        est$samples.study.specific.Sp = fitted2.samples
      }
      if(model.type==2){
        LRpos.samples = fitted1.samples/fitted2.samples
        LRneg.samples = (1-fitted1.samples)/(1-fitted2.samples)
        DOR.samples = LRpos.samples/LRneg.samples
        RD.samples = fitted1.samples - fitted2.samples
        LLRpos.samples = log(LRpos.samples)
        LLRneg.samples = log(LRneg.samples)
        LDOR.samples = log(DOR.samples)
        
        est$samples.study.specific.Se = fitted1.samples
        est$samples.study.specific.Sp = 1-fitted2.samples
      }
      if(model.type==3){
        LRpos.samples = (1-fitted1.samples)/(1-fitted2.samples)
        LRneg.samples = fitted1.samples/fitted2.samples
        DOR.samples = LRpos.samples/LRneg.samples
        RD.samples = (1-fitted1.samples) - (1-fitted2.samples)
        LLRpos.samples = log(LRpos.samples)
        LLRneg.samples = log(LRneg.samples)
        LDOR.samples = log(DOR.samples)
        
        est$samples.study.specific.Se = 1-fitted1.samples
        est$samples.study.specific.Sp = fitted2.samples
      }
      if(model.type==4){
        LRpos.samples = (1-fitted1.samples)/fitted2.samples
        LRneg.samples = fitted1.samples/(1-fitted2.samples)
        DOR.samples = LRpos.samples/LRneg.samples
        RD.samples = (1-fitted1.samples) - fitted2.samples
        LLRpos.samples = log(LRpos.samples)
        LLRneg.samples = log(LRneg.samples)
        LDOR.samples = log(DOR.samples)
        
        est$samples.study.specific.Se = 1-fitted1.samples
        est$samples.study.specific.Sp = 1-fitted2.samples
      }
      
      summary.fitted.LRpos.temp = t(apply(LRpos.samples, 1, function(x) .summary.samples(x, level=quantiles)))
      rownames(summary.fitted.LRpos.temp) = studynames
      summary.fitted.LRneg.temp = t(apply(LRneg.samples, 1, function(x) .summary.samples(x, level=quantiles)))
      rownames(summary.fitted.LRneg.temp) = studynames
      summary.fitted.DOR.temp = t(apply(DOR.samples, 1, function(x) .summary.samples(x, level=quantiles)))
      rownames(summary.fitted.DOR.temp) = studynames
      summary.fitted.RD.temp = t(apply(RD.samples, 1, function(x) .summary.samples(x, level=quantiles)))
      rownames(summary.fitted.RD.temp) = studynames
      summary.fitted.LDOR.temp = t(apply(LDOR.samples, 1, function(x) .summary.samples(x, level=quantiles)))
      rownames(summary.fitted.LDOR.temp) = studynames
      summary.fitted.LLRpos.temp = t(apply(LLRpos.samples, 1, function(x) .summary.samples(x, level=quantiles)))
      rownames(summary.fitted.LLRpos.temp) = studynames
      summary.fitted.LLRneg.temp = t(apply(LLRneg.samples, 1, function(x) .summary.samples(x, level=quantiles)))
      rownames(summary.fitted.LLRneg.temp) = studynames
      
      
      est$summary.study.specific.LRpos = summary.fitted.LRpos.temp
      est$summary.study.specific.LRneg = summary.fitted.LRneg.temp
      est$summary.study.specific.DOR = summary.fitted.DOR.temp
      est$summary.study.specific.RD = summary.fitted.RD.temp
      est$summary.study.specific.LDOR = summary.fitted.LDOR.temp
      est$summary.study.specific.LLRpos = summary.fitted.LLRpos.temp
      est$summary.study.specific.LLRneg = summary.fitted.LLRneg.temp
    }
    
    variables.names = tolower(colnames(est$data))
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
    est$misc$wishart.par = outpriors$original.setting$wishart.par
    est$misc$wishart.flag = model$wishart.flag
    
    if(is.null(outdata$covariates.setting) || outdata$covariates.setting==FALSE){
      est$misc$covariates.flag=FALSE
    } else{
      est$misc$covariates.flag=TRUE
      est$misc$covariates.name = outdata$covariates.setting
      est$misc$covariates.place.in.data = which(variables.names==outdata$covariates.setting)
    }
    if(is.null(outdata$modality.setting) || outdata$modality.setting==FALSE){
      est$misc$modality.flag=FALSE
    }else{
      est$misc$modality.flag=TRUE
      est$misc$modality.name = outdata$modality.setting
      est$misc$modality.place.in.data = which(variables.names==outdata$modality.setting)
      est$misc$modality.level = length(unique(outdata$originaldata[,outdata$modality.setting]))
    }
    est$misc$link = model$link
    est$misc$model.type = model.type
    est$misc$quantiles = model$quantiles
    est$misc$verbose = model$verbose
    est$misc$linkfunc = model$linkfunc
    est$misc$inv.linkfunc = model$inv.linkfunc
    est$misc$wishart.flag = outpriors$wishart.flag
    ################### make new class
    class(est) = 'meta4diag'
    return(est)
  }else{
    stop("INLA need to be installed and loaded!\n
Please use the following command to install and load INLA,\n
install.packages(\"INLA\", repos=\"http://www.math.ntnu.no/inla/R/testing\")
library(INLA) \n")
  }
}