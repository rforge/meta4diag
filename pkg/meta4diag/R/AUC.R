AUC <- function(x, ...) UseMethod("AUC")

AUC.meta4diag <- function(x, sroc.type=1, est.type="median", ...){
  if(class(x)!="meta4diag"){stop("Invalid input given!")}
  
  est.type = tolower(est.type)
  if(est.type=="median"){est.type = "0.5quant"}
  
  # link function
  link = x$misc$link
  g = x$misc$linkfunc
  inv.g = x$misc$inv.linkfunc
  
  quantiles = x$misc$quantiles
  
  modalitylevelnames = unique(x$data[,x$misc$modality.name])
  
  
  g.xx = seq(-10,10,by=0.01)
  invg.xx = inv.g(g.xx)
  
  fitname = x$names.fitted
  
  # AUC for the estimates line
  if(x$misc$covariates.flag){# has covariates
    fit1 = x[[paste("summary.predictor.",link,"(", fitname[1],")",sep="")]][,est.type]
    fit2 = x[[paste("summary.predictor.",link,"(", fitname[2],")",sep="")]][,est.type]
    if(x$misc$model.type==1){
      Si = fit1 + fit2
      Di = fit1 - fit2
    }
    if(x$misc$model.type==2){
      Si = fit1 - fit2
      Di = fit1 + fit2
    }
    if(x$misc$model.type==3){
      Si = - fit1 + fit2
      Di = - fit1 - fit2
    }
    if(x$misc$model.type==4){
      Si = - fit1 - fit2
      Di = - fit1 + fit2
    }
    lr = lm(Si~Di)
    a = lr$coefficients[1]
    b = lr$coefficients[2]
    if(x$misc$model.type==1){
      g.yy = a/(1-b)-(1+b)/(1-b)*g.xx
    }
    if(x$misc$model.type==2){
      g.yy = a/(1-b)+(1+b)/(1-b)*g.xx
    }
    if(x$misc$model.type==3){
      g.yy = -a/(1-b)+(1+b)/(1-b)*g.xx
    }
    if(x$misc$model.type==4){
      g.yy = -a/(1-b)-(1+b)/(1-b)*g.xx
    }
    
    invg.yy = inv.g(g.yy)
    nan.index = which(is.nan(invg.yy))
    if(length(nan.index)!=0){
      invg.xx = invg.xx[-nan.index]
      invg.yy = invg.yy[-nan.index]
    }
    AUC_est = trapz(invg.xx, invg.yy)
  }else{# no covariates
    if(x$misc$modality.flag){
      mod.level = x$misc$modality.level
      
      mu = unlist(lapply(1:mod.level, function(ind) x[[paste("summary.expected.",link,".accuracy",sep="")]][ind,est.type]))
      nu = unlist(lapply(1:mod.level, function(ind) x[[paste("summary.expected.",link,".accuracy",sep="")]][(ind+mod.level),est.type]))
      var1 = rep(x[["summary.hyperpar"]][1,est.type], mod.level)
      var2 = rep(x[["summary.hyperpar"]][2,est.type], mod.level)
      rho = rep(x[["summary.hyperpar"]][3,est.type], mod.level)
      
      # SROC line
      if(sroc.type==1){
        g.yy = lapply(1:mod.level, function(ind) mu[ind] + rho[ind]*sqrt(var1[ind]/var2[ind])*(g.xx-nu[ind]))
      }else if(sroc.type==2){
        g.yy = lapply(1:mod.level, function(ind){
          if(rho[ind]<0){
            if(x$misc$model.type==2 || x$misc$model.type==3){
              g.yy = (var1[ind]-var2[ind]-sqrt((var2[ind]-var1[ind])^2+
                                                 4*rho[ind]^2*var1[ind]*var2[ind]))/(2*rho[ind]*sqrt(var1[ind]*var2[ind]))*(g.xx-nu[ind])+mu[ind]
            }else{
              g.yy = (var1[ind]-var2[ind]+sqrt((var2[ind]-var1[ind])^2+
                                                 4*rho[ind]^2*var1[ind]*var2[ind]))/(2*rho[ind]*sqrt(var1[ind]*var2[ind]))*(g.xx-nu[ind])+mu[ind]
            }
          }else{
            if(x$misc$model.type==1 || x$misc$model.type==4){
              g.yy = (var1[ind]-var2[ind]-sqrt((var2[ind]-var1[ind])^2+4*rho[ind]^2*var1[ind]*var2[ind]))/(2*rho[ind]*sqrt(var1[ind]*var2[ind]))*(g.xx-nu[ind])+mu[ind]
            }else{
              g.yy = (var1[ind]-var2[ind]+sqrt((var2[ind]-var1[ind])^2+4*rho[ind]^2*var1[ind]*var2[ind]))/(2*rho[ind]*sqrt(var1[ind]*var2[ind]))*(g.xx-nu[ind])+mu[ind]
            }
          }
          return(g.yy)
        })
        
      }else if(sroc.type==3){
        if(x$misc$model.type==2 || x$misc$model.type==3){
          g.yy = lapply(1:mod.level, function(ind) (var1[ind] + rho[ind]*sqrt(var1[ind]*var2[ind]))/(var2[ind] + rho[ind]*sqrt(var1[ind]*var2[ind]))*(g.xx-nu[ind])+mu[ind])
        }else{
          g.yy = lapply(1:mod.level, function(ind) -(var1[ind] - rho[ind]*sqrt(var1[ind]*var2[ind]))/(var2[ind] - rho[ind]*sqrt(var1[ind]*var2[ind]))*(g.xx-nu[ind])+mu[ind])
        }
        
      }else if(sroc.type==4){
        g.yy = lapply(1:mod.level, function(ind) mu[ind] + 1/rho[ind]*sqrt(var1[ind]/var2[ind])*(g.xx-nu[ind]))
      }else if(sroc.type==5){
        if(x$misc$model.type==2 || x$misc$model.type==3){
          g.yy = lapply(1:mod.level, function(ind) mu[ind] + sqrt(var1[ind]/var2[ind])*(g.xx-nu[ind]))
        }else{
          g.yy = lapply(1:mod.level, function(ind) mu[ind] - sqrt(var1[ind]/var2[ind])*(g.xx-nu[ind]))
        }
        
      }else{stop("Please give the correct sroc type, which is 1, 2, 3, 4 or 5.")}
      
      invg.yy = lapply(1:mod.level, function(ind) inv.g(g.yy[[ind]]))

      nan.index = lapply(1:mod.level, function(ind) which(is.nan(invg.yy[[ind]])))
      invg.xx = lapply(1:mod.level, function(ind){
        if(length(nan.index[[ind]])!=0){
          invg.xx = invg.xx[-nan.index]
        }else{
          invg.xx = invg.xx
        }
        return(invg.xx)
      })
      invg.yy = lapply(1:mod.level, function(ind){
        if(length(nan.index[[ind]])!=0){
          invg.yy = invg.yy[[ind]][-nan.index[[ind]]]
        }else{
          invg.yy = invg.yy[[ind]]
        }
        return(invg.yy)
      }) 
      
      AUC_est = unlist(lapply(1:mod.level, function(ind) trapz(invg.xx[[ind]], invg.yy[[ind]])))
      names(AUC_est) = modalitylevelnames
      
    }else{ ### no covariates, no modality
      mu = x[[paste("summary.expected.",link,".accuracy",sep="")]][1,est.type]
      nu = x[[paste("summary.expected.",link,".accuracy",sep="")]][2,est.type]
      var1 = x[["summary.hyperpar"]][1,est.type]
      var2 = x[["summary.hyperpar"]][2,est.type]
      rho = x[["summary.hyperpar"]][3,est.type]
      
      #### SROC Line
      if(sroc.type==1){
        g.yy = mu + rho*sqrt(var1/var2)*(g.xx-nu)
      }else if(sroc.type==2){
        if(rho<0){
          if(x$misc$model.type==2 || x$misc$model.type==3){
            g.yy = (var1-var2-sqrt((var2-var1)^2+4*rho^2*var1*var2))/(2*rho*sqrt(var1*var2))*(g.xx-nu)+mu
          }else{
            g.yy = (var1-var2+sqrt((var2-var1)^2+4*rho^2*var1*var2))/(2*rho*sqrt(var1*var2))*(g.xx-nu)+mu
          }
        }else{
          if(x$misc$model.type==1 || x$misc$model.type==4){
            g.yy = (var1-var2-sqrt((var2-var1)^2+4*rho^2*var1*var2))/(2*rho*sqrt(var1*var2))*(g.xx-nu)+mu
          }else{
            g.yy = (var1-var2+sqrt((var2-var1)^2+4*rho^2*var1*var2))/(2*rho*sqrt(var1*var2))*(g.xx-nu)+mu
          }
        }
      }else if(sroc.type==3){
        if(x$misc$model.type==2 || x$misc$model.type==3){
          g.yy = (var1 + rho*sqrt(var1*var2))/(var2 + rho*sqrt(var1*var2))*(g.xx-nu)+mu
        }else{
          g.yy = -(var1 - rho*sqrt(var1*var2))/(var2 - rho*sqrt(var1*var2))*(g.xx-nu)+mu
        }  
      }else if(sroc.type==4){
        g.yy = mu + 1/rho*sqrt(var1/var2)*(g.xx-nu)
      }else if(sroc.type==5){
        if(x$misc$model.type==2 || x$misc$model.type==3){
          g.yy = mu + sqrt(var1/var2)*(g.xx-nu)
        }else{
          g.yy = mu - sqrt(var1/var2)*(g.xx-nu)
        }
      }else{stop("Please give the correct sroc type, which is 1, 2, 3, 4 or 5.")}
      invg.yy = inv.g(g.yy)
      
      nan.index = which(is.nan(invg.yy))
      if(length(nan.index)!=0){
        invg.xx = invg.xx[-nan.index]
        invg.yy = invg.yy[-nan.index]
      }
      AUC_est = trapz(invg.xx, invg.yy)
    }
  }

  ########################
  ## Samples
  ########################
  if(x$misc$sample.flag){ # has samples ----- uncertainty will be estimates for AUC
    if(x$misc$covariates.flag){ # has covaraites
      AUC_samples = lapply(1:x$misc$nsample, function(i){
        fit1 = g(x$samples.study.specific.Se[,i])
        fit2 = g(x$samples.study.specific.Sp[,i])
        if(x$misc$model.type==1){
          Si = fit1 + fit2
          Di = fit1 - fit2
        }
        if(x$misc$model.type==2){
          Si = fit1 - fit2
          Di = fit1 + fit2
        }
        if(x$misc$model.type==3){
          Si = - fit1 + fit2
          Di = - fit1 - fit2
        }
        if(x$misc$model.type==4){
          Si = - fit1 - fit2
          Di = - fit1 + fit2
        }
        lr = lm(Si~Di)
        a = lr$coefficients[1]
        b = lr$coefficients[2]
        if(x$misc$model.type==1){
          g.yy = a/(1-b)-(1+b)/(1-b)*g.xx
        }
        if(x$misc$model.type==2){
          g.yy = a/(1-b)+(1+b)/(1-b)*g.xx
        }
        if(x$misc$model.type==3){
          g.yy = -a/(1-b)+(1+b)/(1-b)*g.xx
        }
        if(x$misc$model.type==4){
          g.yy = -a/(1-b)-(1+b)/(1-b)*g.xx
        }
        invg.xx = inv.g(g.xx)
        invg.yy = inv.g(g.yy)
        
        nan.index = which(is.nan(invg.yy))
        if(length(nan.index)!=0){
          int.xx = invg.xx[-nan.index]
          int.yy = invg.yy[-nan.index]
        }else{
          int.xx = invg.xx
          int.yy = invg.yy
        }
        
        AUC_est = trapz(int.xx, int.yy)
        return(AUC_est)
      })
      AUC_samples = unlist(AUC_samples)
      AUC_summary = .summary.samples(AUC_samples, level=quantiles)
    }else{# no covariates
      if(x$misc$modality.flag){ # has modality
        mod.level = x$misc$modality.level
        AUC_samples = lapply(1:x$misc$nsample, function(i){
          mu = unlist(lapply(1:mod.level, function(ind) x$samples.fixed[ind,i]))
          nu = unlist(lapply(1:mod.level, function(ind) x$samples.fixed[(ind+mod.level),i]))
          var1 = rep(x$samples.hyperpar[1,i], mod.level)
          var2 = rep(x$samples.hyperpar[2,i], mod.level)
          rho = rep(x$samples.hyperpar[3,i], mod.level)
          # SROC line
          if(sroc.type==1){
            g.yy = lapply(1:mod.level, function(ind) mu[ind] + rho[ind]*sqrt(var1[ind]/var2[ind])*(g.xx-nu[ind]))
          }else if(sroc.type==2){
            g.yy = lapply(1:mod.level, function(ind){
              if(rho[ind]<0){
                if(x$misc$model.type==2 || x$misc$model.type==3){
                  g.yy = (var1[ind]-var2[ind]-sqrt((var2[ind]-var1[ind])^2+
                                                     4*rho[ind]^2*var1[ind]*var2[ind]))/(2*rho[ind]*sqrt(var1[ind]*var2[ind]))*(g.xx-nu[ind])+mu[ind]
                }else{
                  g.yy = (var1[ind]-var2[ind]+sqrt((var2[ind]-var1[ind])^2+
                                                     4*rho[ind]^2*var1[ind]*var2[ind]))/(2*rho[ind]*sqrt(var1[ind]*var2[ind]))*(g.xx-nu[ind])+mu[ind]
                }
              }else{
                if(x$misc$model.type==1 || x$misc$model.type==4){
                  g.yy = (var1[ind]-var2[ind]-sqrt((var2[ind]-var1[ind])^2+4*rho[ind]^2*var1[ind]*var2[ind]))/(2*rho[ind]*sqrt(var1[ind]*var2[ind]))*(g.xx-nu[ind])+mu[ind]
                }else{
                  g.yy = (var1[ind]-var2[ind]+sqrt((var2[ind]-var1[ind])^2+4*rho[ind]^2*var1[ind]*var2[ind]))/(2*rho[ind]*sqrt(var1[ind]*var2[ind]))*(g.xx-nu[ind])+mu[ind]
                }
              }
              return(g.yy)
            })
            
          }else if(sroc.type==3){
            if(x$misc$model.type==2 || x$misc$model.type==3){
              g.yy = lapply(1:mod.level, function(ind) (var1[ind] + rho[ind]*sqrt(var1[ind]*var2[ind]))/(var2[ind] + rho[ind]*sqrt(var1[ind]*var2[ind]))*(g.xx-nu[ind])+mu[ind])
            }else{
              g.yy = lapply(1:mod.level, function(ind) -(var1[ind] - rho[ind]*sqrt(var1[ind]*var2[ind]))/(var2[ind] - rho[ind]*sqrt(var1[ind]*var2[ind]))*(g.xx-nu[ind])+mu[ind])
            }
            
          }else if(sroc.type==4){
            g.yy = lapply(1:mod.level, function(ind) mu[ind] + 1/rho[ind]*sqrt(var1[ind]/var2[ind])*(g.xx-nu[ind]))
          }else if(sroc.type==5){
            if(x$misc$model.type==2 || x$misc$model.type==3){
              g.yy = lapply(1:mod.level, function(ind) mu[ind] + sqrt(var1[ind]/var2[ind])*(g.xx-nu[ind]))
            }else{
              g.yy = lapply(1:mod.level, function(ind) mu[ind] - sqrt(var1[ind]/var2[ind])*(g.xx-nu[ind]))
            }
          }else{stop("Please give the correct sroc type, which is 1, 2, 3, 4 or 5.")}
          invg.xx = inv.g(g.xx)
          invg.yy = lapply(1:mod.level, function(ind) inv.g(g.yy[[ind]]))
          
          nan.index = lapply(1:mod.level, function(ind) which(is.nan(invg.yy[[ind]])))
          int.xx = lapply(1:mod.level, function(ind){
            if(length(nan.index[[ind]])!=0){
              int.xx = invg.xx[-nan.index[[ind]]]
            }else{
              int.xx = invg.xx
            }
            return(int.xx)
          }) 
          int.yy = lapply(1:mod.level, function(ind){
            if(length(nan.index[[ind]])!=0){
              int.yy = invg.yy[[ind]][-nan.index[[ind]]]
            }else{
              int.yy = invg.yy[[ind]]
            }
            return(int.yy)
          })
          
          AUC_est = unlist(lapply(1:mod.level, function(ind) trapz(int.xx[[ind]], int.yy[[ind]])))
          names(AUC_est) = modalitylevelnames
          return(AUC_est)
        })
        AUC_samples = do.call(rbind, AUC_samples)
        AUC_summary = lapply(1:mod.level, function(ind) .summary.samples(AUC_samples[,ind], level=quantiles))
        names(AUC_summary) = modalitylevelnames
        
      }else{ ### no covariates, no modality
        AUC_samples = lapply(1:x$misc$nsample, function(i){
          mu = x$samples.fixed[1,i]
          nu = x$samples.fixed[2,i]
          var1 = x$samples.hyperpar[1,i]
          var2 = x$samples.hyperpar[2,i]
          rho = x$samples.hyperpar[3,i]
          #### SROC Line
          if(sroc.type==1){
            g.yy = mu + rho*sqrt(var1/var2)*(g.xx-nu)
          }else if(sroc.type==2){
            if(rho<0){
              if(x$misc$model.type==2 || x$misc$model.type==3){
                g.yy = (var1-var2-sqrt((var2-var1)^2+4*rho^2*var1*var2))/(2*rho*sqrt(var1*var2))*(g.xx-nu)+mu
              }else{
                g.yy = (var1-var2+sqrt((var2-var1)^2+4*rho^2*var1*var2))/(2*rho*sqrt(var1*var2))*(g.xx-nu)+mu
              }
            }else{
              if(x$misc$model.type==1 || x$misc$model.type==4){
                g.yy = (var1-var2-sqrt((var2-var1)^2+4*rho^2*var1*var2))/(2*rho*sqrt(var1*var2))*(g.xx-nu)+mu
              }else{
                g.yy = (var1-var2+sqrt((var2-var1)^2+4*rho^2*var1*var2))/(2*rho*sqrt(var1*var2))*(g.xx-nu)+mu
              }
            }
          }else if(sroc.type==3){
            if(x$misc$model.type==2 || x$misc$model.type==3){
              g.yy = (var1 + rho*sqrt(var1*var2))/(var2 + rho*sqrt(var1*var2))*(g.xx-nu)+mu
            }else{
              g.yy = -(var1 - rho*sqrt(var1*var2))/(var2 - rho*sqrt(var1*var2))*(g.xx-nu)+mu
            }  
          }else if(sroc.type==4){
            g.yy = mu + 1/rho*sqrt(var1/var2)*(g.xx-nu)
          }else if(sroc.type==5){
            if(x$misc$model.type==2 || x$misc$model.type==3){
              g.yy = mu + sqrt(var1/var2)*(g.xx-nu)
            }else{
              g.yy = mu - sqrt(var1/var2)*(g.xx-nu)
            }
          }else{stop("Please give the correct sroc type, which is 1, 2, 3, 4 or 5.")}
          invg.xx = inv.g(g.xx)
          invg.yy = inv.g(g.yy)
          
          nan.index = which(is.nan(invg.yy))
          if(length(nan.index)!=0){
            int.xx = invg.xx[-nan.index]
            int.yy = invg.yy[-nan.index]
          }else{
            int.xx = invg.xx
            int.yy = invg.yy
          }
          
          AUC_est = trapz(int.xx, int.yy)
          return(AUC_est)
        })
        AUC_samples = unlist(AUC_samples)
        AUC_summary = .summary.samples(AUC_samples, level=quantiles)
      }
    }
  }# end of samples
  if(x$misc$sample.flag){
    if(x$misc$covariates.flag){
      AUC = c(AUC_est,AUC_summary)
      names(AUC) = c("est", names(AUC_summary))
    }else{
      if(x$misc$modality.flag){
        AUC = lapply(1:mod.level, function(ind){
          a = c(AUC_est[ind],AUC_summary[[ind]])
          names(a) = c("est", names(AUC_summary[[ind]]))
          return(a)
        })
        AUC = do.call(rbind, AUC)
        rownames(AUC) = modalitylevelnames
      }else{
        AUC = c(AUC_est,AUC_summary)
        names(AUC) = c("est", names(AUC_summary))
      }
    }
    return(AUC)
  }else{
    return(AUC = AUC_est)
  }
  
}