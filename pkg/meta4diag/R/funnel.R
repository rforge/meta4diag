funnel <- function(x, ...) UseMethod("funnel")

funnel.meta4diag <- function(x, est.type="median",intervals=c(0.025,0.975), 
                             lwd=1,arrow.lty=1,col="black",main="Funnel Plot", xlim, ylim, ...){
  
  if(class(x)!="meta4diag"){stop("Invalid input given!")}
  
  est.type = tolower(est.type)
  if(!(est.type %in% c("mean","median"))){
    stop("Argument \"est.type\" could only be either \"mean\" or \"median\".")
  }
  if(est.type=="median"){est.type = "0.5quant"}
  
  if(!is.numeric(intervals)){
    stop("Argument \"intervals\" has to be a numerical vector with length 2.")
  }
  if(length(intervals)!=2){
    stop("Argument \"intervals\" has to be a numerical vector with length 2.")
  }
  
  ###### check intervals
  if(!all(intervals %in% x$misc$quantiles)){
    stop(paste("Argument \"intervals\" has to the values of quantiles. The options are ",paste(x$misc$quantiles,collapse=", "),sep=""))
  }
  if(intervals[1]>=intervals[2]){
    stop("The first element of argument \"intervals\" has to be smaller than the second element.")
  }
  #   if(intervals[2]<=0.5){
  #     stop("The first element of argument \"intervals\" has to be larger than 0.5.")
  #   }
  
  intervals = paste(intervals,"quant",sep="")
  
  if(x$misc$sample.flag){ 
    if(x$misc$covariates.flag){# has covariates
      # deal with estimates
      all.ss.fit = x$summary.study.specific.LDOR[,est.type]
      all.ss.sd = x$summary.study.specific.LDOR[,"sd"]
      all.ss.low = x$summary.study.specific.LDOR[,intervals[1]]
      all.ss.high = x$summary.study.specific.LDOR[,intervals[2]]
      
      if(any(is.infinite(all.ss.fit)) || any(is.na(all.ss.sd))){
        index = unique(c(which(is.infinite(all.ss.fit)), which(is.na(all.ss.sd))))
        ss.fit = x$summary.study.specific.LDOR[-index,est.type]
        ss.sd = x$summary.study.specific.LDOR[-index,"sd"]
        ss.low = x$summary.study.specific.LDOR[-index,intervals[1]]
        ss.high = x$summary.study.specific.LDOR[-index,intervals[2]]
        
        use.data = x$data[-index,]
      }else{
        ss.fit = x$summary.study.specific.LDOR[,est.type]
        ss.sd = x$summary.study.specific.LDOR[,"sd"]
        ss.low = x$summary.study.specific.LDOR[,intervals[1]]
        ss.high = x$summary.study.specific.LDOR[,intervals[2]]
        
        use.data = x$data
      }
      # if missing xlim
      if(missing(xlim)){
        xlim = c(0, max(ss.high))
      }
      # if missing ylim
      if(missing(ylim)){
        ylim = c(max(ss.sd),min(ss.sd))
      }else{
        if(ylim[1]<ylim[2]){ylim = c(ylim[2], ylim[1])}
      }
      # plot when there is covariate
      if(missing(col)){
        col = c(1:length(ss.fit))
        plot(NA,NA,xlab="log(DOR)", ylab="sd(LDOR)", xlim=xlim, ylim=ylim, main=main, ... )
        points(ss.fit, ss.sd, pch=1, col=col)
        arrows(ss.low,ss.sd,ss.high,ss.sd,col=col,angle = 90,length=0.03, code=3, lwd=lwd, lty=arrow.lty)
      }else{
        plot(NA,NA,xlab="log(DOR)", ylab="sd(LDOR)", xlim=xlim, ylim=ylim, main=main, ... )
        points(ss.fit, ss.sd, pch=1, col=col)
        arrows(ss.low,ss.sd,ss.high,ss.sd,col=col,angle = 90,length=0.03, code=3, lwd=lwd, lty=arrow.lty)
      }
      
      
    }else{ # no covariate
      if(x$misc$modality.flag){ # has modality
        nlevel = x$misc$modality.level
        overall.fit = lapply(1:nlevel, function(i){x$summary.overall.statistics[[i]]["overall.LDOR",est.type]})
        overall.sd = lapply(1:nlevel, function(i){x$summary.overall.statistics[[i]]["overall.LDOR","sd"]})
        overall.low = lapply(1:nlevel, function(i){x$summary.overall.statistics[[i]]["overall.LDOR",intervals[1]]})
        overall.high = lapply(1:nlevel, function(i){x$summary.overall.statistics[[i]]["overall.LDOR",intervals[2]]})
        temp.x = lapply(1:nlevel, function(i){c(overall.low[[i]],overall.high[[i]])-overall.fit[[i]]})
        a = lapply(1:nlevel, function(i){overall.sd[[i]]/temp.x[[i]]})
        b = lapply(1:nlevel, function(i){-a[[i]]*overall.fit[[i]]})
        
        all.ss.fit = x$summary.study.specific.LDOR[,est.type]
        all.ss.sd = x$summary.study.specific.LDOR[,"sd"]
        all.ss.low = x$summary.study.specific.LDOR[,intervals[1]]
        all.ss.high = x$summary.study.specific.LDOR[,intervals[2]]
        
        if(any(is.infinite(all.ss.fit)) || any(is.na(all.ss.sd))){
          index = unique(c(which(is.infinite(all.ss.fit)), which(is.na(all.ss.sd))))
          ss.fit = x$summary.study.specific.LDOR[-index,est.type]
          ss.sd = x$summary.study.specific.LDOR[-index,"sd"]
          ss.low = x$summary.study.specific.LDOR[-index,intervals[1]]
          ss.high = x$summary.study.specific.LDOR[-index,intervals[2]]
          
          use.data = x$data[-index,]
        }else{
          ss.fit = x$summary.study.specific.LDOR[,est.type]
          ss.sd = x$summary.study.specific.LDOR[,"sd"]
          ss.low = x$summary.study.specific.LDOR[,intervals[1]]
          ss.high = x$summary.study.specific.LDOR[,intervals[2]]
          
          use.data = x$data
        }
        
        if(missing(xlim)){
          xmax = max(abs(c(min(ss.fit),max(ss.fit))-max(unlist(overall.fit)))) + max(unlist(overall.fit))
          xlim = c(0, xmax)
        }
        
        if(missing(ylim)){
          ylim = c(max(ss.sd),min(ss.sd))
        }else{
          if(ylim[1]<ylim[2]){ylim = c(ylim[2], ylim[1])}
        }
        
        modalitynames = unique(x$data[,x$misc$modality.name])
        ind = lapply(1:nlevel, function(i) which(use.data[,x$misc$modality.name]==modalitynames[i]))
        if(missing(col)){
          col = rainbow(nlevel)
          plot(NA,NA,xlab="log(DOR)", ylab="sd(LDOR)", xlim=xlim, ylim=ylim, main=main, ... )
          lapply(1:x$misc$modality.level, function(i){
            points(ss.fit[ind[[i]]], ss.sd[ind[[i]]], pch=1, col=col[i])
            arrows(ss.low[ind[[i]]],ss.sd[ind[[i]]],ss.high[ind[[i]]],ss.sd[ind[[i]]],col=col[i],angle = 90,length=0.03, code=3, lwd=lwd, lty=arrow.lty)
            abline(v=overall.fit[[i]], lty=2,lwd=lwd,col=col[i])
            abline(a = b[[i]][1], b=a[[i]][1], lty=2,lwd=lwd,col=col[i])
            abline(a = b[[i]][2], b=a[[i]][2], lty=2,lwd=lwd,col=col[i])
          })
        }else{
          if(length(col)!=x$misc$modality.level){col = rep(col[1],nlevel)}
          plot(NA,NA,xlab="log(DOR)", ylab="sd(LDOR)", xlim=xlim, ylim=ylim, main=main, ... )
          lapply(1:x$misc$modality.level, function(i){
            points(ss.fit[ind[[i]]], ss.sd[ind[[i]]], pch=1, col=col[i])
            arrows(ss.low[ind[[i]]],ss.sd[ind[[i]]],ss.high[ind[[i]]],ss.sd[ind[[i]]],col=col[i],angle = 90,length=0.03, code=3, lwd=lwd, lty=arrow.lty)
            abline(v=overall.fit[[i]], lty=2,lwd=lwd,col=col[i])
            abline(a = b[[i]][1], b=a[[i]][1], lty=2,lwd=lwd,col=col[i])
            abline(a = b[[i]][2], b=a[[i]][2], lty=2,lwd=lwd,col=col[i])
          })
        }
        
        
      }else{# no modality
        overall.fit = x$summary.overall.statistics["overall.LDOR",est.type]
        overall.sd = x$summary.overall.statistics["overall.LDOR","sd"]
        overall.low = x$summary.overall.statistics["overall.LDOR",intervals[1]]
        overall.high = x$summary.overall.statistics["overall.LDOR",intervals[2]]
        temp.x = c(overall.low,overall.high)-overall.fit
        a = overall.sd/temp.x
        b = -a*overall.fit
        
        all.ss.fit = x$summary.study.specific.LDOR[,est.type]
        all.ss.sd = x$summary.study.specific.LDOR[,"sd"]
        all.ss.low = x$summary.study.specific.LDOR[,intervals[1]]
        all.ss.high = x$summary.study.specific.LDOR[,intervals[2]]
        
        if(any(is.infinite(all.ss.fit)) || any(is.na(all.ss.sd))){
          index = unique(c(which(is.infinite(all.ss.fit)), which(is.na(all.ss.sd))))
          ss.fit = x$summary.study.specific.LDOR[-index,est.type]
          ss.sd = x$summary.study.specific.LDOR[-index,"sd"]
          ss.low = x$summary.study.specific.LDOR[-index,intervals[1]]
          ss.high = x$summary.study.specific.LDOR[-index,intervals[2]]
          
          use.data = x$data[-index,]
        }else{
          ss.fit = x$summary.study.specific.LDOR[,est.type]
          ss.sd = x$summary.study.specific.LDOR[,"sd"]
          ss.low = x$summary.study.specific.LDOR[,intervals[1]]
          ss.high = x$summary.study.specific.LDOR[,intervals[2]]
          
          use.data = x$data
        }
        
        if(missing(xlim)){
          xmax = max(abs(c(min(ss.fit),max(ss.fit))-overall.fit)) + overall.fit
          xlim = c(0, xmax)
        }
        
        if(missing(ylim)){
          ylim = c(max(ss.sd),min(ss.sd))
        }else{
          if(ylim[1]<ylim[2]){ylim = c(ylim[2], ylim[1])}
        }
        
        if(missing(col)){
          plot(NA,NA,xlab="log(DOR)", ylab="sd(LDOR)", xlim=xlim, ylim=ylim, main=main, ... )
          points(ss.fit, ss.sd, pch=1)
          abline(v=overall.fit, lty=2,lwd=lwd)
          abline(a = b[1], b=a[1], lty=2,lwd=lwd)
          abline(a = b[2], b=a[2], lty=2,lwd=lwd)
          arrows(ss.low,ss.sd,ss.high,ss.sd,angle = 90,length=0.03, code=3, lwd=lwd, lty=arrow.lty)
        }else{
          plot(NA,NA,xlab="log(DOR)", ylab="sd(LDOR)", xlim=xlim, ylim=ylim, main=main, ... )
          points(ss.fit, ss.sd, pch=1, col=col)
          abline(v=overall.fit, lty=2,lwd=lwd)
          abline(a = b[1], b=a[1], lty=2,lwd=lwd)
          abline(a = b[2], b=a[2], lty=2,lwd=lwd)
          arrows(ss.low,ss.sd,ss.high,ss.sd,angle = 90,length=0.03, code=3, lwd=lwd, lty=arrow.lty, col=col)
        }
      }# end of no modality
    }# end of no covariate
    return(invisible())
  }else{
    stop("Funnel plot needs the samples from posteriors. Please let \"nsample=TRUE\" in the \"meta4diag()\" function.")
  }
}