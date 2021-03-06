SROC <- function(x, ...) UseMethod("SROC")

SROC.meta4diag = function(x, 
                          sroc.type=1,
                          est.type="mean", 
                          sp.cex=1.5,
                          sp.pch=8,
                          sp.col="red",
                          dataShow="o", 
                          data.col="#FF0000", 
                          data.cex="scaled", 
                          data.pch=1, 
                          lineShow=T, 
                          line.lty=1, 
                          line.lwd=2, 
                          line.col="black",
                          crShow=T, 
                          cr.lty=2, 
                          cr.lwd=1.5, 
                          cr.col="blue",
                          prShow=T, 
                          pr.lty=3, 
                          pr.lwd=1, 
                          pr.col="darkgray",
                          dataFit = T, 
                          add=FALSE, 
                          main="SROC Plot", 
                          xlim,
                          ylim,
                          legend=F,
                          legend.cex = 0.7,
                          ...){
  if(class(x)!="meta4diag"){stop("Invalid input given!")}
  if(is.character(data.cex)){
    data.cex = tolower(data.cex)
    if(!(data.cex %in% c("scaled", "bubble"))){
      stop("data.cex could be scaled, bubble or fixed to a nuerical value!")
    }
  }
  
  if(!is.logical(legend)){
    if(!is.character(legend)){
      stop("Invalid input for legend.")
    }else{
      if(!(tolower(legend) %in% c("bottom", "right","left"))){
        stop("legend could only be at bottom, left or right")
      }else{
        legend.flag = TRUE
        legend.pos = switch(legend,"bottom"="top", "right"="left", "left"="right")
      }
    }
  }else{
    if(legend){
      legend.flag = TRUE
      legend = "bottom"
      legend.pos = "top"
    }else{
      legend.flag = FALSE
    }
  }
  

  est.type = tolower(est.type)
  if(!(est.type %in% c("mean","median"))){
    stop("Argument \"est.type\" could only be either \"mean\" or \"median\".")
  }
  if(est.type=="median"){est.type = "0.5quant"}
  N = x$data$fp + x$data$tn + x$data$fn + x$data$tp
  
  if(x$misc$model.type==1){
      data.xx = x$data$tn/(x$data$fp + x$data$tn)
      data.yy = x$data$tp/(x$data$fn + x$data$tp)
  }
  if(x$misc$model.type==2){
      data.xx = x$data$fp/(x$data$fp + x$data$tn)
      data.yy = x$data$tp/(x$data$fn + x$data$tp)
  }
  if(x$misc$model.type==3){
      data.xx = x$data$tn/(x$data$fp + x$data$tn)
      data.yy = x$data$fn/(x$data$fn + x$data$tp)
  }
  if(x$misc$model.type==4){
      data.xx = x$data$fp/(x$data$fp + x$data$tn)
      data.yy = x$data$fn/(x$data$fn + x$data$tp)
  }

  fitname = x$names.fitted
  if(dataShow=="f"){
    data.yy = x[[paste("summary.predictor.(", fitname[1],")",sep="")]][,est.type]
    data.xx = x[[paste("summary.predictor.(", fitname[2],")",sep="")]][,est.type]
  }
  
  if(dataFit){
    min.data.xx = min(data.xx)
    max.data.xx = max(data.xx)
    min.data.yy = min(data.yy)
    max.data.yy = max(data.yy)
  }
  
  
  I = dim(x$data)[1]
  f = qf(0.95, 2, I-2)
  c = sqrt(2*f)
  t = seq(0, 2*pi, by = 2*pi/100)
  
  g.xx = seq(-10,10,by=0.01)
  
  link = x$misc$link
  
  fit1 = x[[paste("summary.predictor.",link,"(", fitname[1],")",sep="")]][,est.type]
  fit2 = x[[paste("summary.predictor.",link,"(", fitname[2],")",sep="")]][,est.type]
  if(x$misc$covariates.flag){  
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
    invg.xx = x$misc$inv.linkfunc(g.xx)
    invg.yy = x$misc$inv.linkfunc(g.yy)
  }else{
    if(x$misc$modality.flag){
      mod.level = x$misc$modality.level
      if(length(cr.lty)!=mod.level){cr.lty = rep(cr.lty[1],mod.level)}
      if(length(cr.lwd)!=mod.level){cr.lwd = rep(cr.lwd[1],mod.level)}
      if(length(cr.col)!=mod.level){cr.col = rep(cr.col[1],mod.level)}
      if(length(pr.lty)!=mod.level){pr.lty = rep(pr.lty[1],mod.level)}
      if(length(pr.lwd)!=mod.level){pr.lwd = rep(pr.lwd[1],mod.level)}
      if(length(pr.col)!=mod.level){pr.col = rep(pr.col[1],mod.level)}
      if(length(sp.cex)!=mod.level){sp.cex = rep(sp.cex[1],mod.level)}
      if(length(sp.pch)!=mod.level){sp.pch = rep(sp.pch[1],mod.level)}
      if(length(sp.col)!=mod.level){sp.col = rep(sp.col[1],mod.level)}
      if(length(line.lty)!=mod.level){line.lty = rep(line.lty[1],mod.level)}
      if(length(line.lwd)!=mod.level){line.lwd = rep(line.lwd[1],mod.level)}
      if(length(line.col)!=mod.level){line.col = rep(line.col[1],mod.level)}
      
      
      mean.A = unlist(lapply(1:mod.level, function(ind) x[[paste("summary.expected.",link,".accuracy",sep="")]][ind,est.type]))
      mean.B = unlist(lapply(1:mod.level, function(ind) x[[paste("summary.expected.",link,".accuracy",sep="")]][(ind+mod.level),est.type]))
      var1 = rep(x[["summary.hyperpar"]][1,est.type], mod.level)
      var2 = rep(x[["summary.hyperpar"]][2,est.type], mod.level)
      rho = rep(x[["summary.hyperpar"]][3,est.type], mod.level)
      
      sd.A = unlist(lapply(1:mod.level, function(ind) x[[paste("summary.expected.",link,".accuracy",sep="")]][ind,2]))
      sd.B = unlist(lapply(1:mod.level, function(ind) x[[paste("summary.expected.",link,".accuracy",sep="")]][(ind+mod.level),2]))
      
      # confidence
      r = x[[paste("correlation.expected.",link,".accuracy",sep="")]]
      t.conf.A = lapply(1:mod.level, function(ind) mean.A[ind] + sd.A[ind]*c*cos(t))
      t.conf.B = lapply(1:mod.level, function(ind) mean.B[ind] + sd.B[ind]*c*cos(t + acos(r[ind])))
      
      confidence.A = lapply(1:mod.level, function(ind) x$misc$inv.linkfunc(t.conf.A[[ind]]))
      confidence.B = lapply(1:mod.level, function(ind) x$misc$inv.linkfunc(t.conf.B[[ind]]))
      
      # predict
      covariance.predict = unlist(lapply(1:mod.level, function(ind) rho[ind]*sqrt(var1[ind]*var2[ind]) + r[ind]*sd.A[ind]*sd.B[ind]))
        
      sd.pA = unlist(lapply(1:mod.level, function(ind) sqrt(var1[ind] + sd.A[ind]^2)))
      sd.pB = unlist(lapply(1:mod.level, function(ind) sqrt(var2[ind] + sd.B[ind]^2)))
      rho.predict = unlist(lapply(1:mod.level, function(ind) covariance.predict[ind]/(sd.pA[ind]*sd.pB[ind])))
      t.pred.A = lapply(1:mod.level, function(ind) mean.A[ind] + sd.pA[ind]*c*cos(t))
      t.pred.B = lapply(1:mod.level, function(ind) mean.B[ind] + sd.pB[ind]*c*cos(t + acos(rho.predict[ind])))
      
      predict.A = lapply(1:mod.level, function(ind) x$misc$inv.linkfunc(t.pred.A[[ind]]))
      predict.B = lapply(1:mod.level, function(ind) x$misc$inv.linkfunc(t.pred.B[[ind]]))
      
      # SROC line
      if(sroc.type==1){
        g.yy = lapply(1:mod.level, function(ind) mean.A[ind] + rho[ind]*sqrt(var1[ind]/var2[ind])*(g.xx-mean.B[ind]))
      }else if(sroc.type==2){
        g.yy = lapply(1:mod.level, function(ind){
          if(rho[ind]<0){
            if(x$misc$model.type==2 || x$misc$model.type==3){
              g.yy = (var1[ind]-var2[ind]-sqrt((var2[ind]-var1[ind])^2+
                               4*rho[ind]^2*var1[ind]*var2[ind]))/(2*rho[ind]*sqrt(var1[ind]*var2[ind]))*(g.xx-mean.B[ind])+mean.A[ind]
            }else{
              g.yy = (var1[ind]-var2[ind]+sqrt((var2[ind]-var1[ind])^2+
                               4*rho[ind]^2*var1[ind]*var2[ind]))/(2*rho[ind]*sqrt(var1[ind]*var2[ind]))*(g.xx-mean.B[ind])+mean.A[ind]
            }
          }else{
            if(x$misc$model.type==1 || x$misc$model.type==4){
              g.yy = (var1[ind]-var2[ind]-sqrt((var2[ind]-var1[ind])^2+4*rho[ind]^2*var1[ind]*var2[ind]))/(2*rho[ind]*sqrt(var1[ind]*var2[ind]))*(g.xx-mean.B[ind])+mean.A[ind]
            }else{
              g.yy = (var1[ind]-var2[ind]+sqrt((var2[ind]-var1[ind])^2+4*rho[ind]^2*var1[ind]*var2[ind]))/(2*rho[ind]*sqrt(var1[ind]*var2[ind]))*(g.xx-mean.B[ind])+mean.A[ind]
            }
          }
          return(g.yy)
        })
        
      }else if(sroc.type==3){
        if(x$misc$model.type==2 || x$misc$model.type==3){
          g.yy = lapply(1:mod.level, function(ind) (var1[ind] + rho[ind]*sqrt(var1[ind]*var2[ind]))/(var2[ind] + rho[ind]*sqrt(var1[ind]*var2[ind]))*(g.xx-mean.B[ind])+mean.A[ind])
        }else{
          g.yy = lapply(1:mod.level, function(ind) -(var1[ind] - rho[ind]*sqrt(var1[ind]*var2[ind]))/(var2[ind] - rho[ind]*sqrt(var1[ind]*var2[ind]))*(g.xx-mean.B[ind])+mean.A[ind])
        }
        
      }else if(sroc.type==4){
        g.yy = lapply(1:mod.level, function(ind) mean.A[ind] + 1/rho[ind]*sqrt(var1[ind]/var2[ind])*(g.xx-mean.B[ind]))
      }else if(sroc.type==5){
        if(x$misc$model.type==2 || x$misc$model.type==3){
          g.yy = lapply(1:mod.level, function(ind) mean.A[ind] + sqrt(var1[ind]/var2[ind])*(g.xx-mean.B[ind]))
        }else{
          g.yy = lapply(1:mod.level, function(ind) mean.A[ind] - sqrt(var1[ind]/var2[ind])*(g.xx-mean.B[ind]))
        }
        
      }else{stop("Please give the correct sroc type, which is 1, 2, 3, 4 or 5.")}
      invg.xx = x$misc$inv.linkfunc(g.xx)
      invg.yy = lapply(1:mod.level, function(ind) x$misc$inv.linkfunc(g.yy[[ind]]))
      
    }else{ ### no covariates, no modality
      mean.A = x[[paste("summary.expected.",link,".accuracy",sep="")]][1,est.type]
      mean.B = x[[paste("summary.expected.",link,".accuracy",sep="")]][2,est.type]
      var1 = x[["summary.hyperpar"]][1,est.type]
      var2 = x[["summary.hyperpar"]][2,est.type]
      rho = x[["summary.hyperpar"]][3,est.type]
      
      sd.A = x[[paste("summary.expected.",link,".accuracy",sep="")]][1,2]
      sd.B = x[[paste("summary.expected.",link,".accuracy",sep="")]][2,2]
      
      # confidence
      r = x[[paste("correlation.expected.",link,".accuracy",sep="")]]
      mu.A = mean.A + sd.A*c*cos(t)
      mu.B = mean.B + sd.B*c*cos(t + acos(r))
      
      confidence.A = x$misc$inv.linkfunc(mu.A)
      confidence.B = x$misc$inv.linkfunc(mu.B)
      
      # predict
      covariance.predict = rho*sqrt(var1*var2) + r*sd.A*sd.B
      sd.pA = sqrt(var1 + sd.A^2)
      sd.pB = sqrt(var2 + sd.B^2)
      rho.predict = covariance.predict/(sd.pA*sd.pB)
      mu.A = mean.A + sd.pA*c*cos(t)
      mu.B = mean.B + sd.pB*c*cos(t + acos(rho.predict))
      
      predict.A = x$misc$inv.linkfunc(mu.A)
      predict.B = x$misc$inv.linkfunc(mu.B)
      
      #### SROC Line
      if(sroc.type==1){
          g.yy = mean.A + rho*sqrt(var1/var2)*(g.xx-mean.B)
      }else if(sroc.type==2){
        if(rho<0){
          if(x$misc$model.type==2 || x$misc$model.type==3){
            g.yy = (var1-var2-sqrt((var2-var1)^2+4*rho^2*var1*var2))/(2*rho*sqrt(var1*var2))*(g.xx-mean.B)+mean.A
          }else{
            g.yy = (var1-var2+sqrt((var2-var1)^2+4*rho^2*var1*var2))/(2*rho*sqrt(var1*var2))*(g.xx-mean.B)+mean.A
          }
        }else{
          if(x$misc$model.type==1 || x$misc$model.type==4){
            g.yy = (var1-var2-sqrt((var2-var1)^2+4*rho^2*var1*var2))/(2*rho*sqrt(var1*var2))*(g.xx-mean.B)+mean.A
          }else{
            g.yy = (var1-var2+sqrt((var2-var1)^2+4*rho^2*var1*var2))/(2*rho*sqrt(var1*var2))*(g.xx-mean.B)+mean.A
          }
        }
      }else if(sroc.type==3){
        if(x$misc$model.type==2 || x$misc$model.type==3){
          g.yy = (var1 + rho*sqrt(var1*var2))/(var2 + rho*sqrt(var1*var2))*(g.xx-mean.B)+mean.A
        }else{
          g.yy = -(var1 - rho*sqrt(var1*var2))/(var2 - rho*sqrt(var1*var2))*(g.xx-mean.B)+mean.A
        }  
      }else if(sroc.type==4){
        g.yy = mean.A + 1/rho*sqrt(var1/var2)*(g.xx-mean.B)
      }else if(sroc.type==5){
        if(x$misc$model.type==2 || x$misc$model.type==3){
          g.yy = mean.A + sqrt(var1/var2)*(g.xx-mean.B)
        }else{
          g.yy = mean.A - sqrt(var1/var2)*(g.xx-mean.B)
        }
      }else{stop("Please give the correct sroc type, which is 1, 2, 3, 4 or 5.")}
      invg.xx = x$misc$inv.linkfunc(g.xx)
      invg.yy = x$misc$inv.linkfunc(g.yy)
    }
  }
  
  if(dataFit){
    if(x$misc$modality.flag){
      if(!x$misc$covariates.flag){
        min.invg.xx = min(invg.xx[which(invg.xx>=min.data.xx)])
        min.invg.yy = unlist(lapply(1:mod.level, function(ind) min(invg.yy[[ind]][which(invg.yy[[ind]]>=min.data.yy)])))
        min.xx = min(unlist(lapply(1:mod.level, function(ind) invg.xx[which(invg.yy[[ind]]==min.invg.yy[ind])])),min.invg.xx)
        max.invg.xx = max(invg.xx[which(invg.xx<=max.data.xx)])
        max.invg.yy = unlist(lapply(1:mod.level, function(ind) max(invg.yy[[ind]][which(invg.yy[[ind]]<=max.data.yy)])))
        max.xx = max(unlist(lapply(1:mod.level, function(ind) invg.xx[which(invg.yy[[ind]]==max.invg.yy[ind])])),max.invg.xx)
        ind = which(invg.xx>=min.xx & invg.xx<=max.xx)
        invg.xx = invg.xx[ind]
        invg.yy = lapply(1:mod.level, function(i) invg.yy[[i]][ind])
      }else{
        min.invg.xx = min(invg.xx[which(invg.xx>=min.data.xx)])
        min.invg.yy = min(invg.yy[which(invg.yy>=min.data.yy)])
        min.xx = min(invg.xx[which(invg.yy==min.invg.yy)],min.invg.xx)
        max.invg.xx = max(invg.xx[which(invg.xx<=max.data.xx)])
        max.invg.yy = max(invg.yy[which(invg.yy<=max.data.yy)])
        max.xx = max(invg.xx[which(invg.yy==max.invg.yy)],max.invg.xx)
        ind = which(invg.xx>=min.xx & invg.xx<=max.xx)
        invg.xx = invg.xx[ind]
        invg.yy = invg.yy[ind]
      }
    }else{
      min.invg.xx = min(invg.xx[which(invg.xx>=min.data.xx)])
      min.invg.yy = min(invg.yy[which(invg.yy>=min.data.yy)])
      min.xx = min(invg.xx[which(invg.yy==min.invg.yy)],min.invg.xx)
      max.invg.xx = max(invg.xx[which(invg.xx<=max.data.xx)])
      max.invg.yy = max(invg.yy[which(invg.yy<=max.data.yy)])
      max.xx = max(invg.xx[which(invg.yy==max.invg.yy)],max.invg.xx)
      ind = which(invg.xx>=min.xx & invg.xx<=max.xx)
      invg.xx = invg.xx[ind]
      invg.yy = invg.yy[ind]
    }
  }
  
  
  
  
  
  if(missing(xlim)){
    if(x$misc$model.type %in% c(1,3)){
      xlim = c(1,0)
      x.at = seq(1,0,by=-0.2)
      x.labels = as.character(1-x.at)
    }
    if(x$misc$model.type %in% c(2,4)){
      xlim = c(0,1)
      x.at = seq(0,1,by=0.2)
      x.labels = as.character(x.at)
    }
  }else{
    if(x$misc$model.type %in% c(1,3)){
      xlim = 1-xlim
    }
    if(x$misc$model.type==1){if(xlim[1]<xlim[2]){xlim = c(xlim[2],xlim[1])}}
    if(x$misc$model.type==2){if(xlim[2]<xlim[1]){xlim = c(xlim[2],xlim[1])}}
    if(x$misc$model.type==3){if(xlim[1]<xlim[2]){xlim = c(xlim[2],xlim[1])}}
    if(x$misc$model.type==4){if(xlim[2]<xlim[1]){xlim = c(xlim[2],xlim[1])}}
    x.temp = seq(xlim[1],xlim[2],len=4)
    x.at = unique(c(x.temp[1], round(x.temp[c(2,3)],1), x.temp[4]))
    if(x$misc$model.type %in% c(1,3)){
      x.labels = as.character(1-x.at)
    }
    if(x$misc$model.type %in% c(2,4)){
      x.labels = as.character(x.at)
    }
  }
  if(missing(ylim)){
    if(x$misc$model.type %in% c(1,2)){
      ylim = c(0,1)
      y.at = seq(0,1,by=0.2)
      y.labels = as.character(y.at)
    }
    if(x$misc$model.type %in% c(3,4)){
      ylim = c(1,0)
      y.at = seq(1,0,by=-0.2)
      y.labels = as.character(1-y.at)
    }
  }else{
    if(x$misc$model.type %in% c(3,4)){
      ylim = 1-ylim
    }
    if(x$misc$model.type==1){if(ylim[2]<ylim[1]){ylim = c(ylim[2],ylim[1])}}
    if(x$misc$model.type==2){if(ylim[2]<ylim[1]){ylim = c(ylim[2],ylim[1])}}
    if(x$misc$model.type==3){if(ylim[1]<ylim[2]){ylim = c(ylim[2],ylim[1])}}
    if(x$misc$model.type==4){if(ylim[1]<ylim[2]){ylim = c(ylim[2],ylim[1])}}
    y.temp = seq(ylim[1],ylim[2],len=4)
    y.at = unique(c(y.temp[1], round(y.temp[c(2,3)],1), y.temp[4]))
    if(x$misc$model.type %in% c(1,2)){
      y.labels = as.character(y.at)
    }
    if(x$misc$model.type %in% c(3,4)){
      y.labels = as.character(1-y.at)
    }
  }
  
  if(tolower(data.cex)=="scaled"){
    xrange = N
    info = xrange
    info = info/max(info)
    info = info*5
  }

  if(add){
    if(dataShow=="o" || dataShow=="f"){
      if(data.cex=="bubble"){
        data.col = col2rgb(data.col,alpha=F)
        fg = rgb(data.col[1],data.col[2],data.col[3],200,maxColorValue=255)
        bg = rgb(data.col[1],data.col[2],data.col[3],100,maxColorValue=255)
        symbols(x = data.xx, y = data.yy, circles=N,inches=0.35,fg=fg, bg=bg,add=T)
      }else if(data.cex=="scaled"){
        points(x = data.xx, y = data.yy, col=data.col, cex=info, pch=data.pch)
      }else{
        points(x = data.xx, y = data.yy, col=data.col, cex=data.cex, pch=data.pch)
      }
    }
    if(x$misc$covariates.flag){
      if(lineShow){
        lines(x = invg.xx, y = invg.yy, col=line.col, lty=line.lty, lwd=line.lwd)
      }
    }else{
      if(x$misc$modality.flag){
        if(crShow){
          lapply(1:mod.level, function(ind) lines(x = confidence.B[[ind]], y = confidence.A[[ind]], col=cr.col[ind], lwd=cr.lwd[ind], lty=cr.lty[ind]))
        }
        if(prShow){
          lapply(1:mod.level, function(ind) lines(x = predict.B[[ind]], y = predict.A[[ind]], col=pr.col[ind], lty=pr.lty[ind], lwd=pr.lwd[ind]))
        }
        if(lineShow){
          lapply(1:mod.level, function(ind) lines(x = invg.xx, y = invg.yy[[ind]], col=line.col[ind], lty=line.lty[ind], lwd=line.lwd[ind]))
        }
        lapply(1:mod.level, function(ind) points(x = x$misc$inv.linkfunc(mean.B), y = x$misc$inv.linkfunc(mean.A), pch=sp.pch[ind], cex=sp.cex[ind], col=sp.col[ind]))
      }else{ ## no covariates, no modality
        if(crShow){
          lines(x = confidence.B, y = confidence.A, col=cr.col, lwd=cr.lwd, lty=cr.lty)
        }
        if(prShow){
          lines(x = predict.B, y = predict.A, col=pr.col,lty=pr.lty, lwd=pr.lwd)
        }
        if(lineShow){
          lines(x = invg.xx, y = invg.yy, col=line.col,lty=line.lty, lwd=line.lwd)
        }
        points(x = x$misc$inv.linkfunc(mean.B), y = x$misc$inv.linkfunc(mean.A), pch=sp.pch,cex=sp.cex,col=sp.col)
      }
    }
    
  }else{
    if(!legend.flag){
      plot(NA,NA,xlim=xlim,ylim=ylim,main=main,asp=1,
           xaxs = "i",xaxt="n",yaxt="n",bty="o",xlab="1-Specificity",ylab="Sensitivity", ...)
      axis(1, at = x.at, labels = x.labels, ...)
      axis(2, at = y.at, labels = y.labels, ...)
      if(dataShow=="o" || dataShow=="f"){
        if(data.cex=="bubble"){
          data.col = col2rgb(data.col,alpha=F)
          fg = rgb(data.col[1],data.col[2],data.col[3],200,maxColorValue=255)
          bg = rgb(data.col[1],data.col[2],data.col[3],100,maxColorValue=255)
          symbols(x = data.xx, y = data.yy, circles=N,inches=0.35,fg=fg, bg=bg,add=T)
        }else if(data.cex=="scaled"){
          points(x = data.xx, y = data.yy, col=data.col, cex=info, pch=data.pch)
        }else{
          points(x = data.xx, y = data.yy, col=data.col, cex=data.cex, pch=data.pch)
        }
      }
      if(x$misc$covariates.flag){
        if(lineShow){
          lines(x = invg.xx, y = invg.yy, col=line.col, lty=line.lty, lwd=line.lwd)
        }
      }else{
        if(x$misc$modality.flag){
          if(crShow){
            lapply(1:mod.level, function(ind) lines(x = confidence.B[[ind]], y = confidence.A[[ind]], col=cr.col[ind], lwd=cr.lwd[ind], lty=cr.lty[ind]))
          }
          if(prShow){
            lapply(1:mod.level, function(ind) lines(x = predict.B[[ind]], y = predict.A[[ind]], col=pr.col[ind], lty=pr.lty[ind], lwd=pr.lwd[ind]))
          }
          if(lineShow){
            lapply(1:mod.level, function(ind) lines(x = invg.xx, y = invg.yy[[ind]], col=line.col[ind], lty=line.lty[ind], lwd=line.lwd[ind]))
          }
          lapply(1:mod.level, function(ind) points(x = x$misc$inv.linkfunc(mean.B[ind]), y = x$misc$inv.linkfunc(mean.A[ind]), pch=sp.pch[ind], cex=sp.cex[ind], col=sp.col[ind]))
        }else{ ## no covariates, no modality
          if(crShow){
            lines(x = confidence.B, y = confidence.A, col=cr.col, lwd=cr.lwd, lty=cr.lty)
          }
          if(prShow){
            lines(x = predict.B, y = predict.A, col=pr.col,lty=pr.lty, lwd=pr.lwd)
          }
          if(lineShow){
            lines(x = invg.xx, y = invg.yy, col=line.col,lty=line.lty, lwd=line.lwd)
          }
          points(x = x$misc$inv.linkfunc(mean.B), y = x$misc$inv.linkfunc(mean.A), pch=sp.pch,cex=sp.cex,col=sp.col)
        }
      }
    }else{ # has legend
      if(legend=="bottom"){
        layout(mat = matrix(c(1,2), nrow = 2, ncol = 1), heights = c(1,1))
      }else if(legend=="left"){
        layout(mat = matrix(c(2,1), nrow = 1, ncol = 2), widths = c(1,1))
      }else{
        layout(mat = matrix(c(1,2), nrow = 1, ncol = 2), widths = c(1,1))
      }
      # figure 1 --- main figure
      plot(NA,NA,xlim=xlim,ylim=ylim,main=main,asp=1,
           xaxs = "i",xaxt="n",yaxt="n",bty="o",xlab="1-Specificity",ylab="Sensitivity", ...)
      axis(1, at = x.at, labels = x.labels, ...)
      axis(2, at = y.at, labels = y.labels, ...)
      if(dataShow=="o" || dataShow=="f"){
        if(data.cex=="bubble"){
          data.col = col2rgb(data.col,alpha=F)
          fg = rgb(data.col[1],data.col[2],data.col[3],200,maxColorValue=255)
          bg = rgb(data.col[1],data.col[2],data.col[3],100,maxColorValue=255)
          symbols(x = data.xx, y = data.yy, circles=N,inches=0.35,fg=fg, bg=bg,add=T)
        }else if(data.cex=="scaled"){
          points(x = data.xx, y = data.yy, col=data.col, cex=info, pch=data.pch)
        }else{
          points(x = data.xx, y = data.yy, col=data.col, cex=data.cex, pch=data.pch)
        }
      }
      if(x$misc$covariates.flag){
        if(lineShow){
          lines(x = invg.xx, y = invg.yy, col=line.col, lty=line.lty, lwd=line.lwd)
        }
      }else{
        if(x$misc$modality.flag){
          if(crShow){
            lapply(1:mod.level, function(ind) lines(x = confidence.B[[ind]], y = confidence.A[[ind]], col=cr.col[ind], lwd=cr.lwd[ind], lty=cr.lty[ind]))
          }
          if(prShow){
            lapply(1:mod.level, function(ind) lines(x = predict.B[[ind]], y = predict.A[[ind]], col=pr.col[ind], lty=pr.lty[ind], lwd=pr.lwd[ind]))
          }
          if(lineShow){
            lapply(1:mod.level, function(ind) lines(x = invg.xx, y = invg.yy[[ind]], col=line.col[ind], lty=line.lty[ind], lwd=line.lwd[ind]))
          }
          lapply(1:mod.level, function(ind) points(x = x$misc$inv.linkfunc(mean.B[ind]), y = x$misc$inv.linkfunc(mean.A[ind]), pch=sp.pch[ind], cex=sp.cex[ind], col=sp.col[ind]))
        }else{ ## no covariates, no modality
          if(crShow){
            lines(x = confidence.B, y = confidence.A, col=cr.col, lwd=cr.lwd, lty=cr.lty)
          }
          if(prShow){
            lines(x = predict.B, y = predict.A, col=pr.col,lty=pr.lty, lwd=pr.lwd)
          }
          if(lineShow){
            lines(x = invg.xx, y = invg.yy, col=line.col,lty=line.lty, lwd=line.lwd)
          }
          points(x = x$misc$inv.linkfunc(mean.B), y = x$misc$inv.linkfunc(mean.A), pch=sp.pch,cex=sp.cex,col=sp.col)
        }
      }
      # figure 2 --- legend
      plot(NA,NA,xlim=xlim,ylim=ylim,main="",asp=1,xaxs = "i",xaxt="n",yaxt="n",bty="n",xlab="",ylab="")
      if(x$misc$covariates.flag){
        if(dataShow=="o" || dataShow=="f"){
          if(data.cex=="bubble"){
            legend(legend.pos, 
                   legend=c("SROC line","Data points"), 
                   pch=c(NA,16), 
                   col=c(line.col,data.col),
                   lty=c(line.lty,NA),
                   horiz=FALSE,bty="n", cex=legend.cex)
          }else{
            legend(legend.pos, 
                   legend=c("SROC line","Data points"), 
                   pch=c(NA,data.pch), 
                   col=c(line.col,data.col),
                   lty=c(line.lty,NA),
                   horiz=FALSE,bty="n", cex=legend.cex)
          }
        }else{ # no data
          legend(legend.pos, 
                 legend="SROC line", 
                 col=c(line.col),
                 lty=c(line.lty),
                 horiz=FALSE,bty="n", cex=legend.cex)
        }
      }else{# no covariates
        if(x$misc$modality.flag){
          mod.names = unique(x$data[,x$misc$modality.name])
          mod.level = x$misc$modality.level
          
          sp_list = paste("Summary point for ", mod.names, sep="")
          sroc_list = paste("SROC line for ", mod.names, sep="")
          cr_list = paste("Credible region for ", mod.names, sep="")
          pr_list = paste("Prediction region for ", mod.names, sep="")
        }else{
          mod.level = 1
          sp_list = c("Summary point")
          sroc_list = c("SROC line")
          cr_list = c("Credible region")
          pr_list = c("Prediction region")
        }
        if(dataShow=="o" || dataShow=="f"){
          if(data.cex=="bubble"){
            legend(legend.pos, 
                   legend=c(sp_list, sroc_list,cr_list,pr_list,"Data points"), 
                   pch=c(sp.pch,rep(c(NA,NA,NA),mod.level),16), 
                   col=c(sp.col, line.col,cr.col,pr.col, data.col),
                   lty=c(rep(NA,mod.level),line.lty,cr.lty, pr.lty,NA),
                   horiz=FALSE,bty="n", cex=legend.cex)
          }else{
            legend(legend.pos, 
                   legend=c(sp_list, sroc_list,cr_list,pr_list,"Data points"), 
                   pch=c(sp.pch,rep(c(NA,NA,NA),mod.level),data.pch), 
                   col=c(sp.col, line.col,cr.col,pr.col, data.col),
                   lty=c(rep(NA,mod.level),line.lty,cr.lty, pr.lty,NA),
                   horiz=FALSE,bty="n", cex=legend.cex)
          }
        }else{ # no data
          legend(legend.pos, 
                 legend=c(sp_list, sroc_list,cr_list,pr_list), 
                 pch=c(sp.pch,rep(c(NA,NA,NA),mod.level)), 
                 col=c(sp.col, line.col,cr.col,pr.col),
                 lty=c(rep(NA,mod.level),line.lty,cr.lty, pr.lty),
                 horiz=FALSE,bty="n", cex=legend.cex)
        }
      }
    }
    
  }
  return(invisible())
}