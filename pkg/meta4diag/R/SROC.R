SROC <- function(x, ...) UseMethod("SROC")

SROC.meta4diag = function(x, est.type="mean", sp.cex=1.5,sp.pch="*",sp.col="red",
                          dataShow=T, data.col="#FF0000FF",
                          sroc.type=1, lineShow=T, line.lty=1, line.lwd=2, line.col="black",
                          crShow=T, cr.lty=2, cr.lwd=1.5, cr.col="blue",
                          prShow=T, pr.lty=3, pr.lwd=1,  pr.col="darkgray",
                          add=FALSE, save=F, xlab,ylab,main,xlim,ylim,...){
  if(class(x)!="meta4diag"){stop("Wrong input given!")}
  
  fitname = x$names.fitted
  I = dim(x$data)[1]
  f = qf(0.95, 2, I-2)
  c = sqrt(2*f)
  t = seq(0, 2*pi, by = 2*pi/100)
  est.type = tolower(est.type)
  
  if(est.type=="mean"){
    mean.A = x[["summary.summarized.fixed"]][1,1]
    mean.B = x[["summary.summarized.fixed"]][2,1]
    var1 = x[["summary.hyperpar"]][1,1]
    var2 = x[["summary.hyperpar"]][2,1]
    rho = x[["summary.hyperpar"]][3,1]
  }
  if(est.type=="median"){
    mean.A = x[["summary.summarized.fixed"]][1,4]
    mean.B = x[["summary.summarized.fixed"]][2,4]
    var1 = x[["summary.hyperpar"]][1,4]
    var2 = x[["summary.hyperpar"]][2,4]
    rho = x[["summary.hyperpar"]][3,4]
  }
  
  
  sd.A = x[["summary.summarized.fixed"]][1,2]
  sd.B = x[["summary.summarized.fixed"]][2,2]
  
  # confidence
  r = x[["summarized.fixed.correlation.matrix"]][1,2]
  mu.A = mean.A + sd.A*c*cos(t)
  mu.B = mean.B + sd.B*c*cos(t + acos(r))
  
  confidence.A = .invlogit(mu.A)
  confidence.B = .invlogit(mu.B)
  
  # predict
  covariance.predict = rho*sqrt(var1*var2) + r*sd.A*sd.B
  sd.pA = sqrt(var1 + sd.A^2)
  sd.pB = sqrt(var2 + sd.B^2)
  rho.predict = covariance.predict/(sd.pA*sd.pB)
  mu.A = mean.A + sd.pA*c*cos(t)
  mu.B = mean.B + sd.pB*c*cos(t + acos(rho.predict))
  
  predict.A = .invlogit(mu.A)
  predict.B = .invlogit(mu.B)
  
  #### SROC Line
  datatemp = x$outdata
  fit1ind = seq(1,dim(datatemp)[1],by=2)
  fit2ind = seq(2,dim(datatemp)[1],by=2)
  rate = datatemp$Y/datatemp$Ntrials
  datafit1 = rate[fit1ind]
  datafit2 = rate[fit2ind]
  N = datatemp$Ntrials[fit1ind]+datatemp$Ntrials[fit2ind]
  
  logit.xx = seq(-6,6,by=0.01)
  if(sroc.type==1){
    logit.yy = mean.A + rho*sqrt(var1/var2)*(logit.xx-mean.B)
  }else if(sroc.type==2){
    if(rho<0){
      if(x$misc$model.type==2 || x$misc$model.type==3){
        logit.yy = (var1-var2-sqrt((var2-var1)^2+4*rho^2*var1*var2))/(2*rho*sqrt(var1*var2))*(logit.xx-mean.B)+mean.A
      }else{
        logit.yy = (var1-var2+sqrt((var2-var1)^2+4*rho^2*var1*var2))/(2*rho*sqrt(var1*var2))*(logit.xx-mean.B)+mean.A
      }
    }else{
      if(x$misc$model.type==1 || x$misc$model.type==4){
        logit.yy = (var1-var2-sqrt((var2-var1)^2+4*rho^2*var1*var2))/(2*rho*sqrt(var1*var2))*(logit.xx-mean.B)+mean.A
      }else{
        logit.yy = (var1-var2+sqrt((var2-var1)^2+4*rho^2*var1*var2))/(2*rho*sqrt(var1*var2))*(logit.xx-mean.B)+mean.A
      }
    }
  }else if(sroc.type==3){
    logit.yy = (var1 + rho*sqrt(var1*var2))/(var2 + rho*sqrt(var1*var2))*(logit.xx-mean.B)+mean.A
  }else if(sroc.type==4){
    logit.yy = mean.A + 1/rho*sqrt(var1/var2)*(logit.xx-mean.B)
  }else if(sroc.type==5){
    logit.yy = mean.A + sqrt(var1/var2)*(logit.xx-mean.B)
  }else{stop("Please give the correct sroc type, which is 1, 2, 3, 4 or 5.")}
  
  ww = .invlogit(logit.yy)
  tt = .invlogit(logit.xx)
  
  if(missing(xlim)){
    if(x$misc$model.type==1){xlim = c(1,0)}
    if(x$misc$model.type==2){xlim = c(0,1)}
    if(x$misc$model.type==3){xlim = c(1,0)}
    if(x$misc$model.type==4){xlim = c(0,1)}
  }else{
    if(x$misc$model.type==1){if(xlim[1]<xlim[2]){xlim = c(xlim[2],xlim[1])}}
    if(x$misc$model.type==2){if(xlim[2]<xlim[1]){xlim = c(xlim[2],xlim[1])}}
    if(x$misc$model.type==3){if(xlim[1]<xlim[2]){xlim = c(xlim[2],xlim[1])}}
    if(x$misc$model.type==4){if(xlim[2]<xlim[1]){xlim = c(xlim[2],xlim[1])}}
  }
  if(missing(ylim)){
    if(x$misc$model.type==1){ylim = c(0,1)}
    if(x$misc$model.type==2){ylim = c(0,1)}
    if(x$misc$model.type==3){ylim = c(1,0)}
    if(x$misc$model.type==4){ylim = c(1,0)}
  }else{
    if(x$misc$model.type==1){if(ylim[2]<ylim[1]){ylim = c(ylim[2],ylim[1])}}
    if(x$misc$model.type==2){if(ylim[2]<ylim[1]){ylim = c(ylim[2],ylim[1])}}
    if(x$misc$model.type==3){if(ylim[1]<ylim[2]){ylim = c(ylim[2],ylim[1])}}
    if(x$misc$model.type==4){if(ylim[1]<ylim[2]){ylim = c(ylim[2],ylim[1])}}
  }
  if(add){
    if(dataShow){
      data.col = col2rgb(data.col,alpha=F)
      fg = rgb(data.col[1],data.col[2],data.col[3],200,maxColorValue=255)
      bg = rgb(data.col[1],data.col[2],data.col[3],100,maxColorValue=255)
      symbols(x = datafit2, y = datafit1,circles=N,inches=0.35,fg=fg, bg=bg,add=T)
    }
    if(crShow){
      lines(x = confidence.B, y = confidence.A, col=cr.col, lwd=cr.lwd, lty=cr.lty)
    }
    if(prShow){
      lines(x = predict.B, y = predict.A, col=pr.col,lty=pr.lty, lwd=pr.lwd)
    }
    if(lineShow){
      lines(x = tt, y = ww, col=line.col,lty=line.lty, lwd=line.lwd)
    }
    points(x = .invlogit(mean.B), y = .invlogit(mean.A), type="p", pch=sp.pch,cex=sp.cex,col=sp.col)
  }else{
    
    if(missing(main)){main="SROC Plot"}
    if(missing(xlab)){xlab = fitname[2]}
    if(missing(ylab)){ylab = fitname[1]}

    par(mfrow=c(1,1),mar=c(5.1, 4.1, 4.1, 2.1))
    plot(-10,-10,xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab,main=main,asp=1,
         xaxs = "i",family="sans",xaxt="s",yaxt="s",bty="o",...)
    if(dataShow){
      data.col = col2rgb(data.col,alpha=F)
      fg = rgb(data.col[1],data.col[2],data.col[3],200,maxColorValue=255)
      bg = rgb(data.col[1],data.col[2],data.col[3],100,maxColorValue=255)
      symbols(x = datafit2, y = datafit1,circles=N,inches=0.35,fg=fg, bg=bg,add=T)
    }
    if(crShow){
      lines(x = confidence.B, y = confidence.A, col=cr.col, lwd=cr.lwd, lty=cr.lty)
    }
    if(prShow){
      lines(x = predict.B, y = predict.A, col=pr.col,lty=pr.lty, lwd=pr.lwd)
    }
    if(lineShow){
      lines(x = tt, y = ww, col=line.col,lty=line.lty, lwd=line.lwd)
    }
    points(x = .invlogit(mean.B), y = .invlogit(mean.A), pch=sp.pch,cex=sp.cex,col=sp.col)
  }
}