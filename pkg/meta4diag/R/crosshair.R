crosshair <- function(x, ...) UseMethod("crosshair")

crosshair.meta4diag = function(x, est.type="mean", add=FALSE, main="Crosshair Plot", xlim, ylim, col, ...){
  if(class(x)!="meta4diag"){stop("Wrong input given!")}
  if(!(est.type %in% c("mean","median"))){stop("Argument \"est.type\" should be \"mean\" or \"median\".")}

  fitname = x$names.fitted
  fullname = paste("summary.predictor.(",fitname,")",sep="")
  
  if(est.type=="mean"){
    est.A = x[[fullname[1]]][,"mean"]
    est.B = x[[fullname[2]]][,"mean"]
  }
  if(est.type=="median"){
    est.A = x[[fullname[1]]][,"0.5quant"]
    est.B = x[[fullname[2]]][,"0.5quant"]
  }
  
  
  lb.A = x[[fullname[1]]][,"0.025quant"]
  ub.A = x[[fullname[1]]][,"0.975quant"]
  lb.B = x[[fullname[2]]][,"0.025quant"]
  ub.B = x[[fullname[2]]][,"0.975quant"]
  
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
  if(missing(col)){
    if(x$misc$covariates.flag){
      col = c(1:length(est.B))
      if(add){
        points(est.B, est.A, col=col)
        arrows(lb.B, est.A, ub.B, est.A, angle=90, code=3, length=0.05, col=col)
        arrows(est.B, lb.A, est.B, ub.A, angle=90, code=3, length=0.05, col=col)
      }else{
        plot(NA,NA,xlim=xlim,ylim=ylim,main=main,asp=1,
             xaxs = "i",xaxt="n",yaxt="n",bty="o",xlab="1-Specificity",ylab="Sensitivity", ...)
        axis(1, at = x.at, labels = x.labels, ...)
        axis(2, at = y.at, labels = y.labels, ...)
        points(est.B, est.A,col=col)
        arrows(lb.B, est.A, ub.B, est.A, angle=90, code=3, length=0.05,col=col)
        arrows(est.B, lb.A, est.B, ub.A, angle=90, code=3, length=0.05,col=col)
      }
    }else{
      if(x$misc$modality.flag){
        col = rainbow(x$misc$modality.level)
        modalitynames = unique(x$data[,x$misc$modality.name])
        ind = lapply(1:x$misc$modality.level, function(i) which(x$data[,x$misc$modality.name]==modalitynames[i]))
        if(add){
          lapply(1:x$misc$modality.level, function(i){
            points(est.B[ind[[i]]], est.A[ind[[i]]], col=col[i])
            arrows(lb.B[ind[[i]]], est.A[ind[[i]]], ub.B[ind[[i]]], est.A[ind[[i]]], angle=90, code=3, length=0.05, col=col[i])
            arrows(est.B[ind[[i]]], lb.A[ind[[i]]], est.B[ind[[i]]], ub.A[ind[[i]]], angle=90, code=3, length=0.05, col=col[i])
          })
        }else{
          plot(NA,NA,xlim=xlim,ylim=ylim,main=main,asp=1,
               xaxs = "i",xaxt="n",yaxt="n",bty="o",xlab="1-Specificity",ylab="Sensitivity", ...)
          axis(1, at = x.at, labels = x.labels, ...)
          axis(2, at = y.at, labels = y.labels, ...)
          lapply(1:x$misc$modality.level, function(i){
            points(est.B[ind[[i]]], est.A[ind[[i]]],col=col[i])
            arrows(lb.B[ind[[i]]], est.A[ind[[i]]], ub.B[ind[[i]]], est.A[ind[[i]]], angle=90, code=3, length=0.05,col=col[i])
            arrows(est.B[ind[[i]]], lb.A[ind[[i]]], est.B[ind[[i]]], ub.A[ind[[i]]], angle=90, code=3, length=0.05,col=col[i])
          })
        }
        
          
        
      }else{
        col="black"
        if(add){
          points(est.B, est.A, col=col)
          arrows(lb.B, est.A, ub.B, est.A, angle=90, code=3, length=0.05, col=col)
          arrows(est.B, lb.A, est.B, ub.A, angle=90, code=3, length=0.05, col=col)
        }else{
          plot(NA,NA,xlim=xlim,ylim=ylim,main=main,asp=1,
               xaxs = "i",xaxt="n",yaxt="n",bty="o",xlab="1-Specificity",ylab="Sensitivity", ...)
          axis(1, at = x.at, labels = x.labels, ...)
          axis(2, at = y.at, labels = y.labels, ...)
          points(est.B, est.A,col=col)
          arrows(lb.B, est.A, ub.B, est.A, angle=90, code=3, length=0.05,col=col)
          arrows(est.B, lb.A, est.B, ub.A, angle=90, code=3, length=0.05,col=col)
        }
      }
    }
  }else{
    if(add){
      points(est.B, est.A, col=col)
      arrows(lb.B, est.A, ub.B, est.A, angle=90, code=3, length=0.05, col=col)
      arrows(est.B, lb.A, est.B, ub.A, angle=90, code=3, length=0.05, col=col)
    }else{
      plot(NA,NA,xlim=xlim,ylim=ylim,main=main,asp=1,
           xaxs = "i",xaxt="n",yaxt="n",bty="o",xlab="1-Specificity",ylab="Sensitivity", ...)
      axis(1, at = x.at, labels = x.labels, ...)
      axis(2, at = y.at, labels = y.labels, ...)
      points(est.B, est.A,col=col)
      arrows(lb.B, est.A, ub.B, est.A, angle=90, code=3, length=0.05,col=col)
      arrows(est.B, lb.A, est.B, ub.A, angle=90, code=3, length=0.05,col=col)
    }
  }
  return(invisible())
}