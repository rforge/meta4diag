forest <- function(x, ...) UseMethod("forest")

forest.meta4diag = function(x, accuracy.type="sens", est.type="mean", p.cex="scaled", p.pch=15, p.col="black",
                            nameShow="right", dataShow="center", ciShow="left", cex=1,
                            shade.col="gray", arrow.col="black", arrow.lty=1, arrow.lwd=1,
                            main="Forest Plot", main.cex=1.5, axis.cex=1,...){
  
  if(length(accuracy.type)!=1){stop("Argument \"accuracy.type\" could only be one character string.")}
  if(!is.character(accuracy.type)){stop("Argument \"accuracy.type\" could only be one character string.")}
  accuracy.type = tolower(accuracy.type)
  suitable.set = c("sens", "TPR", "spec", "TNR", "FPR", "FNR", "LRpos", "LRneg", "DOR")
  if(!(accuracy.type %in% tolower(suitable.set))){
    stop(paste("Please give the correct accuracy.type type, which could be ",paste(suitable.set, collapse=", "),".",sep=""))
  }
  if(!x$misc$sample.flag){
    if(accuracy.type %in% tolower(c("LRpos", "LRneg", "DOR"))){
      stop("The statistics is not the default return. Please let \"nsample=TRUE\" in the \"meta4diag()\" function.")
    }
  }
  est.type = tolower(est.type)
  if(!(est.type %in% c("mean","median"))){
    stop("Argument \"est.type\" could only be either \"mean\" or \"median\".")
  }
  
  ####### check nameShow
  if(!is.logical(nameShow)){
    if(!is.character(nameShow)){
      nameFlag = FALSE
      stop("Argument \"nameShow\" could only be FALSE, TRUE, \"left\", \"right\" or \"center\".")
    }else{
      if(tolower(nameShow) %in% c("left","right","center")){
        nameShow = tolower(nameShow)
        nameFlag = TRUE
      }else{
        nameFlag = FALSE
        stop("Argument \"nameShow\" could only be FALSE, TRUE, \"left\", \"right\" or \"center\".")
      }
    }
  }else{
    if(nameShow){
      nameShow = "right"
      nameFlag = TRUE
    }else{nameFlag = FALSE}
  }
  
  ####### check dataShow
  if(!is.logical(dataShow)){
    if(!is.character(dataShow)){
      dataFlag = FALSE
      stop("Argument \"dataShow\" could only be FALSE, TRUE, \"left\", \"right\" or \"center\".")
    }else{
      if(tolower(dataShow) %in% c("left","right","center")){
        dataShow = tolower(dataShow)
        dataFlag = TRUE
      }else{
        dataFlag = FALSE
        stop("Argument \"dataShow\" could only be FALSE, TRUE, \"left\", \"right\" or \"center\".")
      }
    }
  }else{
    if(dataShow){
      dataShow = "right"
      dataFlag = TRUE
    }else{dataFlag = FALSE}
  }
  
  ####### check ciShow
  if(!is.logical(ciShow)){
    if(!is.character(ciShow)){
      ciFlag = FALSE
      stop("Argument \"ciShow\" could only be FALSE, TRUE, \"left\", \"right\" or \"center\".")
    }else{
      if(tolower(ciShow) %in% c("left","right","center")){
        ciShow = tolower(ciShow)
        ciFlag = TRUE
      }else{
        ciFlag = FALSE
        stop("Argument \"ciShow\" could only be FALSE, TRUE, \"left\", \"right\" or \"center\".")
      }
    }
  }else{
    if(ciShow){
      ciShow = "right"
      ciFlag = TRUE
    }else{ciFlag = FALSE}
  }
  if(tolower(p.cex)!="scaled"){
    if(!is.numeric(p.cex)){
      stop("Argument \"p.cex\" could only be scaled or fixed to 1,2,3,....")
    }
  }
  
  ######################### main estimates
  if(accuracy.type=="sens" || accuracy.type=="tpr"){
    fitname = "True positive rate (Sensitivity)"
    fit = x[["summary.fitted.(Se)"]]
    sfit = x[["summary.summarized.fitted"]]["mean(Se)",]
    xmin = min(fit[,"0.025quant"])
    xmax = max(fit[,"0.975quant"])
  }
  if(accuracy.type=="spec" || accuracy.type=="tnr"){
    fitname = "True negative rate (Specificity)"
    fit = x[["summary.fitted.(Sp)"]]
    sfit = x[["summary.summarized.fitted"]]["mean(Sp)",]
    xmin = min(fit[,"0.025quant"])
    xmax = max(fit[,"0.975quant"])
  }
  if(accuracy.type=="fpr"){
    fitname = "False positive rate (1-Specificity)"
    fit = x[["summary.fitted.(1-Sp)"]]
    sfit = x[["summary.summarized.fitted"]]["mean(1-Sp)",]
    xmin = min(fit[,"0.025quant"])
    xmax = max(fit[,"0.975quant"])
    
  }
  if(accuracy.type=="fnr"){
    fitname = "False negative rate (1-Sensitivity)"
    fit = x[["summary.fitted.(1-Se)"]]
    sfit = x[["summary.summarized.fitted"]]["mean(1-Se)",]
    xmin = min(fit[,"0.025quant"])
    xmax = max(fit[,"0.975quant"])
    
  }
  if(accuracy.type=="lrpos"){
    fitname = "Positive likelihood ratio (LR+)"
    fit = x[["summary.fitted.LRpos"]]
    sfit = x[["summary.summarized.statistics"]]["mean(LRpos)",]
    scale = 1.5*(sfit["0.975quant"]-sfit["0.025quant"])
    xmin = max(0, sfit["0.025quant"]-scale) 
    xmax = sfit["0.975quant"]+scale
    
  }
  if(accuracy.type=="lrneg"){
    fitname = "Negative likelihood ratio (LR-)"
    fit = x[["summary.fitted.LRneg"]]
    sfit = x[["summary.summarized.statistics"]]["mean(LRneg)",]
    scale = 1.5*(sfit["0.975quant"]-sfit["0.025quant"])
    xmin = max(0, sfit["0.025quant"]-scale) 
    xmax = sfit["0.975quant"]+scale
    
  }
  if(accuracy.type=="dor"){
    fitname = "Diagnostic odds ratio (DOR)"
    fit = x[["summary.fitted.DOR"]]
    sfit = x[["summary.summarized.statistics"]]["mean(DOR)",]
    scale = 1.5*(sfit["0.975quant"]-sfit["0.025quant"])
    xmin = max(0, sfit["0.025quant"]-scale) 
    xmax = sfit["0.975quant"]+scale
  }
  
  nr = dim(fit)[1]
  
  if(est.type=="mean"){
    est = c(fit[,1],sfit[1])
    ml.est = max(nchar(as.character(round(est,2))))
    ci.est = format(round(est,2), nsmall = 2L)
  }
  if(est.type=="median"){
    est = c(fit[,"0.5quant"],sfit["0.5quant"])
    ml.est = max(nchar(as.character(round(est,2))))
    ci.est = format(round(est,2), nsmall = 2L, width=ml.est, justify="right")
  }
  
  lb = c(fit[,"0.025quant"],sfit["0.025quant"])
  ml.lb = max(nchar(as.character(round(lb,2)),type="width"))
  ci.lb = format(round(lb,2), nsmall = 2L, width=ml.lb, justify="right")
  
  ub = c(fit[,"0.975quant"],sfit["0.975quant"])
  ml.ub = max(nchar(as.character(round(ub,2)),type="width"))
  ci.ub = format(round(ub,2), nsmall = 2L, width=ml.ub, justify="right")
  
  ci = paste(ci.est," [ ",ci.lb,", ",ci.ub," ]",sep="")
  
  exlb = which(fit[,"0.025quant"]<xmin)
  exub = which(fit[,"0.975quant"]>xmax)
  
  ###### data
  PP = x$data$tp + x$data$fn
  TPlab = format(x$data$tp, width=max(nchar(as.character(x$data$tp),type="width")))
  PPlab = format(PP, width=max(nchar(as.character(PP),,type="width")))
  datalab1 = paste(TPlab,"/",PPlab,sep="")
  NN = x$data$tn + x$data$fp
  TNlab = format(x$data$tn, width=max(nchar(as.character(x$data$tn),type="width")))
  NNlab = format(NN, width=max(nchar(as.character(NN),type="width")))
  datalab2 = paste(TNlab,"/",NNlab,sep="")
  
  studynames = rownames(fit)
  ###### caculate width
  strwidth_names_main = strwidth(studynames,units="in",cex=cex,font=1,family="sans")
  strwidth_names_title = strwidth("Study",units="in",cex=cex,font=2,family="sans")
  strwidth_names_sum = strwidth("Summary",units="in",cex=cex,font=2,family="sans")
  
  name_width = max(strwidth_names_main,strwidth_names_title,strwidth_names_sum)
  
  strwidth_data_main1 = strwidth(datalab1,units="in",cex=cex,font=1,family="sans")
  strwidth_data_main2 = strwidth(datalab2,units="in",cex=cex,font=1,family="sans")
  strwidth_data_title1 = strwidth("TP/(TP+FN)",units="in",cex=cex,font=2,family="sans")
  strwidth_data_title2 = strwidth("TN/(TN+FP)",units="in",cex=cex,font=2,family="sans")
  
  data1_width = max(strwidth_data_main1,strwidth_data_title1)
  data2_width = max(strwidth_data_main2,strwidth_data_title2)
  
  if(accuracy.type %in% c("dor","lrneg","lrpos")){
    strwidth_ci_main = strwidth(ci[1:nr],units="in",cex=cex,font=1,family="mono")
    strwidth_ci_sum = strwidth(ci[nr+1],units="in",cex=cex,font=2,family="mono")
  }else{
    strwidth_ci_main = strwidth(ci[1:nr],units="in",cex=cex,font=1,family="sans")
    strwidth_ci_sum = strwidth(ci[nr+1],units="in",cex=cex,font=2,family="sans")
  }
  strwidth_ci_title = strwidth("Estimate",units="in",cex=cex,font=2,family="sans")
  
  ci_width = max(strwidth_ci_main,strwidth_ci_title,strwidth_ci_sum)
  
  figure_width = max(name_width,data1_width,data2_width,ci_width)*2.3

  if(missing(main)){
    main = paste("Forest plot for ",fitname,sep="")
  }
  
  flags = c(nameFlag, dataFlag, ciFlag)*1
  ncFlag = sum(flags)
  
  nc = ncFlag + 1
  
  if(tolower(p.cex)=="scaled"){
    xrange = ub-lb
    info = 1/xrange
    info = info/max(info)
    info = info[1:nr]
  }else{
    info = rep(p.cex,nr)
  }
  
  if(ncFlag==3){
    layout(matrix(c(1,2,3,4,5),1,5),c(name_width,data1_width,data2_width,figure_width,ci_width),1) # name data data figure ci
  }
  if(ncFlag==2){
    if(!nameFlag){
      #figure_size = data1_width + data2_width + graph_width + ci_width
      layout(matrix(c(1,2,3,4),1,4),c(data1_width,data2_width,figure_width,ci_width),1) # data data figure ci
    }
    if(!dataFlag){
      #figure_size = name_width + graph_width + ci_width
      layout(matrix(c(1,2,3),1,3),c(name_width,figure_width,ci_width),1) # name figure ci
    }
    if(!ciFlag){
      #figure_size = name_width + data1_width + data2_width + graph_width 
      layout(matrix(c(1,2,3,4),1,4),c(name_width,data1_width,data2_width,figure_width),1) # name data data figure
    }
  }
  if(ncFlag==1){
    if(nameFlag){
      #figure_size = name_width + graph_width 
      layout(matrix(c(1,2),1,2),c(name_width,figure_width),1) # name figure
    }
    if(dataFlag){
      #figure_size = data1_width + data2_width + graph_width 
      layout(matrix(c(1,2,3),1,3),c(data1_width,data2_width,figure_width),1) # data figure
    }
    if(ciFlag){
      #figure_size = graph_width + ci_width
      layout(matrix(c(1,2),1,2),c(figure_width,ci_width),1) # figure ci
    }
  }
  if(ncFlag==0){
    layout(matrix(c(1),1,1),1,1) # figure
  }
  
  op <- par(no.readonly = TRUE)
  par(mar=c(4.5, 0.5, 3.5, 0.5),xaxs="i",family="sans",xaxt="n",yaxt="n",bty="n")
  

  if(nameFlag){
    plot.new()
    strwidth_names_main = strwidth(studynames,units="user",cex=cex,font=1,family="sans")
    strwidth_names_title = strwidth("Study",units="user",cex=cex,font=2,family="sans")
    strwidth_names_sum = strwidth("Summary",units="user",cex=cex,font=2,family="sans")
    
    name_width = max(strwidth_names_main,strwidth_names_title,strwidth_names_sum)
    
    name_adj = switch(nameShow,left=0,right=1,center=0.5)
    xlim = switch(nameShow,left=c(0,name_width),right=c(-name_width,0),center=c(-0.5,0.5)*name_width)
    plot.window(xlim=xlim,ylim=c(0,nr+1),mar=c(4.5, 1.5, 3.5, 1.5),xlab="",ylab="",xaxs="i",family="sans",xaxt="n",yaxt="n",bty="n")
    text(rep(0,nr),c(nr:1), studynames, adj=c(name_adj,0.5),family="sans",font=1,cex=cex)
    text(0,0,"Summary", adj=c(name_adj,0.5),family="sans",font=2,cex=cex)
    text(0,nr+1, "Study", adj=c(name_adj,0.5),family="sans",font=2,cex=cex)
  }
  if(dataFlag){
    plot.new()
    
    strwidth_data_main1 = strwidth(datalab1,units="user",cex=cex,font=1,family="sans")
    strwidth_data_main2 = strwidth(datalab2,units="user",cex=cex,font=1,family="sans")
    strwidth_data_title1 = strwidth("TP/(TP+FN)",units="user",cex=cex,font=2,family="sans")
    strwidth_data_title2 = strwidth("TN/(TN+FP)",units="user",cex=cex,font=2,family="sans")
    
    data1_width = max(strwidth_data_main1,strwidth_data_title1)
    data2_width = max(strwidth_data_main2,strwidth_data_title2)

    data_adj = switch(dataShow,left=0,right=1,center=0.5)
    xlim1 = switch(dataShow,left=c(0,data1_width),right=c(-data1_width,0),center=data1_width*c(-0.5,0.5))
    xlim2 = switch(dataShow,left=c(0,data2_width),right=c(-data2_width,0),center=data2_width*c(-0.5,0.5))
    plot.window(xlim=xlim1,ylim=c(0,nr+1),mar=c(4.5, 1.5, 3.5, 1.5),xlab="",ylab="",xaxs="i",family="sans",xaxt="n",yaxt="n",bty="n")
    text(rep(0,nr),c(nr:1), datalab1, adj=c(data_adj,0.5),family="sans",font=1,cex=cex)
    text(0,nr+1,"TP/(TP+FN)", adj=c(data_adj,0.5),family="sans",font=2, cex=cex)
    plot(-10,-10,xlim=xlim2,ylim=c(0,nr+1),mar=c(4.5, 1.5, 3.5, 1.5),xlab="",ylab="",xaxs="i",family="sans",xaxt="n",yaxt="n",bty="n")
    text(rep(0,nr),c(nr:1), datalab2, adj=c(data_adj,0.5),family="sans",cex=cex)
    text(0,nr+1,"TN/(TN+FP)", adj=c(data_adj,0.5),family="sans",font=2, cex=cex)
  }

  if(accuracy.type %in% c("dor","lrneg","lrpos")){
    xlim = c(max(xmin-0.1,0),xmax)
  }else{
    xlim = c(max(xmin-0.1,0),min(xmax+0.1,1))
  }
  plot(-10,-10,xlim=xlim,ylim=c(0,nr+1),mar=c(4.5, 0.5, 3.5, 0.5),xlab="",ylab="",xaxs="i",family="sans",xaxt="s",yaxt="n",bty="n")
  polygon(sfit[c("0.025quant","0.975quant","0.975quant","0.025quant")],c(-1,-1,nr+1,nr+1),col=shade.col,angle=45,density=10,border = NA)
  arrows(lb,c(nr:0),ub,c(nr:0),col=arrow.col,angle = 90,length=0.03, code=3)
  points(est[1:nr],c(nr:1),pch=p.pch,cex=info,col=p.col)
  polygon(c(0.5*(est[nr+1]+sfit[3]),est[nr+1],0.5*(est[nr+1]+sfit[5]),est[nr+1]),c(0,-0.3,0,0.3),col=p.col,border = NA)
  if(length(exlb)!=0){
    arrows(rep(xmin,length(exlb)),nr-exlb+1,rep(xmin+1,length(exlb)),nr-exlb+1,angle=12, length=0.1,code=1,col=arrow.col)
  }
  if(length(exub)!=0){
    arrows(rep(xmax-1,length(exub)),nr-exub+1,rep(xmax,length(exub)),nr-exub+1,angle=12, length=0.1,code=2,col=arrow.col)
  }
  abline(v=est[nr+1],col="darkgray")
  abline(v=sfit[c("0.025quant","0.975quant")],col=shade.col,lty=2)
  axis(1,at=round(c(xmin,est[nr+1],xmax),2),labels=round(c(xmin,est[nr+1],xmax),2),cex.axis=axis.cex,...)

  if(ciFlag){
    plot.new()
    
    if(accuracy.type %in% c("dor","lrneg","lrpos")){
      strwidth_ci_main = strwidth(ci[1:nr],units="user",cex=cex,font=1,family="mono")
      strwidth_ci_sum = strwidth(ci[nr+1],units="user",cex=cex,font=2,family="mono")
    }else{
      strwidth_ci_main = strwidth(ci[1:nr],units="user",cex=cex,font=1,family="sans")
      strwidth_ci_sum = strwidth(ci[nr+1],units="user",cex=cex,font=2,family="sans")
    }
    strwidth_ci_title = strwidth("Estimate",units="user",cex=cex,font=2,family="sans")
    
    ci_width = max(strwidth_ci_main,strwidth_ci_title,strwidth_ci_sum)
    
    if(accuracy.type %in% c("dor","lrneg","lrpos")){
      family = "mono"
    }else{
      family = "sans"
    }
    ci_adj = switch(ciShow,left=0,right=1,center=0.5)
    xlim = switch(ciShow,left=c(0,ci_width),right=c(-ci_width,0),center=ci_width*c(-0.5,0.5))
    plot.window(xlim=xlim,ylim=c(0,nr+1),mar=c(4.5, 1.5, 3.5, 1.5),xlab="",ylab="",xaxs="i",family="sans",xaxt="n",xaxs="i",yaxt="n",bty="n")
    text(rep(0,nr),c(nr:1),ci[1:nr],adj=c(ci_adj,0.5),family=family,font=1,cex=cex)
    text(0,0,ci[nr+1],adj=c(ci_adj,0.5),family=family,font=2,cex=cex)
    text(0,nr+1,"Estimates",adj=c(ci_adj,0.5),family="sans",font=2,cex=cex)
  }
  mtext(main, adj=0.5, line=-2, cex = main.cex, outer=TRUE) 
  par(op)
  
}



