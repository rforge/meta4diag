library(shiny)
library(shinyBS)


shinyServer(function(input, output, session) {
  observe({
    if (input$stopButton > 0) stopApp()  # stop shiny
  })
  
  data(Telomerase, package = "meta4diag")
  data(Catheter, package = "meta4diag")
  data(Scheidler, package = "meta4diag")
  
  ###########################################################
  ###### Construct reactive numerical input with "observe"
  ###########################################################
  
  #---------------- Reset Inverse Wishart prior ---------------
  observe({
    input$reset_var1iw
    
    isolate({
      updateNumericInput(session, "var1IWnu", value = 4)
      updateNumericInput(session, "var1IWa", value = 1)
      updateNumericInput(session, "var1IWb", value = 1)
      updateNumericInput(session, "var1IWc", value = 0)
    })
  })
  #---------------- Var1 - PC prior ---------------
  observe({
    # reset
    input$reset_var1pc
    
    isolate({
      updateNumericInput(session, "var1PCu", value = 3)
      updateNumericInput(session, "var1PCalpha", value = 0.05)
    })
  })
  observe({
    # accept popup value
    input$acceptvar1pc
    
    isolate({
      updateNumericInput(session, "var1PCu", value = input$var1pc_slider_p1)
      updateNumericInput(session, "var1PCalpha", value = input$var1pc_slider_p2)
    })
  })
  observe({
    # give value to popup
    updateSliderInput(session, "var1pc_slider_p1", value = input$var1PCu)
    updateSliderInput(session, "var1pc_slider_p2", value = input$var1PCalpha)
  })
  #---------------- Var1 - Inverse-Gamma prior ---------------
  observe({
    # reset
    input$reset_var1ig
    
    isolate({
      updateNumericInput(session, "var1IGa", value = 0.25)
      updateNumericInput(session, "var1IGb", value = 0.025)
    })
  })
  observe({
    # accept popup value
    input$acceptvar1ig
    
    isolate({
      updateNumericInput(session, "var1IGa", value = input$var1ig_slider_p1)
      updateNumericInput(session, "var1IGb", value = input$var1ig_slider_p2)
    })
  })
  observe({
    # give value to popup
    updateSliderInput(session, "var1ig_slider_p1", value = input$var1IGa)
    updateSliderInput(session, "var1ig_slider_p2", value = input$var1IGb)
  })
  #---------------- Var1 - Half-cauchy prior ---------------
  observe({
    # reset
    input$reset_var1hc
    
    isolate({
      updateNumericInput(session, "var1HCa", value = 3)
    })
  })
  observe({
    # accept popup value
    input$acceptvar1hc
    
    isolate({
      updateNumericInput(session, "var1HCa", value = input$var1hc_slider_p1)
    })
  })
  observe({
    # give value to popup
    updateSliderInput(session, "var1hc_slider_p1", value = input$var1HCa)
  })
  #---------------- Var1 - T-Normal prior ---------------
  observe({
    # reset
    input$reset_var1tn
    
    isolate({
      updateNumericInput(session, "var1TNa", value = 0)
      updateNumericInput(session, "var1TNb", value = 1)
    })
  })
  observe({
    # accept popup value
    input$acceptvar1tn
    
    isolate({
      updateNumericInput(session, "var1TNa", value = input$var1tn_slider_p1)
      updateNumericInput(session, "var1TNb", value = input$var1tn_slider_p2)
    })
  })
  observe({
    # give value to popup
    updateSliderInput(session, "var1tn_slider_p1", value = input$var1TNa)
    updateSliderInput(session, "var1tn_slider_p2", value = input$var1TNb)
  })
  #---------------- Var2 - PC prior ---------------
  observe({
    # reset
    input$reset_var2pc
    
    isolate({
      updateNumericInput(session, "var2PCu", value = 3)
      updateNumericInput(session, "var2PCalpha", value = 0.05)
    })
  })
  observe({
    # accept popup value
    input$acceptvar2pc
    
    isolate({
      updateNumericInput(session, "var2PCu", value = input$var2pc_slider_p1)
      updateNumericInput(session, "var2PCalpha", value = input$var2pc_slider_p2)
    })
  })
  observe({
    # give value to popup
    updateSliderInput(session, "var2pc_slider_p1", value = input$var2PCu)
    updateSliderInput(session, "var2pc_slider_p2", value = input$var2PCalpha)
  })
  #---------------- Var2 - Inverse-Gamma prior ---------------
  observe({
    # reset
    input$reset_var2ig
    
    isolate({
      updateNumericInput(session, "var2IGa", value = 0.25)
      updateNumericInput(session, "var2IGb", value = 0.025)
    })
  })
  observe({
    # accept popup value
    input$acceptvar2ig
    
    isolate({
      updateNumericInput(session, "var2IGa", value = input$var2ig_slider_p1)
      updateNumericInput(session, "var2IGb", value = input$var2ig_slider_p2)
    })
  })
  observe({
    # give value to popup
    updateSliderInput(session, "var2ig_slider_p1", value = input$var2IGa)
    updateSliderInput(session, "var2ig_slider_p2", value = input$var2IGb)
  })
  #---------------- Var2 - Half-cauchy prior ---------------
  observe({
    # reset
    input$reset_var2hc
    
    isolate({
      updateNumericInput(session, "var2HCa", value = 3)
    })
  })
  observe({
    # accept popup value
    input$acceptvar2hc
    
    isolate({
      updateNumericInput(session, "var2HCa", value = input$var2hc_slider_p1)
    })
  })
  observe({
    # give value to popup
    updateSliderInput(session, "var2hc_slider_p1", value = input$var2HCa)
  })
  #---------------- Var2 - T-Normal prior ---------------
  observe({
    # reset
    input$reset_var2tn
    
    isolate({
      updateNumericInput(session, "var2TNa", value = 0.25)
      updateNumericInput(session, "var2TNb", value = 0.025)
    })
  })
  observe({
    # accept popup value
    input$acceptvar2tn
    
    isolate({
      updateNumericInput(session, "var2TNa", value = input$var2tn_slider_p1)
      updateNumericInput(session, "var2TNb", value = input$var2tn_slider_p2)
    })
  })
  observe({
    # give value to popup
    updateSliderInput(session, "var2tn_slider_p1", value = input$var2TNa)
    updateSliderInput(session, "var2tn_slider_p2", value = input$var2TNb)
  })
  #---------------- Cor - PC prior Strategy 1 ---------------
  observe({
    # reset
    input$reset_corpc1
    
    isolate({
      updateNumericInput(session, "corPC1rho0", value = -0.2)
      updateNumericInput(session, "corPC1omega", value = 0.6)
      updateNumericInput(session, "corPC1u", value = -0.95)
      updateNumericInput(session, "corPC1alpha", value = 0.05)
    })
  })
  observe({
    # accept popup value
    input$acceptcorpc1
    
    isolate({
      updateNumericInput(session, "corPC1rho0", value = input$corpc1_slider_rho0)
      updateNumericInput(session, "corPC1omega", value = input$corpc1_slider_p1)
      updateNumericInput(session, "corPC1u", value = input$corpc1_slider_p2)
      updateNumericInput(session, "corPC1alpha", value = input$corpc1_slider_p3)
    })
  })
  observe({
    # give value to popup
    updateSliderInput(session, "corpc1_slider_rho0", value = input$corPC1rho0)
    updateSliderInput(session, "corpc1_slider_p1", value = input$corPC1omega)
    updateSliderInput(session, "corpc1_slider_p2", value = input$corPC1u)
    updateSliderInput(session, "corpc1_slider_p3", value = input$corPC1alpha)
  })
  observe({
    updateNumericInput(session, "corPC1u", max = input$corPC1rho0-0.000001)
    updateNumericInput(session, "corPC1alpha", max = input$corPC1omega-0.000001)
    
    updateSliderInput(session, "corpc1_slider_p2", max = input$corpc1_slider_rho0-0.000001)
    updateSliderInput(session, "corpc1_slider_p3", max = input$corpc1_slider_p1-0.000001)
  })
  #---------------- Cor - PC prior Strategy 2 ---------------
  observe({
    # reset
    input$reset_corpc2
    
    isolate({
      updateNumericInput(session, "corPC2rho0", value = -0.2)
      updateNumericInput(session, "corPC2omega", value = 0.6)
      updateNumericInput(session, "corPC2u", value = 0.95)
      updateNumericInput(session, "corPC2alpha", value = 0.05)
    })
  })
  observe({
    # accept popup value
    input$acceptcorpc2
    
    isolate({
      updateNumericInput(session, "corPC2rho0", value = input$corpc2_slider_rho0)
      updateNumericInput(session, "corPC2omega", value = input$corpc2_slider_p1)
      updateNumericInput(session, "corPC2u", value = input$corpc2_slider_p2)
      updateNumericInput(session, "corPC2alpha", value = input$corpc2_slider_p3)
    })
  })
  observe({
    # give value to popup
    updateSliderInput(session, "corpc2_slider_rho0", value = input$corPC2rho0)
    updateSliderInput(session, "corpc2_slider_p1", value = input$corPC2omega)
    updateSliderInput(session, "corpc2_slider_p2", value = input$corPC2u)
    updateSliderInput(session, "corpc2_slider_p3", value = input$corPC2alpha)
  })
  observe({
    updateNumericInput(session, "corPC2u", min = input$corPC2rho0+0.000001)
    updateNumericInput(session, "corPC2alpha", max = 1-input$corPC2omega-0.000001)
    
    updateSliderInput(session, "corpc2_slider_p2", min = input$corpc2_slider_rho0+0.000001)
    updateSliderInput(session, "corpc2_slider_p3", max = 1-input$corpc2_slider_p1-0.000001)
  })
  #---------------- Cor - PC prior Strategy 3 ---------------
  observe({
    # reset
    input$reset_corpc3
    
    isolate({
      updateNumericInput(session, "corPC3rho0", value = -0.2)
      updateNumericInput(session, "corPC3u1", value = -0.95)
      updateNumericInput(session, "corPC3alpha1", value = 0.05)
      updateNumericInput(session, "corPC3u2", value = 0.95)
      updateNumericInput(session, "corPC3alpha2", value = 0.05)
    })
  })
  observe({
    # accept popup value
    input$acceptcorpc3
    
    isolate({
      updateNumericInput(session, "corPC3rho0", value = input$corpc3_slider_rho0)
      updateNumericInput(session, "corPC3u1", value = input$corpc3_slider_p1)
      updateNumericInput(session, "corPC3alpha1", value = input$corpc3_slider_p2)
      updateNumericInput(session, "corPC3u2", value = input$corpc3_slider_p3)
      updateNumericInput(session, "corPC3alpha2", value = input$corpc3_slider_p4)
    })
  })
  observe({
    # give value to popup
    updateSliderInput(session, "corpc3_slider_rho0", value = input$corPC3rho0)
    updateSliderInput(session, "corpc3_slider_p1", value = input$corPC3u1)
    updateSliderInput(session, "corpc3_slider_p2", value = input$corPC3alpha1)
    updateSliderInput(session, "corpc3_slider_p3", value = input$corPC3u2)
    updateSliderInput(session, "corpc3_slider_p4", value = input$corPC3alpha2)
  })
  observe({
    updateNumericInput(session, "corPC3u1", max = input$corPC3rho0-0.000001)
    updateNumericInput(session, "corPC3u2", min = input$corPC3rho0+0.000001)
    updateNumericInput(session, "corPC3alpha2", max = 1-input$corPC3alpha1-0.000001)
    
    updateNumericInput(session, "corpc3_slider_p1", max = input$corpc3_slider_rho0-0.000001)
    updateNumericInput(session, "corpc3_slider_p3", min = input$corpc3_slider_rho0+0.000001)
    updateNumericInput(session, "corpc3_slider_p4", max = 1-input$corpc3_slider_p2-0.000001)
  })
  #---------------- Cor - Normal prior  ---------------
  observe({
    # reset
    input$reset_cornormal
    
    isolate({
      updateNumericInput(session, "corNormalmu", value = 0)
      updateNumericInput(session, "corNormalvar", value = 5)
    })
  })
  observe({
    # accept popup value
    input$acceptcornormal
    
    isolate({
      updateNumericInput(session, "corNormalmu", value = input$cornormal_slider_p1)
      updateNumericInput(session, "corNormalvar", value = input$cornormal_slider_p2)
    })
  })
  observe({
    # give value to popup
    updateSliderInput(session, "cornormal_slider_p1", value = input$corNormalmu)
    updateSliderInput(session, "cornormal_slider_p2", value = input$corNormalvar)
  })
  #---------------- Cor - Beta prior  ---------------
  observe({
    # reset
    input$reset_corbeta
    
    isolate({
      updateNumericInput(session, "corBetaa", value = 0.5)
      updateNumericInput(session, "corBetab", value = 0.5)
    })
  })
  observe({
    # accept popup value
    input$acceptcornormal
    
    isolate({
      updateNumericInput(session, "corBetaa", value = input$corbeta_slider_p1)
      updateNumericInput(session, "corBetab", value = input$corbeta_slider_p2)
    })
  })
  observe({
    # give value to popup
    updateSliderInput(session, "corbeta_slider_p1", value = input$corBetaa)
    updateSliderInput(session, "corbeta_slider_p2", value = input$corBetab)
  })
  
  #--------- Construc reaction when press on the main Notepage ------------
  observe({
    main_select <- input$main
    if(main_select=="Fitted"){
      updateTabsetPanel(session, "sidebar", selected = "Fitted")
    }
    if(main_select=="SROC"){
      updateTabsetPanel(session, "sidebar", selected = "SROC")
    }
    if(main_select=="Forest"){
      updateTabsetPanel(session, "sidebar", selected = "Forest")
    }
    if(main_select=="Marginals"){
      updateTabsetPanel(session, "sidebar", selected = "Prior")
    }
    if(main_select=="Data"){
      updateTabsetPanel(session, "sidebar", selected = "Data")
    }
    if(main_select=="Estimates"){
      updateTabsetPanel(session, "sidebar", selected = "Model")
    }
  })
  
  #--------- update the marginals selector -------------
  observe({
    if(input$RunINLAButton > 0){
      res = inputmodel()
      fixed_name = rownames(res$summary.fixed)
      hyper_name = rownames(res$summary.hyperpar)
      choices = c(fixed_name, hyper_name)
      
      updateSelectInput(session, "marginalspar", choices = choices, selected = hyper_name[1])
    }
  })

  ###########################################################
  ###### Construct output prior plot
  ###########################################################
  #----------- prior: var1  ----------#
  output$var1plot_pc <- renderPlot({
    par(mar=c(4,4,2,2))
    meta4diag:::.priorSigmaPC(u = input$var1pc_slider_p1, alpha = input$var1pc_slider_p2, xmax = input$var1pc_slider_xlim)
  })
  output$var1plot_ig <- renderPlot({
    par(mar=c(4,4,2,2))
    meta4diag:::.priorInvgamma(a = input$var1ig_slider_p1, b = input$var1ig_slider_p2, xmax = input$var1ig_slider_xlim)
  })
  output$var1plot_hc <- renderPlot({
    par(mar=c(4,4,2,2))
    meta4diag:::.priorHalfCauchy(gamma=input$var1hc_slider_p1, xmax = input$var1hc_slider_xlim)
  })
  output$var1plot_tn <- renderPlot({
    par(mar=c(4,4,2,2))
    meta4diag:::.priorSigmaTnorm(m=input$var1tn_slider_p1, v=input$var1tn_slider_p2, xmax = input$var1tn_slider_xlim)
  })
  output$var1plot_unif <- renderPlot({
    par(mar=c(4,4,2,2))
    meta4diag:::.priorUniformSig(xmax = input$var1unif_slider_xlim)
  })
  #----------- prior: var2 ----------#
  output$var2plot_pc <- renderPlot({
    par(mar=c(4,4,2,2))
    meta4diag:::.priorSigmaPC(u = input$var2pc_slider_p1, alpha = input$var2pc_slider_p2, xmax = input$var2pc_slider_xlim)
  })
  output$var2plot_ig <- renderPlot({
    par(mar=c(4,4,2,2))
    meta4diag:::.priorInvgamma(a = input$var2ig_slider_p1, b = input$var2ig_slider_p2, xmax = input$var2ig_slider_xlim)
  })
  output$var2plot_hc <- renderPlot({
    par(mar=c(4,4,2,2))
    meta4diag:::.priorHalfCauchy(gamma=input$var2hc_slider_p1, xmax = input$var2hc_slider_xlim)
  })
  output$var2plot_tn <- renderPlot({
    par(mar=c(4,4,2,2))
    meta4diag:::.priorSigmaTnorm(m=input$var2tn_slider_p1, v=input$var2tn_slider_p2, xmax = input$var2tn_slider_xlim)
  })
  output$var2plot_unif <- renderPlot({
    par(mar=c(4,4,2,2))
    meta4diag:::.priorUniformSig(xmax = input$var2unif_slider_xlim)
  })
  #----------- prior: cor ----------#
  output$corplot_pc1 <- renderPlot({
    par(mar=c(4,4,2,2))
    meta4diag:::.priorExpRhoS1(rho.ref = input$corpc1_slider_rho0,
                   left.portion = input$corpc1_slider_p1,
                   Umin = input$corpc1_slider_p2,
                   alpha1 = input$corpc1_slider_p3)
  })
  output$corplot_pc2 <- renderPlot({
    par(mar=c(4,4,2,2))
    meta4diag:::.priorExpRhoS2(rho.ref = input$corpc2_slider_rho0,
                   left.portion = input$corpc2_slider_p1,
                   Umax = input$corpc2_slider_p2,
                   alpha2 = input$corpc2_slider_p3)
  })
  output$corplot_pc3 <- renderPlot({
    par(mar=c(4,4,2,2))
    meta4diag:::.priorExpRhoS3(rho.ref = input$corpc3_slider_rho0,
                   Umin = input$corpc3_slider_p1,
                   alpha1 = input$corpc3_slider_p2,
                   Umax = input$corpc3_slider_p3,
                   alpha2 = input$corpc3_slider_p4)
  })
  output$corplot_normal <- renderPlot({
    par(mar=c(4,4,2,2))
    meta4diag:::.priorRhoNormalPlot(mean=input$cornormal_slider_p1,variance=input$cornormal_slider_p2)
  })
  output$corplot_beta <- renderPlot({
    par(mar=c(4,4,2,2))
    meta4diag:::.priorRhoBetaPlot(a=input$corbeta_slider_p1,b=input$corbeta_slider_p2)
  })
  output$corplot_unif <- renderPlot({
    par(mar=c(4,4,2,2))
    x = seq(-1, 1, by=0.1)
    y = rep(0.5, length(x))
    plot(x,y, type="l", xlab=expression(rho), ylab=expression(pi(rho)))
  })
    
  ###########################################################
  ###### Construct data reaction
  ###########################################################
  
  #----------- data file ----------#
  inputDataFile <- reactive({
    inFile <- input$datafile
    if (is.null(inFile)){
      inputData = switch(input$choosedata, "telomerase"=Telomerase, "catheter"=Catheter, "scheidler"=Scheidler)
    }else{
      fullname = inFile$name
      filepath = inFile$datapath
      
      fullname = unlist(strsplit(basename(fullname), "[.]"))
      filename = fullname[1]
      fileform = fullname[2]
      
      form = tolower(fileform)
      if(form == "txt"){
        inputData <- read.table(filepath, header=TRUE)
      }
      if(form == "rdata" || form == "rda"){
        df = load(filepath)
        df = get(df)
        inputData <- df
      }
      if(form == "csv"){
        inputData <- read.csv(filepath, header = TRUE)
      }
    }
    return(inputData)
  })
  
  #----------- data info -----------#
  inputDataInfo <- reactive({
    if(is.null(input$datafile)){
      info = "Data is valid!"
    }else{
      df = inputDataFile()
      if(is.null(colnames(df))){
        info = "Please give the names to indicate \"TP\", \"FN\", \"FP\", \"TN\"!"
      }
      datanames = tolower(colnames(df))
      colnames(df) = datanames
      I = dim(df)[1]
      fic = c("tp","tn","fp","fn") # four important components
      is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
      if(!("studynames" %in% datanames)){
        studynames = paste("study[",c(1:I),"]",sep="")
        df = data.frame(studynames=studynames, df)
      }
      if(all(fic %in% datanames)){
        tf.integer = apply(cbind(df$tp,df$tn,df$fp,df$fn),2,function(x) all(is.wholenumber(x)))
        tf.pos = apply(cbind(df$tp,df$tn,df$fp,df$fn)>=0,2,function(x) all(x))
        if(all(tf.integer)){
          if(all(tf.pos)){
            info = "Data is valid!"
          }else{
            fvp = paste("data$",fic[tf.pos==FALSE],sep="",collapse=" and ")
            info = paste(fvp," has some negative value!!!",sep="")
          }
        }else{
          fvi = paste("data$",fic[tf.integer==FALSE],sep="",collapse=" and ")
          info = paste(fvi," has some non-integer value!!!",sep="")
        }
      }else{
        ffic = toupper(paste(fic[!(fic %in% datanames)],collapse=" "))
        info = paste("Data is not valid!",ffic,"are missing!!!",sep=" ")
      }
    }
    return(info)
  })
  
  #------- data other variable -------#
  inputDataCM <- reactive({
    if(is.null(input$datafile)){
      pt_variables = switch(input$choosedata, 
                            "telomerase"="None", 
                            "catheter"= c("None", "type", "prevalence"), 
                            "scheidler"= c("None", "modality"))
    }else{
      if(inputDataInfo()=="Data is valid!"){
        df = inputDataFile()
        original_datanames = colnames(df)
        datanames = tolower(original_datanames)
        if(dim(df)[2]>5){
          pt_variables = original_datanames[-which(datanames %in% c("studynames","tp","tn","fp","fn"))]
          pt_variables = c("None", pt_variables)
        }else{
          pt_variables = "None"
        }
      }else{
        pt_variables = "None"
      }
    }
    return(pt_variables)
  })

  ###########################################################
  ###### Construct prior reaction
  ###########################################################
  #----------- prior: var1 ----------#
  inputPriorVar1 <- reactive({
    value = list()
    var1PriorName = input$var1PriorName
    if(var1PriorName=="var1pc"){
      var.prior = "PC"
      u = input$var1PCu
      alpha = input$var1PCalpha
      if(is.na(u) || is.na(alpha) || u < 0.0001 || alpha < 0.00005 || alpha > 0.99995){
        var.flag = FALSE
      }else{
        var.flag = TRUE
      }
      var.par = c(u, alpha)
    }else if(var1PriorName=="var1ig"){
      var.prior = "invgamma"
      a = input$var1IGa
      b = input$var1IGb
      if(is.na(a) || is.na(b) ||a < 0.005 || b < 0.005){
        var.flag = FALSE
      }else{
        var.flag = TRUE
      }
      var.par = c(a, b)
    }else if(var1PriorName=="var1hc"){
      var.prior = "hcauchy"
      a = input$var1HCa
      if(is.na(a) ||a < 0.0005){
        var.flag = FALSE
      }else{
        var.flag = TRUE
      }
      var.par = c(a)
    }else if(var1PriorName=="var1tn"){
      var.prior = "tnormal"
      a = input$var1TNa
      b = input$var1TNb
      if(is.na(a) || is.na(b) || b < 0.005){
        var.flag = FALSE
      }else{
        var.flag = TRUE
      }
      var.par = c(a, b)
    }else if(var1PriorName=="var1unif"){
      var.prior = "unif"
      var.par = c()
      var.flag = TRUE
    }else{
      var.prior = "invwishart"
      nu = input$var1IWnu
      a = input$var1IWa
      b = input$var1IWb
      c = input$var1IWc
      if(is.na(a) || is.na(b) || is.na(c) || is.na(nu) ||nu < 3 || a < 0.01 || b < 0.01){
        var.flag = FALSE
      }else{
        var.flag = TRUE
      }
      var.par = c(nu, a, b, c)
    }
    value$var.prior = var.prior
    value$var.par = var.par
    value$var.flag = var.flag
    return(value)
  })
  
  #----------- prior: var2 ----------#
  inputPriorVar2 <- reactive({
    value = list()
    var2PriorName = input$var2PriorName
    if(var2PriorName=="var2pc"){
      var2.prior = "PC"
      var.prior = "PC"
      u = input$var2PCu
      alpha = input$var2PCalpha
      if(is.na(u) || is.na(alpha) || u < 0.0001 || alpha < 0.005 || alpha > 0.995){
        var2.flag = FALSE
      }else{
        var2.flag = TRUE
      }
      var2.par = c(u, alpha)
    }else if(var2PriorName=="var2ig"){
      var2.prior = "invgamma"
      a = input$var2IGa
      b = input$var2IGb
      if(is.na(a) || is.na(b) || a < 0.005 || b < 0.005){
        var2.flag = FALSE
      }else{
        var2.flag = TRUE
      }
      var2.par = c(a, b)
    }else if(var2PriorName=="var2hc"){
      var2.prior = "hcauchy"
      a = input$var2HCa
      if(is.na(a) || a < 0.0005){
        var2.flag = FALSE
      }else{
        var2.flag = TRUE
      }
      var2.par = c(a)
    }else if(var2PriorName=="var2tn"){
      var2.prior = "tnormal"
      a = input$var2TNa
      b = input$var2TNb
      if(is.na(a) || is.na(b) || b < 0.005){
        var2.flag = FALSE
      }else{
        var2.flag = TRUE
      }
      var2.par = c(a, b)
    }else{
      var2.prior = "unif"
      var2.par = c()
      var2.flag = TRUE
    }
    value$var.prior = var2.prior
    value$var.par = var2.par
    value$var.flag = var2.flag
    return(value)
  })
  
  #----------- prior: cor ----------#
  inputPriorCor <- reactive({
    value = list()
    corPriorName = input$corPriorName
    if(corPriorName=="cor-prior-name-pc"){
      cor.prior = "PC"
      strategy = input$strategy
      if(strategy=="strategy1"){
        rho0 = input$corPC1rho0
        omega = input$corPC1omega
        u = input$corPC1u
        alpha = input$corPC1alpha
        if(is.na(rho0) || is.na(omega) || is.na(u) || is.na(alpha) || rho0 < -0.95 || rho0 > 0.95 || omega < 0.01 || omega > 0.99 || u < -0.99 || u > (rho0-0.000001) || alpha < 0.001 || alpha > (omega - 0.001)){
          var.flag = FALSE
        }else{
          var.flag = TRUE
        }
        cor.par = c(1, rho0, omega, u, alpha, NA, NA)
      }else if(strategy=="strategy2"){
        rho0 = input$corPC2rho0
        omega = input$corPC2omega
        u = input$corPC2u
        alpha = input$corPC2alpha
        if(is.na(rho0) || is.na(omega) || is.na(u) || is.na(alpha) || rho0 < -0.95 || rho0 > 0.95 || omega < 0.01 || omega > 0.99 || u > 0.99 || u < (rho0+0.000001) || alpha < 0.001 || alpha > (1-omega-0.001)){
          var.flag = FALSE
        }else{
          var.flag = TRUE
        }
        cor.par = c(2, rho0, omega, NA, NA, u, alpha)
      }else{
        rho0 = input$corPC3rho0
        u1 = input$corPC3u1
        alpha1 = input$corPC3alpha1
        u2 = input$corPC3u2
        alpha2 = input$corPC3alpha2
        if(is.na(rho0) || is.na(u1) || is.na(u2) || is.na(alpha1) || is.na(alpha2) || rho0 < -0.95 || rho0 > 0.95 || u1 < -0.99 || u1 > (rho0-0.000001) || u2 > 0.99 || u2 < (rho0+0.000001) || alpha1 < 0.001 || alpha1 > 0.99 || alpha2 < 0.001 || alpha2 > (1-alpha1-0.0001)){
          var.flag = FALSE
        }else{
          var.flag = TRUE
        }
        cor.par = c(3, rho0, NA, u1, alpha1, u2, alpha2)
      }
    }else if(corPriorName=="cor-prior-name-normal"){
      cor.prior = "normal"
      mu = input$corNormalmu
      var = input$corNormalvar
      if(is.na(mu) || is.na(var) || var < 0.001){
        var.flag = FALSE
      }else{
        var.flag = TRUE
      }
      cor.par = c(mu, var)
    }else if(corPriorName=="cor-prior-name-beta"){
      cor.prior = "beta"
      a = input$corBetaa
      b = input$corBetab
      if(is.na(a) || is.na(b) || a < 0.01 || b < 0.01){
        var.flag = FALSE
      }else{
        var.flag = TRUE
      }
      cor.par = c(a, b)
    }else{
      cor.prior = "unif"
      cor.par = c()
      var.flag = TRUE
    }
    value$var.prior = cor.prior
    value$var.par = cor.par
    value$var.flag = var.flag
    return(value)
  })
  
  #----------- make prior -----------#
  inputPrior <- reactive({
    var1 = inputPriorVar1()
    var2 = inputPriorVar2()
    cor = inputPriorCor()
    if(!var1$var.flag || !var2$var.flag || !cor$var.flag){
      return(NULL)
    }else{
      if(var1$var.prior=="invwishart"){
        outpriors = makePriors(var.prior = "invwishart",
                               wishart.par = var1$var.par,
                               init = c(0.01, 0.01, -0.1))
      }else{
        outpriors = makePriors(var.prior = var1$var.prior, var2.prior=var2$var.prior, cor.prior = cor$var.prior,
                               var.par = var1$var.par, var2.par=var2$var.par, cor.par = cor$var.par,
                               init = c(0.01, 0.01, -0.1))
      }
      return(outpriors)
    }
  })
  
  ###########################################################
  ###### Construct sidebar Data needed UI
  ###########################################################
  ##------- Sidebar Data Modality ---------#
  output$sidebardatamodality <- renderUI({
    data = inputDataFile()
    pt_names = inputDataCM()
    if(length(pt_names)>1){
      new_pt_names = pt_names[pt_names!="None"]
      class_pt_names = unlist(lapply(1:length(new_pt_names), function(x) class(data[,new_pt_names[x]])))
      if(any(class_pt_names=="factor")){
        modality = new_pt_names[class_pt_names=="factor"]
        radioButtons("datamodality", label = NULL, choices = c("None",modality),selected = "None")
      }else{
        radioButtons("datamodality", label = NULL, choices = "None",selected = "None")
      }
    }else{
      radioButtons("datamodality", label = NULL, choices = pt_names, selected=pt_names)
    }
  })
  
  output$sidebarpartialdata <- renderUI({
    data = inputDataFile()
    choose_modality = input$datamodality
    if(!is.null(choose_modality)){
      if(choose_modality != "None"){
        if(choose_modality %in% colnames(data)){
          modality_type = class(data[,choose_modality])
          if(modality_type=="factor"){
            partial_data = levels(data[,choose_modality])
            checkboxGroupInput("partialdata", label=NULL, choices = partial_data, selected = partial_data)
          }else{
            p("Not valid modality variable.")
          }
        }else{return()}
      }else{return()}
    }else{return()}
  })
  ##------- Sidebar Data Covariance ---------#
  output$sidebardatacovar <- renderUI({
    pt_names = inputDataCM()
    if(length(pt_names)==1){
      p("No available covariates for this dataset.")
    }else{
      ptco_names = pt_names[-which("None"==pt_names)]
      modality = input$datamodality
      if(length(ptco_names)==0){
        p("No available covariates for this dataset.")
      }else{
        if(modality=="None"){
          checkboxGroupInput("datacovar", label=NULL, choices = ptco_names)
        }else{
          pt_covar_names = ptco_names[-which(modality==ptco_names)]
          if(length(pt_covar_names)==0){
            p("No available covariates for this dataset.")
          }else{
            checkboxGroupInput("datacovar", label=NULL, choices = pt_covar_names)
          }
        }
      }
    }
  })
  ##------- Sidebar Data Info ---------#
  output$sidebardatainfo  = renderText({
    inputDataInfo()
  })
  
  ###########################################################
  ###### Construct INLA version
  ###########################################################
  
  #----------- INLA Version ---------------#
  output$inlaversion  = renderPrint({
    cat(input$var1PCu)
    #   cat(length(input$partialdata))
    #   cat("\n")
        # cat(input$modelnos)
    #     cat("\n")
    #     cat(class(input$datamodality))
    #     cat("\n")
    #     cat(input$partialdata)
    #     cat("\n")
    #     cat(class(input$partialdata))
    #     cat("\n")
    cat(paste("INLA version.........: ", inla.version("version"),sep=""))
    cat("\n")
    cat(paste("INLA date............: ", inla.version("date"),sep=""))
  })
  output$inlaversionOut = renderUI({
    verbatimTextOutput("inlaversion")
  })
  
  ###########################################################
  ###### Construct model reaction
  ###########################################################
  #----------- make data -----------#
  inputdata <- eventReactive(input$RunINLAButton, {
    info = inputDataInfo()
    
    model.type = switch(input$modeltype, "type1" = 1, "type2" = 2, "type3" = 3, "type4" = 4)
    modality = input$datamodality
    if(modality=="None"){modality = NULL}
    partial = input$partialdata
    c(paste(partial,collapse=","))
    covariates = input$datacovar
    
    if(info=="Data is valid!"){
      
      data = inputDataFile()
      if(!is.null(modality)){
        if(!is.null(partial)){
          idx = rep(FALSE, length(data[modality]))
          for(nam in partial)
            idx = idx | (data[modality] == nam)
          outdata = makeData(data = data[idx,], 
                             model.type=model.type, modality=modality, covariates=covariates)
        }else{
          outdata = makeData(data = data, 
                             model.type=model.type, modality=modality, covariates=covariates)
        }
      }else{
        outdata = makeData(data = data, 
                           model.type=model.type, modality=modality, covariates=covariates)
      }
      return(outdata)
    }else{
      return(NULL)
    }
  })
  #----------- model --------------#
  inputmodel <- eventReactive(input$RunINLAButton, {
    data = inputdata()
    if(is.null(data)){
      return(NULL)
    }else{
      priors = inputPrior()
      if(is.null(priors)){
        return(NULL)
      }else{
        model.type = switch(input$modeltype, "type1" = 1, "type2" = 2, "type3" = 3, "type4" = 4)
        link = input$modellink
        quantiles = as.numeric(input$modellevel)
        verbose = input$modelverbose
        if(verbose=="On"){
          model = runModel(outdata=data, outpriors = priors, link=link, quantiles=quantiles, verbose=TRUE)
        }else{
          model = runModel(outdata=data, outpriors = priors, link=link, quantiles=quantiles, verbose=FALSE)
        }
        if(model$ok){
          object = makeObject(model=model, nsample=input$modelnos)
          return(object)
        }else{
          return(NULL)
        }
      }
    }
  })
  
  ###########################################################
  ###### Construct Main Rcode needed UI
  ###########################################################
  rcodeReact <- eventReactive(input$RunINLAButton, {
    data = inputdata()
    if(is.null(data)){
      return(NULL)
    }else{
      model = inputmodel()
      if(is.null(model)){
        return(NULL)
      }else{
        var1 = inputPriorVar1()
        var2 = inputPriorVar2()
        cor = inputPriorCor()
        cat("library(INLA)")
        cat("\n")
        cat("library(meta4diag)")
        cat("\n")
        cat("\n")
        inFile <- input$datafile
        if (is.null(inFile)){
          names = input$choosedata
          cat(paste("data(",names,")",sep=""))
          cat("\n")
          cat(paste("data = ",names,sep=""))
        }else{
          fullname = inFile$name
          splitname = unlist(strsplit(basename(fullname), "[.]"))
          filename = splitname[1]
          fileform = splitname[2]
          
          fileform = tolower(fileform)
          if(fileform=="txt"){
            cat(paste("data = read.table(...your path.../",fullname,")",sep=""))
          }
          if(fileform=="csv"){
            cat(paste("read.csv(...your path.../",fullname,", header = TRUE)",sep=""))
          }
          if(fileform == "rdata" || fileform == "rda"){
            cat(paste("load(...your path.../",fullname,")",sep=""))
          }
          cat("\n")
          cat(paste("data = ",filename,sep=""))
        }
        
        cat("\n")
        cat("\n")
        if(var1$var.prior=="Invwishart"){
          cat(paste("outpriors = makePriors(var.prior = \"",var1$var.prior,"\", 
                    wishart.par = c(",paste(var1$var.par,collapse = ", "),"),  
                    init = c(0.01, 0.01, -0.1))",sep=""))
        }else{
          cat(paste("outpriors = makePriors(var.prior = \"",var1$var.prior,"\", 
                    var2.prior = \"",var2$var.prior,"\",
                    cor.prior = \"",cor$var.prior,"\",
                    var.par = c(",paste(var1$var.par,collapse = ", "),"),  
                    var2.par = c(",paste(var2$var.par,collapse = ", "),"), 
                    cor.par = c(",paste(cor$var.par,collapse = ", "),"),
                    init = c(0.01, 0.01, -0.1))",sep=""))
        }
        cat("\n")
        cat("\n")
        modality = input$datamodality
        partial = input$partialdata
        covar = input$datacovar
        model.type = as.numeric(switch(input$modeltype, "type1" = 1, "type2" = 2, "type3" = 3, "type4" = 4))
        if(modality != "None"){
          if(!is.null(partial)){
            if(is.null(covar)){
              cat(paste("outdata = makeData(data = data[which(data[, \"",modality,"\"] %in% c(\"",paste(partial,collapse="\",\""),"\")),], model.type = ",model.type,", modality = \"",modality,"\", covariates = NULL)",sep=""))
            }else{
              cat(paste("outdata = makeData(data = data[which(data[, \"",modality,"\"] %in% c(\"",paste(partial,collapse="\",\""),"\")),], model.type = ",model.type,", modality = \"",modality,"\", covariates = \"",covar,"\")",sep=""))
            }
          }else{
            if(is.null(covar)){
              cat(paste("outdata = makeData(data = data, model.type = ",model.type,", modality = NULL, covariates = NULL)",sep=""))
            }else{
              cat(paste("outdata = makeData(data = data, model.type = ",model.type,", modality = NULL, covariates = \"",covar,"\")",sep=""))
            }
          }
        }else{
          if(is.null(covar)){
            cat(paste("outdata = makeData(data = data, model.type = ",model.type,", modality = NULL, covariates = NULL)",sep=""))
          }else{
            cat(paste("outdata = makeData(data = data, model.type = ",model.type,", modality = NULL, covariates = \"",covar,"\")",sep=""))
          }
        }
        cat("\n")
        cat("\n")
        link = input$modellink
        quantiles = input$modellevel
        if(is.null(quantiles)){
          quantiles = c(0.025, 0.5, 0.975)
        }
        verbose = input$modelverbose
        if(verbose=="On"){
          cat(paste("model = runModel(outdata, outpriors, link=\"",link, "\", quantiles=c(",paste(quantiles,collapse=","), "), verbose=TRUE)",sep=""))
        }else{
          cat(paste("model = runModel(outdata, outpriors, link=\"",link, "\", quantiles=c(",paste(quantiles,collapse=","), "), verbose=FALSE)",sep=""))
        }
        cat("\n")
        cat("\n")
        cat(paste("res = makeObject(outdata = outdata, outprior = outpriors, model = model, nsample = ",input$modelnos,")"))
      }
    }
  })
  output$Rcode <- renderPrint({
    rcodeReact()
  })
  output$RcodeOut = renderUI({
    if(input$RunINLAButton == 0){
      p("Please press the RunINLA button to check the result.")
    }else{
      data = inputdata()
      if(is.null(data)){
        p("Data is not valid. Please check data.")
      }else{
        model = inputmodel()
        if(is.null(model)){
          priors = inputPrior()
          if(is.null(priors)){
            var1 = inputPriorVar1()
            var2 = inputPriorVar2()
            cor = inputPriorCor()
            if(!var1$var.flag){
              p("Invalid hyper parameter(s) of the prior for the first variance is given!")
            }else{
              if(!var2$var.flag){
                p("Invalid hyper parameter(s) of the prior for the second variance is given!")
              }else{
                if(!cor$var.flag){
                  p("Invalid hyper parameter(s) of the prior for the correlation is given!")
                }
              }
            }
          }else{
            p("Model is not valid. Please set verbose to TRUE to check the internal information. 
              The information will be shown in the R Console.")
          }
          }else{
            verbatimTextOutput("Rcode")
        } 
      }
      }
    
  })
  
  ###########################################################
  ###### Construct Main Data needed UI
  ###########################################################
  output$inputdataTable <- renderTable({
      inputDataFile()
  }) #*********
  output$modelRunningOut <- renderPrint({
    if(input$RunINLAButton > 0){
      data = inputdata()
      if(is.null(data)){
        p("Invalid data! Please check data!")
      }else{
        res = inputmodel()
        if(is.null(res)){
          priors = inputPrior()
          if(is.null(priors)){
            var1 = inputPriorVar1()
            var2 = inputPriorVar2()
            cor = inputPriorCor()
            if(!var1$var.flag){
              p("Invalid hyper parameter(s) of the prior for the first variance is given!")
            }else{
              if(!var2$var.flag){
                p("Invalid hyper parameter(s) of the prior for the second variance is given!")
              }else{
                if(!cor$var.flag){
                  p("Invalid hyper parameter(s) of the prior for the correlation is given!")
                }
              }
            }
          }else{
            p("Model is not ok! Please set verbose to TRUE in Model Control Panel to check! The result is shown in R console. :-(")
          }
        }else{
          p("Model is ready! :-)")
        }
      }
    }else{
      p("Model is not running yet. Please press the RunINLA button to get estimates and results!")
    }
  })
  ###########################################################
  ###### Construct Main Estimates needed UI
  ###########################################################
  estimatesReact <- eventReactive(input$RunINLAButton, {
    data = inputdata()
    if(is.null(data)){
      return(NULL)
    }else{
      model = inputmodel()
      if(is.null(model)){
        return(NULL)
      }else{
        a = summary(model)
        return(a)
      }
    }
  })
  output$estimates <- renderPrint({
    estimatesReact()
  })
  output$estimatesOut <- renderUI({
    if(input$RunINLAButton == 0){
      p("Please press the RunINLA button to check the result.")
    }else{
      data = inputdata()
      if(is.null(data)){
        p("Data is not valid. Please check data.")
      }else{
        model = inputmodel()
        if(is.null(model)){
          priors = inputPrior()
          if(is.null(priors)){
            var1 = inputPriorVar1()
            var2 = inputPriorVar2()
            cor = inputPriorCor()
            if(!var1$var.flag){
              p("Invalid hyper parameter(s) of the prior for the first variance is given!")
            }else{
              if(!var2$var.flag){
                p("Invalid hyper parameter(s) of the prior for the second variance is given!")
              }else{
                if(!cor$var.flag){
                  p("Invalid hyper parameter(s) of the prior for the correlation is given!")
                }
              }
            }
          }else{
            p("Model is not valid. Please set verbose to TRUE to check the internal information. 
              The information will be shown in the R Console.")
          }
        }else{
          verbatimTextOutput("estimates")
        } 
      }
    }
  })
  ###########################################################
  ###### Construct Main Marginals needed UI
  ###########################################################
  marginalsReact <- eventReactive(input$RunINLAButton, {
    model = inputmodel()
    if(is.null(model)){
      return()
    }else{
      fixed_names = rownames(model$summary.fixed)
      hyperpar_names = rownames(model$summary.hyperppar)
      num_fixed = length(fixed_names)
      num_row = ceiling(0.5*num_fixed) + 2
      par(mfrow=c(num_row,2))
      for(i in 1:num_fixed){
        plot(INLA::inla.smarginal(model$marginals.fixed[[i]]), type="l", xlab=fixed_names[i], ylab="")
      }
      if(num_fixed %% 2==1){
        pot.new()
      }
      plot(model, var.type="var1")
      plot(model, var.type="var2")
      plot(model, var.type="rho")
      plot(-10,-10, xlim=c(0,1),ylim=c(0,1), xaxt = "n", yaxt="n", bty="n",xlab="",ylab="")
      legend("center",legend=c("prior", "posterior"),lty=c(1,2),col=c("black","gray"), bty="n")
    }
  })
  output$marginals <- renderPlot({
    marginalsReact()
  })
  output$marginalsOut <- renderUI({
    if(input$RunINLAButton == 0){
      p("Please press the RunINLA button to check the result.")
    }else{
      data = inputdata()
      if(is.null(data)){
        p("Data is not valid. Please check data.")
      }else{
        model = inputmodel()
        if(is.null(model)){
          priors = inputPrior()
          if(is.null(priors)){
            var1 = inputPriorVar1()
            var2 = inputPriorVar2()
            cor = inputPriorCor()
            if(!var1$var.flag){
              p("Invalid hyper parameter(s) of the prior for the first variance is given!")
            }else{
              if(!var2$var.flag){
                p("Invalid hyper parameter(s) of the prior for the second variance is given!")
              }else{
                if(!cor$var.flag){
                  p("Invalid hyper parameter(s) of the prior for the correlation is given!")
                }
              }
            }
          }else{
            p("Model is not valid. Please set verbose to TRUE to check the internal information. 
              The information will be shown in the R Console.")
          }
          }else{
            plotOutput("marginals", width="80%",height = paste(input$fheightsize,"px",sep=""))
        } 
      }
    }
  })
  
  ###########################################################
  ###### Construct Main SROC needed UI
  ###########################################################
  output$SROC <- renderPlot({
    data = inputdata()
    if(is.null(data)){
      return(NULL)
    }else{
      model = inputmodel()
      if(is.null(model)){
        return(NULL)
      }else{
        number.partial = length(input$partialdata)
        
        est.type = input$sptype
        sp.cex = input$spsize
        sp.pch = switch(input$spsymbol, "whitesquare"=0, "whitecircle"=1, "whitediamond"=5,
                        "whitetriangleup"=2,"whitetriangledown"=6,"blacksquare"=15,
                        "blackcircle"=16,"blackdiamond"=18,"blacktriangleup"=17,"plus"=3,
                        "cross"=4,"squareplus"=12,"circleplus"=10,"squarecross"=7,"circlecross"=13,
                        "star"=8,"starfive"="*","twotriangles"=11,"dot"=20)
        sp.col = paste("#",input$spcolor,sep="")
        if(number.partial>1){
          angles = 360/number.partial
          given_sp_rgb = col2rgb(sp.col)
          given_sp_hsl = meta4diag:::.rgb_to_hsl(given_sp_rgb[1],given_sp_rgb[2],given_sp_rgb[3])
          h = given_sp_hsl[1]
          s = given_sp_hsl[2]
          l = given_sp_hsl[3]
          sp_h_list = h + angles*(0:number.partial)
          sp_col_list = unlist(lapply(1:number.partial, function(x) meta4diag:::.hsl_to_rgb(sp_h_list[x], s, l)))
          sp.col = sp_col_list
        }
        
        dataShow = switch(input$dptype, "original"="o", "fitted"="f", "none"="n")
        data.col = paste("#",input$dpcolor,sep="")
        data.cex = input$dpsizeind
        if(data.cex=="fixed"){data.cex=input$dpsize}
        data.pch = switch(input$dpsymbol, "whitesquare"=0, "whitecircle"=1, "whitediamond"=5,
                          "whitetriangleup"=2,"whitetriangledown"=6,"blacksquare"=15,
                          "blackcircle"=16,"blackdiamond"=18,"blacktriangleup"=17,"plus"=3,
                          "cross"=4,"squareplus"=12,"circleplus"=10,"squarecross"=7,"circlecross"=13,
                          "star"=8,"starfive"="*","twotriangles"=11,"dot"=20)
        lineShow = switch(input$srocshow, "show"=T, "notshow"=F)
        sroc.type = switch(input$srocfunction, "srocf1"=1, "srocf2"=2, "srocf3"=3, "srocf4"=4, "srocf5"=5)
        line.lty = switch(input$sroclt, "lty1"=1, "lty2"=2, "lty3"=3, "lty4"=4, "lty5"=5, "lty6"=6)
        line.lwd = input$srocwidth
        line.col = paste("#",input$sroccolor,sep="")
        if(number.partial>1){
          angles = 360/number.partial
          given_line_rgb = col2rgb(line.col)
          given_line_hsl = meta4diag:::.rgb_to_hsl(given_line_rgb[1],given_line_rgb[2],given_line_rgb[3])
          h = given_line_hsl[1]
          s = given_line_hsl[2]
          l = given_line_hsl[3]
          line_h_list = h + angles*(0:number.partial)
          line_col_list = unlist(lapply(1:number.partial, function(x) meta4diag:::.hsl_to_rgb(line_h_list[x], s, l)))
          line.col = line_col_list
        }
        crShow = switch(input$crshow, "show"=T, "notshow"=F)
        cr.lty = switch(input$crlt, "lty1"=1, "lty2"=2, "lty3"=3, "lty4"=4, "lty5"=5, "lty6"=6)
        cr.lwd = input$crwidth
        cr.col = paste("#",input$crcolor,sep="")
        if(number.partial>1){
          angles = 360/number.partial
          given_cr_rgb = col2rgb(cr.col)
          given_cr_hsl = meta4diag:::.rgb_to_hsl(given_cr_rgb[1],given_cr_rgb[2],given_cr_rgb[3])
          h = given_cr_hsl[1]
          s = given_cr_hsl[2]
          l = given_cr_hsl[3]
          cr_h_list = h + angles*(0:number.partial)
          cr_col_list = unlist(lapply(1:number.partial, function(x) meta4diag:::.hsl_to_rgb(cr_h_list[x], s, l)))
          cr.col = cr_col_list
        }
        prShow = switch(input$prshow, "show"=T, "notshow"=F)
        pr.lty = switch(input$prlt, "lty1"=1, "lty2"=2, "lty3"=3, "lty4"=4, "lty5"=5, "lty6"=6)
        pr.lwd = input$prwidth
        pr.col = paste("#",input$prcolor,sep="")
        if(number.partial>1){
          angles = 360/number.partial
          given_pr_rgb = col2rgb(pr.col)
          given_pr_hsl = meta4diag:::.rgb_to_hsl(given_pr_rgb[1],given_pr_rgb[2],given_pr_rgb[3])
          h = given_pr_hsl[1]
          s = given_pr_hsl[2]
          l = given_pr_hsl[3]
          pr_h_list = h + angles*(0:number.partial)
          pr_col_list = unlist(lapply(1:number.partial, function(x) meta4diag:::.hsl_to_rgb(pr_h_list[x], s, l)))
          pr.col = pr_col_list
        }
        main = as.character(input$SROC_title)
        cex.main = input$SROC_cex_main
        cex.axis = input$SROC_cex_axis
        cex.lab = input$SROC_cex_lab
        legend = switch(input$sroclegendshow, "left"="left", "right"="right", "FALSE"="no","bottom"="bottom")
        legend.cex = input$SROC_cex_legend
        if(legend=="no"){
          SROC(model, est.type=est.type, sp.cex=sp.cex,sp.pch=sp.pch,sp.col=sp.col,
               dataShow=dataShow, data.col=data.col, data.cex=data.cex, data.pch=data.pch, 
               lineShow=lineShow, sroc.type=sroc.type, line.lty=line.lty, line.lwd=line.lwd, line.col=line.col,
               crShow=crShow, cr.lty=cr.lty, cr.lwd=cr.lwd, cr.col=cr.col,
               prShow=prShow, pr.lty=pr.lty, pr.lwd=pr.lwd,  pr.col=pr.col,
               dataFit = T, 
               main=main, cex.main=cex.main, cex.axis=cex.axis,
               cex.lab=cex.lab)
        }else{
          SROC(model, est.type=est.type, sp.cex=sp.cex,sp.pch=sp.pch,sp.col=sp.col,
               dataShow=dataShow, data.col=data.col, data.cex=data.cex, data.pch=data.pch, 
               lineShow=lineShow, sroc.type=sroc.type, line.lty=line.lty, line.lwd=line.lwd, line.col=line.col,
               crShow=crShow, cr.lty=cr.lty, cr.lwd=cr.lwd, cr.col=cr.col,
               prShow=prShow, pr.lty=pr.lty, pr.lwd=pr.lwd,  pr.col=pr.col,
               dataFit = T, legend=legend, legend.cex=legend.cex,
               main=main, cex.main=cex.main, cex.axis=cex.axis,
               cex.lab=cex.lab)
        }
      }
    }
  })
  output$SROCOut <- renderUI({
    if(input$RunINLAButton == 0){
      p("Please press the RunINLA button to check the result.")
    }else{
      data = inputdata()
      if(is.null(data)){
        p("Data is not valid. Please check data.")
      }else{
        model = inputmodel()
        if(is.null(model)){
          priors = inputPrior()
          if(is.null(priors)){
            var1 = inputPriorVar1()
            var2 = inputPriorVar2()
            cor = inputPriorCor()
            if(!var1$var.flag){
              p("Invalid hyper parameter(s) of the prior for the first variance is given!")
            }else{
              if(!var2$var.flag){
                p("Invalid hyper parameter(s) of the prior for the second variance is given!")
              }else{
                if(!cor$var.flag){
                  p("Invalid hyper parameter(s) of the prior for the correlation is given!")
                }
              }
            }
          }else{
            p("Model is not valid. Please set verbose to TRUE to check the internal information. 
              The information will be shown in the R Console.")
          }
          }else{
            plotOutput("SROC")
        } 
      }
    }
  })
  
  ###########################################################
  ###### Construct AUC needed UI
  ###########################################################
  output$AUC <- renderPrint({
    data = inputdata()
    if(is.null(data)){
      return(NULL)
    }else{
      model = inputmodel()
      if(is.null(model)){
        return(NULL)
      }else{
        sroc.type = switch(input$srocfunction, "srocf1"=1, "srocf2"=2, "srocf3"=3, "srocf4"=4, "srocf5"=5)
        est.type = input$sptype
        a = AUC(model,sroc.type=sroc.type,est.type=est.type)
        return(round(a,2))
      }
    }
  })
  output$AUCOut <- renderUI({
    if(input$RunINLAButton == 0){
      p("Please press the RunINLA button to check the result.")
    }else{
      data = inputdata()
      if(is.null(data)){
        p("Data is not valid. Please check data.")
      }else{
        model = inputmodel()
        if(is.null(model)){
          priors = inputPrior()
          if(is.null(priors)){
            var1 = inputPriorVar1()
            var2 = inputPriorVar2()
            cor = inputPriorCor()
            if(!var1$var.flag){
              p("Invalid hyper parameter(s) of the prior for the first variance is given!")
            }else{
              if(!var2$var.flag){
                p("Invalid hyper parameter(s) of the prior for the second variance is given!")
              }else{
                if(!cor$var.flag){
                  p("Invalid hyper parameter(s) of the prior for the correlation is given!")
                }
              }
            }
          }else{
            p("Model is not valid. Please set verbose to TRUE to check the internal information. 
              The information will be shown in the R Console.")
          }
          }else{
            verbatimTextOutput("AUC")
        } 
      }
    }
  })
  ###########################################################
  ###### Construct Main Forest needed UI
  ###########################################################
  output$Forest <- renderPlot({
    data = inputdata()
    if(is.null(data)){
      return(NULL)
    }else{
      model = inputmodel()
      if(is.null(model)){
        return(NULL)
      }else{
        accuracy.type = input$accutype
        est.type = input$esttype
        nameShow = input$snshow
        if(nameShow=="no"){nameShow = FALSE}
        dataShow = input$odshow
        if(dataShow=="no"){dataShow = FALSE}
        ciShow = input$cishow
        if(ciShow=="no"){ciShow = FALSE}
        p.pch = switch(input$fpsymbol, "whitesquare"=0, "whitecircle"=1, "whitediamond"=5,
                       "whitetriangleup"=2,"whitetriangledown"=6,"blacksquare"=15,
                       "blackcircle"=16,"blackdiamond"=18,"blacktriangleup"=17,"plus"=3,
                       "cross"=4,"squareplus"=12,"circleplus"=10,"squarecross"=7,"circlecross"=13,
                       "star"=8,"starfive"="*","twotriangles"=11,"dot"=20)
        p.cex = input$fpsizeind
        if(p.cex=="fixed"){p.cex = input$fpsize}
        p.col = paste("#", input$fpcolor, sep="")
        text.cex = input$ftextsize
        shade.col = paste("#", input$shadecolor, sep="")
        arrow.col = paste("#", input$arrowcolor, sep="")
        arrow.lty = switch(input$arrowlt, "lty1"=1, "lty2"=2, "lty3"=3, "lty4"=4, "lty5"=5, "lty6"=6)
        arrow.lwd = input$arrowwidth
        forest(model, accuracy.type=accuracy.type, est.type=est.type, 
               p.cex=p.cex, p.pch=p.pch, p.col=p.col,
               nameShow=nameShow, dataShow=dataShow, estShow=ciShow, text.cex=text.cex,
               shade.col=shade.col, arrow.col=arrow.col, arrow.lty=arrow.lty, arrow.lwd=arrow.lwd,
               intervals=c(0.025,0.975),
               main="Forest plot", main.cex=1.5, axis.cex=1)
      }
    }
  })
  output$ForestOut <- renderUI({
    if(input$RunINLAButton == 0){
      p("Please press the RunINLA button to check the result.")
    }else{
      data = inputdata()
      if(is.null(data)){
        p("Data is not valid. Please check data.")
      }else{
        model = inputmodel()
        if(is.null(model)){
          priors = inputPrior()
          if(is.null(priors)){
            var1 = inputPriorVar1()
            var2 = inputPriorVar2()
            cor = inputPriorCor()
            if(!var1$var.flag){
              p("Invalid hyper parameter(s) of the prior for the first variance is given!")
            }else{
              if(!var2$var.flag){
                p("Invalid hyper parameter(s) of the prior for the second variance is given!")
              }else{
                if(!cor$var.flag){
                  p("Invalid hyper parameter(s) of the prior for the correlation is given!")
                }
              }
            }
          }else{
            p("Model is not valid. Please set verbose to TRUE to check the internal information. 
              The information will be shown in the R Console.")
          }
          }else{
            plotOutput("Forest", width="100%",height = paste(input$fheightsize,"px",sep=""))
        } 
      }
    }
  })
  
  ###########################################################
  ###### Construct Main Crosshair needed UI
  ###########################################################
  output$Crosshair <- renderPlot({
    data = inputdata()
    if(is.null(data)){
      return(NULL)
    }else{
      model = inputmodel()
      if(is.null(model)){
        return(NULL)
      }else{
        est.type = input$sptype
        crosshair(model,est.type=est.type)
      }
    }
  })
  output$CrossOut <- renderUI({
    if(input$RunINLAButton == 0){
      p("Please press the RunINLA button to check the result.")
    }else{
      data = inputdata()
      if(is.null(data)){
        p("Data is not valid. Please check data.")
      }else{
        model = inputmodel()
        if(is.null(model)){
          priors = inputPrior()
          if(is.null(priors)){
            var1 = inputPriorVar1()
            var2 = inputPriorVar2()
            cor = inputPriorCor()
            if(!var1$var.flag){
              p("Invalid hyper parameter(s) of the prior for the first variance is given!")
            }else{
              if(!var2$var.flag){
                p("Invalid hyper parameter(s) of the prior for the second variance is given!")
              }else{
                if(!cor$var.flag){
                  p("Invalid hyper parameter(s) of the prior for the correlation is given!")
                }
              }
            }
          }else{
            p("Model is not valid. Please set verbose to TRUE to check the internal information. 
              The information will be shown in the R Console.")
          }
          }else{
            plotOutput("Crosshair", height="400px", width = "400px")
        } 
      }
    }
  })
  
  ###########################################################
  ###### Construct Main Fitted needed UI
  ###########################################################
  output$Fitted <- renderPrint({
    data = inputdata()
    if(is.null(data)){
      return(NULL)
    }else{
      model = inputmodel()
      if(is.null(model)){
        return(NULL)
      }else{
        a = fitted(model, accuracy.type = input$fitaccutype)
        print(a)
      }
    }
  })
  output$FittedOut <- renderUI({
    if(input$RunINLAButton == 0){
      p("Please press the RunINLA button to check the result.")
    }else{
      data = inputdata()
      if(is.null(data)){
        p("Data is not valid. Please check data.")
      }else{
        model = inputmodel()
        if(is.null(model)){
          priors = inputPrior()
          if(is.null(priors)){
            var1 = inputPriorVar1()
            var2 = inputPriorVar2()
            cor = inputPriorCor()
            if(!var1$var.flag){
              p("Invalid hyper parameter(s) of the prior for the first variance is given!")
            }else{
              if(!var2$var.flag){
                p("Invalid hyper parameter(s) of the prior for the second variance is given!")
              }else{
                if(!cor$var.flag){
                  p("Invalid hyper parameter(s) of the prior for the correlation is given!")
                }
              }
            }
          }else{
            p("Model is not valid. Please set verbose to TRUE to check the internal information. 
              The information will be shown in the R Console.")
          }
          }else{
            verbatimTextOutput("Fitted")
        } 
      }
    }
  })
  
  ###########################################################
  ###### Construct Save Model
  ###########################################################
  output$saveModel <- downloadHandler(
    filename = function() {
      paste('model-', Sys.Date(), '.Rdata', sep='')
    },
    content = function(file) {
      if(input$RunINLAButton == 0){
        model = NULL
        save(model,file=file)
      }else{
        data = inputdata()
        if(is.null(data)){
          model = NULL
          save(model,file=file)
        }else{
          model = inputmodel()
          save(model,file=file)
        }
      }
    }
  )
  
  ###########################################################
  ###### Construct Save SROC
  ###########################################################
  output$SaveSROC <- downloadHandler(
    filename = function() {
      paste('SROC-', Sys.Date(), '.',input$srocform, sep='')
      },
    content = function(file) {
      fileform = input$srocform
      width = input$SROC_width
      height = input$SROC_height
      if(fileform=="eps"){
        setEPS()
        postscript(file, width=width, height=height)
      }else if(fileform=="pdf"){
        pdf(file, width=width, height=height)
      }else if(fileform=="jpg"){
        jpeg(file, width = width, height = height, units = "in", res=300)
      }else{
        png(file, width = width, height = height, units = "in", res=300)
      }
      if(input$RunINLAButton == 0){
        plot.new()
      }else{
        data = inputdata()
        if(is.null(data)){
          plot.new()
        }else{
          model = inputmodel()
          if(is.null(model)){
            plot.new()
          }else{
            number.partial = length(input$partialdata)
            
            est.type = input$sptype
            sp.cex = input$spsize
            sp.pch = switch(input$spsymbol, "whitesquare"=0, "whitecircle"=1, "whitediamond"=5,
                            "whitetriangleup"=2,"whitetriangledown"=6,"blacksquare"=15,
                            "blackcircle"=16,"blackdiamond"=18,"blacktriangleup"=17,"plus"=3,
                            "cross"=4,"squareplus"=12,"circleplus"=10,"squarecross"=7,"circlecross"=13,
                            "star"=8,"starfive"="*","twotriangles"=11,"dot"=20)
            sp.col = paste("#",input$spcolor,sep="")
            if(number.partial>1){
              angles = 360/number.partial
              given_sp_rgb = col2rgb(sp.col)
              given_sp_hsl = meta4diag:::.rgb_to_hsl(given_sp_rgb[1],given_sp_rgb[2],given_sp_rgb[3])
              h = given_sp_hsl[1]
              s = given_sp_hsl[2]
              l = given_sp_hsl[3]
              sp_h_list = h + angles*(0:number.partial)
              sp_col_list = unlist(lapply(1:number.partial, function(x) meta4diag:::.hsl_to_rgb(sp_h_list[x], s, l)))
              sp.col = sp_col_list
            }
            dataShow = switch(input$dptype, "original"="o", "fitted"="f", "none"="n")
            data.col = paste("#",input$dpcolor,sep="")
            data.cex = input$dpsizeind
            if(data.cex=="fixed"){data.cex=input$dpsize}
            data.pch = switch(input$dpsymbol, "whitesquare"=0, "whitecircle"=1, "whitediamond"=5,
                              "whitetriangleup"=2,"whitetriangledown"=6,"blacksquare"=15,
                              "blackcircle"=16,"blackdiamond"=18,"blacktriangleup"=17,"plus"=3,
                              "cross"=4,"squareplus"=12,"circleplus"=10,"squarecross"=7,"circlecross"=13,
                              "star"=8,"starfive"="*","twotriangles"=11,"dot"=20)
            lineShow = switch(input$srocshow, "show"=T, "notshow"=F)
            sroc.type = switch(input$srocfunction, "srocf1"=1, "srocf2"=2, "srocf3"=3, "srocf4"=4, "srocf5"=5)
            line.lty = switch(input$sroclt, "lty1"=1, "lty2"=2, "lty3"=3, "lty4"=4, "lty5"=5, "lty6"=6)
            line.lwd = input$srocwidth
            line.col = paste("#",input$sroccolor,sep="")
            if(number.partial>1){
              angles = 360/number.partial
              given_line_rgb = col2rgb(line.col)
              given_line_hsl = meta4diag:::.rgb_to_hsl(given_line_rgb[1],given_line_rgb[2],given_line_rgb[3])
              h = given_line_hsl[1]
              s = given_line_hsl[2]
              l = given_line_hsl[3]
              line_h_list = h + angles*(0:number.partial)
              line_col_list = unlist(lapply(1:number.partial, function(x) meta4diag:::.hsl_to_rgb(line_h_list[x], s, l)))
              line.col = line_col_list
            }
            crShow = switch(input$crshow, "show"=T, "notshow"=F)
            cr.lty = switch(input$crlt, "lty1"=1, "lty2"=2, "lty3"=3, "lty4"=4, "lty5"=5, "lty6"=6)
            cr.lwd = input$crwidth
            cr.col = paste("#",input$crcolor,sep="")
            if(number.partial>1){
              angles = 360/number.partial
              given_cr_rgb = col2rgb(cr.col)
              given_cr_hsl = meta4diag:::.rgb_to_hsl(given_cr_rgb[1],given_cr_rgb[2],given_cr_rgb[3])
              h = given_cr_hsl[1]
              s = given_cr_hsl[2]
              l = given_cr_hsl[3]
              cr_h_list = h + angles*(0:number.partial)
              cr_col_list = unlist(lapply(1:number.partial, function(x) meta4diag:::.hsl_to_rgb(cr_h_list[x], s, l)))
              cr.col = cr_col_list
            }
            prShow = switch(input$prshow, "show"=T, "notshow"=F)
            pr.lty = switch(input$prlt, "lty1"=1, "lty2"=2, "lty3"=3, "lty4"=4, "lty5"=5, "lty6"=6)
            pr.lwd = input$prwidth
            pr.col = paste("#",input$prcolor,sep="")
            if(number.partial>1){
              angles = 360/number.partial
              given_pr_rgb = col2rgb(pr.col)
              given_pr_hsl = meta4diag:::.rgb_to_hsl(given_pr_rgb[1],given_pr_rgb[2],given_pr_rgb[3])
              h = given_pr_hsl[1]
              s = given_pr_hsl[2]
              l = given_pr_hsl[3]
              pr_h_list = h + angles*(0:number.partial)
              pr_col_list = unlist(lapply(1:number.partial, function(x) meta4diag:::.hsl_to_rgb(pr_h_list[x], s, l)))
              pr.col = pr_col_list
            }
            main = input$SROC_title
            cex.main = input$SROC_cex_main
            cex.axis = input$SROC_cex_axis
            cex.lab = input$SROC_cex_lab
            legend = switch(input$sroclegendshow, "left"="left", "right"="right", "FALSE"="no","bottom"="bottom")
            legend.cex = input$SROC_cex_legend
            if(legend=="no"){
              SROC(model, est.type=est.type, sp.cex=sp.cex,sp.pch=sp.pch,sp.col=sp.col,
                   dataShow=dataShow, data.col=data.col, data.cex=data.cex, data.pch=data.pch, 
                   lineShow=lineShow, sroc.type=sroc.type, line.lty=line.lty, line.lwd=line.lwd, line.col=line.col,
                   crShow=crShow, cr.lty=cr.lty, cr.lwd=cr.lwd, cr.col=cr.col,
                   prShow=prShow, pr.lty=pr.lty, pr.lwd=pr.lwd,  pr.col=pr.col,
                   dataFit = T, 
                   main=main, cex.main=cex.main, cex.axis=cex.axis,
                   cex.lab=cex.lab)
            }else{
              SROC(model, est.type=est.type, sp.cex=sp.cex,sp.pch=sp.pch,sp.col=sp.col,
                   dataShow=dataShow, data.col=data.col, data.cex=data.cex, data.pch=data.pch, 
                   lineShow=lineShow, sroc.type=sroc.type, line.lty=line.lty, line.lwd=line.lwd, line.col=line.col,
                   crShow=crShow, cr.lty=cr.lty, cr.lwd=cr.lwd, cr.col=cr.col,
                   prShow=prShow, pr.lty=pr.lty, pr.lwd=pr.lwd,  pr.col=pr.col,
                   dataFit = T, legend=legend, legend.cex=legend.cex,
                   main=main, cex.main=cex.main, cex.axis=cex.axis,
                   cex.lab=cex.lab)
            }
          }
        }
      }
      dev.off()
    }
  )
  
  ###########################################################
  ###### Construct Save Crosshair
  ###########################################################
  output$SaveCross <- downloadHandler(
    filename = function() {
      paste('Crosshair-', Sys.Date(), '.',input$crossform, sep='')
    },
    content = function(file) {
      fileform = input$crossform
      width = input$Cross_width
      height = input$Cross_height
      if(fileform=="eps"){
        setEPS()
        postscript(file, width=width, height=height)
      }else if(fileform=="pdf"){
        pdf(file, width=width, height=height)
      }else if(fileform=="jpg"){
        jpeg(file, width = width, height = height, units = "in", res=300)
      }else{
        png(file, width = width, height = height, units = "in", res=300)
      }
      if(input$RunINLAButton == 0){
        plot.new()
      }else{
        data = inputdata()
        if(is.null(data)){
          plot.new()
        }else{
          model = inputmodel()
          if(is.null(model)){
            plot.new()
          }else{
            est.type = input$sptype
            
            crosshair(model,est.type=est.type)
          }
        }
      }
      dev.off()
    }
  )
  
  ###########################################################
  ###### Construct Save Forest
  ###########################################################
  output$SaveForest <- downloadHandler(
    filename = function() {
      paste('Forest-', Sys.Date(), '.',input$forestform, sep='')
    },
    content = function(file) {
        fileform = input$forestform
        width = input$Forest_width
        height = input$Forest_height
        if(fileform=="eps"){
          setEPS()
          postscript(file, width=width, height=height)
        }else if(fileform=="pdf"){
          pdf(file, width=width, height=height)
        }else if(fileform=="jpg"){
          jpeg(file, width = width, height = height, units = "in", res=300)
        }else{
          png(file, width = width, height = height, units = "in", res=300)
        }
        if(input$RunINLAButton == 0){
          plot.new()
        }else{
          data = inputdata()
          if(is.null(data)){
            plot.new()
          }else{
            model = inputmodel()
            if(is.null(model)){
              plot.new()
            }else{
              accuracy.type = input$accutype
              est.type = input$esttype
              nameShow = input$snshow
              if(nameShow=="no"){nameShow = FALSE}
              dataShow = input$odshow
              if(dataShow=="no"){dataShow = FALSE}
              ciShow = input$cishow
              if(ciShow=="no"){ciShow = FALSE}
              p.pch = switch(input$fpsymbol, "whitesquare"=0, "whitecircle"=1, "whitediamond"=5,
                             "whitetriangleup"=2,"whitetriangledown"=6,"blacksquare"=15,
                             "blackcircle"=16,"blackdiamond"=18,"blacktriangleup"=17,"plus"=3,
                             "cross"=4,"squareplus"=12,"circleplus"=10,"squarecross"=7,"circlecross"=13,
                             "star"=8,"starfive"="*","twotriangles"=11,"dot"=20)
              p.cex = input$fpsizeind
              if(p.cex=="fixed"){p.cex = input$fpsize}
              p.col = paste("#", input$fpcolor, sep="")
              text.cex = input$ftextsize
              shade.col = paste("#", input$shadecolor, sep="")
              arrow.col = paste("#", input$arrowcolor, sep="")
              arrow.lty = switch(input$arrowlt, "lty1"=1, "lty2"=2, "lty3"=3, "lty4"=4, "lty5"=5, "lty6"=6)
              arrow.lwd = input$arrowwidth
              forest(model, accuracy.type=accuracy.type, est.type=est.type, 
                     p.cex=p.cex, p.pch=p.pch, p.col=p.col,
                     nameShow=nameShow, dataShow=dataShow, estShow=ciShow, text.cex=text.cex,
                     shade.col=shade.col, arrow.col=arrow.col, arrow.lty=arrow.lty, arrow.lwd=arrow.lwd,
                     intervals=c(0.025,0.975),
                     main="Forest plot", main.cex=1.5, axis.cex=1)
            }
          }
        }
        dev.off()
    }
  )
  ###########################################################
  ###### Construct Save Estimates
  ###########################################################
  output$saveEstimates <- downloadHandler(
    filename = function() {
      paste('Estimates-', Sys.Date(), '.txt', sep='')
    },
    content = function(file) {
      if(input$RunINLAButton == 0){
        model = NULL
      }else{
        data = inputdata()
        if(is.null(data)){
          model = NULL
        }else{
          model = inputmodel()
        }
      }
      a = summary(model)
      sink(file)
      print(a)
      sink()
    }
  )
  ###########################################################
  ###### Construct Save Marginals
  ###########################################################
  output$SaveMarginals <- downloadHandler(
    filename = function() {
      paste(input$marginalspar,'-', Sys.Date(), '.', input$marginalsform, sep='')
    },
    content = function(file) {
      fileform = input$marginalsform
      width = input$marginals_width
      height = input$marginals_height
      if(fileform=="eps"){
        setEPS()
        postscript(file, width=width, height=height)
      }else if(fileform=="pdf"){
        pdf(file, width=width, height=height)
      }else if(fileform=="jpg"){
        jpeg(file, width = width, height = height, units = "in", res=300)
      }else{
        png(file, width = width, height = height, units = "in", res=300)
      }
      if(input$RunINLAButton == 0){
        plot.new()
      }else{
        data = inputdata()
        if(is.null(data)){
          plot.new()
        }else{
          model = inputmodel()
          if(is.null(model)){
            plot.new()
          }else{
            ind = input$marginalspar
            if(ind != "all"){
              plot(model, var.type=input$marginalspar)
            }else{
              fixed_names = rownames(model$summary.fixed)
              hyperpar_names = rownames(model$summary.hyperppar)
              num_fixed = length(fixed_names)
              num_row = ceiling(0.5*num_fixed) + 2
              par(mfrow=c(num_row,2))
              for(i in 1:num_fixed){
                plot(INLA::inla.smarginal(model$marginals.fixed[[i]]), type="l", xlab=fixed_names[i], ylab="")
              }
              if(num_fixed %% 2==1){
                pot.new()
              }
              plot(model, var.type="var1")
              plot(model, var.type="var2")
              plot(model, var.type="rho")
              plot(-10,-10, xlim=c(0,1),ylim=c(0,1), xaxt = "n", yaxt="n", bty="n",xlab="",ylab="")
              legend("center",legend=c("prior", "posterior"),lty=c(1,2),col=c("black","gray"), bty="n")
            }
          }
        }
      }
      dev.off()
    }
  )
#   ###########################################################
#   ###### Construct Save Priors
#   ###########################################################
})
  
  
  