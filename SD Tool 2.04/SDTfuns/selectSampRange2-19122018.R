# SD Tool v.3 and WKBIOPTIM SimTool v.1  were taken into account for the developement of the present tool.
# SD tool has been developed in MARE/2014/19 project and updated in MARE/2016/22 project (STREAM)

# The tool is documented in Deliverable D3.3 "Upgrade the methodological framework and tools for sampling optimization, implement and report case studies" (January 2019)

# Authors of the first version of SD Tool: F. Gontrand, T. Rouyer, N. Billet, 2016
# IFREMER, UMR MARBEC, Avenue Jean Monnet, BP 171, 34203 S` ete, France 

# Authors of the first version of WKBIOPTIM scripts: Nuno Prista (SLU, Sweden) from a subgroup work carried out during a local Workshop on Sampling Design and Optimization (Lysekil, 31 October to 4 November 2016, unpublished)
# SLU Aqua, Institute of Marine Research, Department of Acquatic Resources - Swedish University of Agricultural Sciences

# Authors of this new tool: M.T. Facchini, I. Bitetto, 2017
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 

# In case of use of the tool, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail addresses: facchini@coispa.it, bitetto@coispa.it
# SD Tool is believed to be reliable. However, we disclaim any implied warranty or representation 
# about its accuracy, completeness or appropriateness for any particular purpose.

selectSampRange2 = function(indStr, availStrat, cvFunS2) {
  
  if (FALSE) {
    indStr = 1
    cvFunS2 =  resOptCV_final
  }
  
  
  cvFun0 <- cvFunS2[as.character(cvFunS2$Var1) == as.character(availStrat$Var1[indStr]) & 
                                   as.character(cvFunS2$Var2) == as.character(availStrat$Var2[indStr]) & 
                                   as.character(cvFunS2$Var3) == as.character(availStrat$Var3[indStr]), ]
  
  if (!all(is.na(cvFun0$cv))) {
    #cvFun0 = cvFunS2[[indStr]]
    
    # removing rows with cv = NA
    cvFun = cvFun0[!is.na(cvFun0$cv),]
    
    # ordering
    cvFunOrder = cvFun[order(cvFun$sampSize),]
    
    # removing outliers
    cvMAXi = which.max(cvFunOrder$cv)
    
    if(cvMAXi != 1){
      indOutliers =  unlist(lapply(1 : (cvMAXi - 1), function(i){
        diffCV = abs((cvFunOrder$cv[i] - cvFunOrder$cv[cvMAXi]) / cvFunOrder$cv[cvMAXi])
        if(diffCV > 0.5){
          return(i)
        }
      }))
      if(!is.null(indOutliers)){
        cvFunOrder = cvFunOrder[- indOutliers,]
      }
    }
    
    # format
    dat = data.frame(nMeas= cvFunOrder$sampSize, cv = cvFunOrder$cv)
    colnames(dat) = c("x","y")
    
    # find local minima
    d<-try(density(dat$y,bw="sj"), silent = TRUE)
    
    min_accepted_sample_size <- as.numeric(as.character(table_min_accepted_sample_size$min_accepted_ss[as.character(table_min_accepted_sample_size$space) == as.character(availStrat$Var1[indStr]) &
                                                                                                         as.character(table_min_accepted_sample_size$time) == as.character(availStrat$Var2[indStr]) & 
                                                                                                         as.character(table_min_accepted_sample_size$technical) == as.character(availStrat$Var3[indStr]) ]) )
    
    if(class(d) != "try-error") {
      
      loc.max <- d$x[localMaxima(d$y)]*max(dat$x)
      
      SOLs <- data.frame(Var1=NA, Var2=NA, Var3= NA, solutions = c(round(loc.max), round(loc.max)[length(round(loc.max))] ),  maxCV= NA, minCV=NA, maxRR= NA, minRR=NA, meanRR=NA, noIterations=NA)
      
      for (rig in 1:(nrow(SOLs)) ) {
       if (rig == 1) {
         dat_this_rig = dat[dat$x <= as.numeric(as.character(SOLs$solutions[rig])),]
         cvFun0_this_rig = cvFun0[cvFun0$sampSize <= as.numeric(as.character(SOLs$solutions[rig])),]
         # print(paste( "minore = di ", as.numeric(as.character(SOLs$solutions[rig]) )) )
       } else if (rig == (nrow(SOLs)) ) {
         dat_this_rig = dat[dat$x > as.numeric(as.character(SOLs$solutions[rig-1] )),]
         cvFun0_this_rig = cvFun0[cvFun0$sampSize  > as.numeric(as.character(SOLs$solutions[rig-1] )),]
         # print(paste( "maggiore di ", as.numeric(as.character(SOLs$solutions[rig-1]))  ))
       } else {
         dat_this_rig = dat[dat$x > as.numeric(as.character(SOLs$solutions[rig-1])) & dat$x <= as.numeric(as.character(SOLs$solutions[rig])),]
         cvFun0_this_rig = cvFun0[cvFun0$sampSize  > as.numeric(as.character(SOLs$solutions[rig-1])) & cvFun0$sampSize <= as.numeric(as.character(SOLs$solutions[rig])),]
         # print(paste( "compreso tra ", as.numeric(as.character(SOLs$solutions[rig-1])), "e" , as.numeric(as.character(SOLs$solutions[rig])) ))
       }
        SOLs$minCV[rig] <- round(min(dat_this_rig$y), 2)
        SOLs$maxCV[rig] <- round(max(dat_this_rig$y), 2)
        SOLs$minRR[rig] <- min(cvFun0_this_rig$recyclingRate)
        SOLs$maxRR[rig] <- max(cvFun0_this_rig$recyclingRate)
        SOLs$meanRR[rig] <- round(mean(cvFun0_this_rig$recyclingRate),2)
        SOLs$noIterations[rig] <- nrow(cvFun0_this_rig)
      }
      
      accepted_solutions <- which(SOLs$solutions >= min_accepted_sample_size)
      
      file_name <- paste( res_dir, "/",RS_shortcode, " - CV Density function ", 
                          cvFun0$Var1[1], "-",cvFun0$Var2[1], "-", cvFun0$Var3[1] , ".png", sep="") 
      
      png(file=file_name, width=21, height=21, bg="white", units="cm",res=200)
      # 
      pp <- plot(d, main="Density plot of the CV values")
      abline(v=d$x[localMaxima(d$y)])
      try(abline(v=d$x[localMaxima(d$y)][accepted_solutions[1]], col="red" )) 
      try(abline(v=d$x[localMaxima(d$y)][accepted_solutions[2]], col="red" ) )
      try(abline(v=d$x[localMaxima(d$y)][accepted_solutions[3]], col="red" ) )
      try(abline(v=d$x[localMaxima(d$y)][accepted_solutions[4]], col="red" )) 
      
      text(d$x[localMaxima(d$y)], 0, round(loc.max))
      text(c(d$x[localMaxima(d$y)], max(d$x)), c(d$y[localMaxima(d$y)]+0.1, min(d$y)+0.1), paste(SOLs$maxCV, "-" , SOLs$minCV, sep=""), cex = 0.7)
      
      print(pp)
      
      dev.off()
      
      SOLs$solutions[nrow(SOLs)] <- paste(">",  SOLs$solutions[nrow(SOLs)], sep="")
      
      if (nrow(SOLs[accepted_solutions,]) <= 4 ) {
        result  = SOLs[accepted_solutions,]
      } else {
        result  = SOLs[accepted_solutions[1:4],]
      }
      
    } else {
      
      result <- data.frame(Var1=NA, Var2=NA, Var3= NA, solutions = NA,  maxCV= NA, minCV=NA)
      
    }
    
  } else {
    
    result = data.frame(Var1=NA, Var2=NA, Var3= NA, solutions = NA,  maxCV= NA, minCV=NA)
  }
  
  result$Var1 = availStrat$Var1[indStr]
    result$Var2 = availStrat$Var2[indStr]
    result$Var3 = availStrat$Var3[indStr]

    return(result)
}
