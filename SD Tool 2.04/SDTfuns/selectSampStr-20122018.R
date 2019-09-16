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

selectSampStr = function(indStr, availStrat, availSamp, sampSizeRange){
                                      # indStr, availStrat, availSamp, sampSizeRange
  case = availStrat[indStr,1:3]
  
   # subset by area 
switch(col_ele_var1,
       
    GSA={ sampPoolStr1 = data.frame(sampPoolStr = unique(availSamp$trpCode[which(availSamp$area == case$Var1)] ) , group1 = rep(1, length(unique(availSamp$trpCode[which(availSamp$area == case$Var1)] )) )) },
    Country={ sampPoolStr1 = data.frame(sampPoolStr = unique(availSamp$trpCode[which(availSamp$vslFlgCtry == case$Var1)]  ) , group1 = rep(1, length(unique(availSamp$trpCode[which(availSamp$vslFlgCtry == case$Var1)]  ) ) ) )},
	  ALL = { sampPoolStr1 = data.frame(sampPoolStr = unique(availSamp$trpCode  ), group1 = rep(1, length(unique(availSamp$trpCode  )) ) )},
    stop("Enter something that switches me!") )


switch(col_ele_var2,
       
    Q = {  sampPoolStr2 = data.frame(sampPoolStr = unique(availSamp$trpCode[which(availSamp$SampQuarter == case$Var2)] ), group2 = rep(1, length(unique(availSamp$trpCode[which(availSamp$SampQuarter == case$Var2)] ))) )},
    S = { sampPoolStr2 = data.frame(sampPoolStr = unique(availSamp$trpCode[which(availSamp$SampSemester == case$Var2)] ), group2 = rep(1, length(unique(availSamp$trpCode[which(availSamp$SampSemester == case$Var2)] ))))},
    Y = { sampPoolStr2 = data.frame(sampPoolStr = unique(availSamp$trpCode  ), group2 = rep(1, length(unique(availSamp$trpCode  )) ) )},
    stop("Enter something that switches me!") )


switch(col_ele_var3, 
       
    lev6 = { sampPoolStr3 = data.frame(sampPoolStr = unique(availSamp$trpCode[which(availSamp$foCatEu6 == case$Var3)] ), group3 = rep(1, length(unique(availSamp$trpCode[which(availSamp$foCatEu6 == case$Var3)] )) ) )},
    lev4 = { sampPoolStr3 = data.frame(sampPoolStr = unique(availSamp$trpCode[which(availSamp$foCatEu5 == case$Var3)] ), group3 = rep(1, length(unique(availSamp$trpCode[which(availSamp$foCatEu5 == case$Var3)] ) )) )},
    NONE = { sampPoolStr3 = data.frame(sampPoolStr = unique(availSamp$trpCode ), group3  = rep(1, length(unique(availSamp$trpCode ) )) )},
    stop("Enter something that switches me!") )

  sampPoolStr =  merge(sampPoolStr1, merge(sampPoolStr2, sampPoolStr3, all=TRUE), all=TRUE)
  
  sampPoolStr$sum_groups <- rowSums(sampPoolStr[, 2:4], na.rm=TRUE)
  
  sampPoolStr <- as.character(sampPoolStr$ sampPoolStr[sampPoolStr$sum_groups ==3] )

  minSize = sampSizeRange$solutions[as.character(sampSizeRange$Var1) == as.character(case$Var1[1]) &
                                      as.character(sampSizeRange$Var2) == as.character(case$Var2[1]) &
                                      as.character(sampSizeRange$Var3) == as.character(case$Var3[1]) &
                                      sampSizeRange$min_max == "min"]
  maxSize = sampSizeRange$solutions[as.character(sampSizeRange$Var1) == as.character(case$Var1[1]) &
                                      as.character(sampSizeRange$Var2) == as.character(case$Var2[1]) &
                                      as.character(sampSizeRange$Var3) == as.character(case$Var3[1]) &
                                      sampSizeRange$min_max == "max"]
  
  if(!is.na(minSize) & !is.na(maxSize)) {
    
    if(maxSize == minSize & minSize != 0){maxSize = maxSize + 1}
    
    sampSize = sample(minSize : maxSize, 1, replace = FALSE)

    selectSamp = sample(sampPoolStr, sampSize, replace = TRUE)
    
  } else {
  
  selectSamp = NA
  
  }
  
  return(selectSamp)
  
}
