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

cvFunStr = function(caseInd, stratification, availStrat, availSamp, CSobject, CLobjectList, sppName, nIter){


  if (FALSE) {
    
    caseInd= nr
    stratification= strIni()
    CSobject= CS_boot  
    CLobjectList = CL_boot
    sppName= REFERENCE_SPECIES

  }
   
  print("______________________________________________________________________", quote=FALSE)
  print(paste("Stratum:", toupper(availStrat$Var1[caseInd]), toupper(availStrat$Var2[caseInd]), toupper(availStrat$Var3[caseInd]) ), quote=FALSE)
  print("______________________________________________________________________", quote=FALSE)

  global_case <<- availStrat[caseInd,1:3]
  
  case = global_case
  
  # subset by area 
  switch(col_ele_var1,
         
		 GSA={ CLobjectList =  subset(CLobjectList, area == as.character(availStrat$Var1[caseInd]), link = TRUE)},
		 Country={CLobjectList =  subset(CLobjectList, vslFlgCtry == as.character(availStrat$Var1[caseInd]), link = TRUE)},
		 ALL = { CLobjectList = CLobjectList },
     stop("Enter something that switches me!") )
  
  
  switch(col_ele_var2,
        
		 Q = {  CLobjectList = subset(CLobjectList, quarter == as.character(availStrat$Var2[caseInd]))},
		 S = {  if (availStrat$Var2[caseInd] == 1) {
             CLobjectList = subset(CLobjectList, quarter == 1 | quarter == 2)
           } else {
             CLobjectList = subset(CLobjectList, quarter == 3 | quarter == 4) } },
     Y = { CLobjectList = CLobjectList },
     stop("Enter something that switches me!") )
  
  
  switch(col_ele_var3,  
         
		 lev6 = { CLobjectList = subset(CLobjectList, foCatEu6 == as.character(availStrat$Var3[caseInd]))},
		 lev4 = { CLobjectList = subset(CLobjectList, foCatEu5 == as.character(availStrat$Var3[caseInd]))},
     NONE = { CLobjectList = CLobjectList },
     stop("Enter something that switches me!") )
  

#    switch(col_ele_var4,    
#           
# 	 Y = {   CLobjectList = subset(CLobjectList, foCatEu6 == as.character(availStrat$Var3[caseInd]))},
#    N = { sampPoolStr4 = data.frame(sampPoolStr = unique(availSamp$trpCode) , group4 = rep(1, length(unique(availSamp$trpCode)))) },
#    stop("Enter something that switches me!") ) 
#    


 # subset by area 

switch(col_ele_var1,
       
    GSA={ sampPoolStr1 = data.frame(sampPoolStr = unique(availSamp$trpCode[which(availSamp$area == case$Var1)] ) , group1 = rep(1, length(unique(availSamp$trpCode[which(availSamp$area == case$Var1)] )) )) },
    Country={ sampPoolStr1 = data.frame(sampPoolStr = unique(availSamp$trpCode[which(availSamp$vslFlgCtry == case$Var1)]  ) , group1 = rep(1, length(unique(availSamp$trpCode[which(availSamp$vslFlgCtry == case$Var1)]  ) ) ) ) },
    ALL = { sampPoolStr1 = data.frame(sampPoolStr = unique(availSamp$trpCode  ), group1 = rep(1, length(unique(availSamp$trpCode  )) ) ) },
    stop("Enter something that switches me!") )


switch(col_ele_var2,
    
    Q = {  sampPoolStr2 = data.frame(sampPoolStr = unique(availSamp$trpCode[which(quarter(availSamp$date) %in% case$Var2)] ), 
             group2 = rep(1, length(unique(availSamp$trpCode[which( quarter(availSamp$date) %in% case$Var2)] ))) ) },
    S = { sampPoolStr2 = data.frame(sampPoolStr = unique(availSamp$trpCode[which( ifelse(quarter(availSamp$date) == 1 | quarter(availSamp$date) == 2, 1, 2) %in% case$Var2)] ), 
                                    group2 = rep(1, length(unique(availSamp$trpCode[which( ifelse(quarter(availSamp$date) == 1 | quarter(availSamp$date) == 2, 1, 2) %in% case$Var2)] )))) },
    Y = { sampPoolStr2 = data.frame(sampPoolStr = unique(availSamp$trpCode  ), group2 = rep(1, length(unique(availSamp$trpCode  )) ) ) },
    stop("Enter something that switches me!") )


switch(col_ele_var3,    
    
    lev6 = { sampPoolStr3 = data.frame(sampPoolStr = unique(availSamp$trpCode[which(availSamp$foCatEu6 == case$Var3)] ), group3 = rep(1, length(unique(availSamp$trpCode[which(availSamp$foCatEu6 == case$Var3)] )) ) ) },
    lev4 = { sampPoolStr3 = data.frame(sampPoolStr = unique(availSamp$trpCode[which(availSamp$foCatEu5 == case$Var3)] ), group3 = rep(1, length(unique(availSamp$trpCode[which(availSamp$foCatEu5 == case$Var3)] ) )) ) },
    NONE = { sampPoolStr3 = data.frame(sampPoolStr = unique(availSamp$trpCode ), group3  = rep(1, length(unique(availSamp$trpCode ) )) ) },
    stop("Enter something that switches me!") )


# switch(col_ele_var4,
  
#      Y = {  sampPoolStr4 = data.frame(sampPoolStr = unique(availSamp$trpCode[which(availSamp@hh$commCat == case$Var4)]), group4 =rep(1, length( unique(availSamp$trpCode[which(availSamp@hh$commCat == case$Var4)])) ) ) },
#      N = { sampPoolStr4 = data.frame(sampPoolStr = unique(availSamp$trpCode) , group4 = rep(1, length(unique(availSamp$trpCode)))) },
#      stop("Enter something that switches me!") )

  sampPoolStr =  merge(sampPoolStr1, merge(sampPoolStr2, sampPoolStr3, all=TRUE), all=TRUE)
  sampPoolStr$sum_groups <- rowSums(sampPoolStr[, 2:4])
  sampPoolStr <- sampPoolStr[!is.na(sampPoolStr$sum_groups), ]
  sampPoolStr <- as.character(sampPoolStr$sampPoolStr[sampPoolStr$sum_groups == 3] )
  

    print("**********************************************************************", quote=FALSE)
    print(paste("total number of trips available for the bootstrap:",length(sampPoolStr) ), quote=F)
    print("**********************************************************************", quote=FALSE)
    
  max.sample.size = availStrat$maxSampSize[caseInd]
  min.sample.size = 2 

  head_ <- c("cv", "nMeas","sampSize" , "recyclingRate")
  res_all_iter <- data.frame(matrix(nrow=0, ncol=length(head_)))
  colnames(res_all_iter) <- head_

  LANDINGS_EXISTS <- TRUE

for (n_it in c(1:nIter) ) {
  
  if (LANDINGS_EXISTS) {

    # sample size
     sampSize = sample(min.sample.size:max.sample.size, 1, replace = FALSE)
     # select samples (trip codes)
     #  selectSamp = sample(sampPoolStr, sampSize, replace = TRUE)
     sampSize =   round(sampSize, 0)
     selectSamp = sample(sampPoolStr, sampSize, replace = TRUE)
     # recycling rate
     rate = (length(selectSamp) - length(unique(selectSamp)) ) / length(selectSamp)
     
     print(paste("No. of samples selected for bootstrap iteration n°", n_it), quote=FALSE )
     print( length( selectSamp), quote=FALSE )

     extentionList = 1 : length(selectSamp) # pas assez unque pour les trip code de spain data

     tr = CSobject@tr
     hh = CSobject@hh
     sl = CSobject@sl
     hl = CSobject@hl

     selectTR = do.call("rbind", lapply(1 : sampSize, select.and.rename.tripcode2, tr, extentionList, selectSamp))
     selectHH = do.call("rbind", lapply(1 : sampSize, select.and.rename.tripcode2, hh, extentionList, selectSamp))
     selectSL = do.call("rbind", lapply(1 : sampSize, select.and.rename.tripcode2, sl, extentionList, selectSamp))
     selectHL = do.call("rbind", lapply(1 : sampSize, select.and.rename.tripcode2, hl, extentionList, selectSamp))

     # re-building the CS object

     selectSL <- selectSL[ selectSL$catchCat !=  "DIS", ]
     selectHL <- selectHL[ selectHL$catchCat !=  "DIS", ]
     

     # re-building the CS object
     
     no_trip_positive_to_species <- length(unique(selectSL$trpCode))
     
     if (no_trip_positive_to_species > 1 ) {
       
     subCS = csData(tr = selectTR,
                    hh = selectHH,
                    sl = selectSL,
                    hl = selectHL)

     years_for_selection <- unique(CLobjectList@cl$year) 
     # random selectection of the CLobject
     CL_ye = sample(years_for_selection, 1)
     if (length(years_for_selection) == 1) {
       CL_ye = years_for_selection
     }
     CLobject_ye = subset(CLobjectList, year == CL_ye, link = TRUE)
     
     
     if (nrow(CLobject_ye@cl) > 0) {
       
     CLobject_ye@cl$year = 2999
     
     # outputs
     nMeas = sum(subCS@hl$lenNum, na.rm = TRUE)

     CSval = csDataVal(subCS)
     CLval = clDataVal(CLobject_ye)
     
     CSval@hl
     
     CScons = csDataCons(CSval, stratification)
     CLcons = clDataCons(CLval, stratification) 
     
     dbeOutput = dbeObject(desc = sppName, species = sppName, catchCat = "Lan",  strataDesc = stratification, methodDesc = "analytical")
     
     raising = try(RaiseLgth(dbeOutput, CScons, CLcons, spp = sppName, taxon = sppName, strDesc = stratification), silent=TRUE)
     
     #computeCv_res = list(cvLenCls = raising@lenNum$cv, cv = raising@lenNum$DCRcvIndicator, ci = raising@lenNum$ci, LenStr = raising@lenStruc[[1]])
     # computeCv_res <-  try(computeCv(CLobject, subCS, stratification, sppName, taxoCode), silent = TRUE)
     
     if(class(raising) == "try-error"){
       cv = NA
       ci = NA
       lenstructure = NA
       rr = NA
       nMeas = NA
       
       print("Impossible to calculate CV due a problem in COST function!", quote=FALSE)
       
     } else {
       computeCv_res = list(cvLenCls = raising@lenNum$cv, cv = raising@lenNum$DCRcvIndicator, ci = raising@lenNum$ci, LenStr = raising@lenStruc[[1]])
       cv = computeCv_res$cv
       ci = computeCv_res$ci
       
       lenstructure = computeCv_res$LenStr
       
       result2 = data.frame(cbind(rep(n_it, nrow(lenstructure)), cbind(rep(global_case$Var1, nrow(lenstructure)), cbind(rep(global_case$Var2, nrow(lenstructure)) , cbind(rep(global_case$Var3, nrow(lenstructure)), lenstructure[,4:5]) ))))
       colnames(result2)[1:4] <- c("iteration" , "Var1", "Var2", "Var3")
       
       LengthStr_dataframe <<- data.frame(rbind(LengthStr_dataframe,result2 ))
       rr <- round(rate, 2)

     }
     
     result = data.frame(cv, nMeas, sampSize, recyclingRate = rr)
    

     }   else {
       
       LANDINGS_EXISTS <- FALSE

       cv = NA
       ci = NA
       rr = NA
       nMeas = NA
       result = data.frame(cv, nMeas, sampSize, recyclingRate = rr)
       lenstructure = NA
       lenCV = NA
     }
     
} else {
  
  if (no_trip_positive_to_species == 0) {
    print("Impossible to calculate CV due to only samples of discard in the stratum!", quote=FALSE)
  } else {
    print("Impossible to calculate CV due to the number of trips positive to the species in the stratum (equal to 1)!", quote=FALSE)
  }
  
  cv = NA
  ci = NA
  rr = NA
  nMeas = NA
  result = data.frame(cv, nMeas, sampSize, recyclingRate = rr)
  lenstructure = NA
  lenCV = NA
  
}
     
  } else {
    
    cv = NA
    ci = NA
    rr = NA
    nMeas = NA
    result = data.frame(cv, nMeas, sampSize, recyclingRate = rr)
    lenstructure = NA
    lenCV = NA
    

  }
  
  
  if (n_it == 1 | !exists("res_all_iter") ) {
    res_all_iter = result
  } else {
    if (LANDINGS_EXISTS) {
          res_all_iter = data.frame(rbind(res_all_iter, result))
    }
  }
  
  # res_all_iter = data.frame(rbind(res_all_iter, result))
     
} # end iterations
   
  if (!LANDINGS_EXISTS) {
     print("Impossible to calculate CV on the raised LFD for all the iterations due to MISSING LANDINGS in the stratum!", quote=FALSE)
  } else {
    if (n_it == nIter) {
      print(paste(nIter , "iterations done!"), quote=F)
    }
  }

  return(res_all_iter)        

    }
                                           
                                                                 