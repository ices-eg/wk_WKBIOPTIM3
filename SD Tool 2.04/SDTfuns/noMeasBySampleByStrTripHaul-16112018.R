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

noMeasBySampleByStrTripHaul = function(i, tabStrat, CSsp) {
  
    if (FALSE) {
      i=1
      CSsp = CS_boot_scen
    }
  
  case = tabStrat[i,]
  
  # subset by area 
switch(col_ele_var1,
       
    GSA={ CSstr1 = subsetCOST(CSsp, area %in% case$Var1, table = "hh") },
    Country={ CSstr1 = subsetCOST(CSsp,  vslFlgCtry %in% case$Var1, table = "hh") },
	  ALL={ CSstr1 = CSsp},
    stop("Enter something that switches me!") )


switch(col_ele_var2,
    
    Q = { CSstr2 = subsetCOST(CSstr1,  ceiling(as.numeric(month(CSstr1@hh$date)) / 3) %in% case$Var2, table = "hh")},
    S = { CSstr2 = subsetCOST(CSstr1,  ceiling(as.numeric(month(CSstr1@hh$date)) / 6) %in% case$Var2, table = "hh")},
    Y = { CSstr2 = CSstr1 },
    stop("Enter something that switches me!") )


switch(col_ele_var3,    
    
    lev6 = { CSstr3 = subsetCOST(CSstr2,  foCatEu6 %in% case$Var3, table = "hh") },
    lev4 = { CSstr3 = subsetCOST(CSstr2,  foCatEu5 %in% case$Var3, table = "hh") },
    NONE = {  CSstr3 = CSstr2},
    stop("Enter something that switches me!") )



# switch(col_ele_var4,  
#        
#     Y = {  CSstr4 = subsetCOST(CSstr2,  commCat %in% case$Var4, table = "hh") },
#     N = { CSstr4 = CSstr3 },
#     stop("Enter something that switches me!") )


#  CSstr0 = .subsetCOST(CSsp, foCatEu6 %in% case$Var1, table = "hh")
#  CSstr = .subsetCOST(CSstr0, ceiling(as.numeric(substring(CSstr0@hh$date, 6, 7)) / 3) %in% case$Var2, table = "hh")
  
HL_df <-  data.frame(CSstr3@hl)

HL_df$commCat <- "ALL"
HL_df$trpCodeHaul <- paste(HL_df$trpCode, HL_df$staNum, sep="_")

result_grouped = dplyr::group_by(HL_df, trpCode, trpCodeHaul, commCat)
result = data.frame(dplyr::summarise(result_grouped,  noMeasures = sum(as.numeric(as.character(lenNum) )) ))

  result = data.frame(cbind(data.frame(Var1 = rep(case$Var1, nrow(result)), Var2 = rep(case$Var2, nrow(result)), Var3 = rep(case$Var3, nrow(result))), 
                            trpCode = result$trpCode, commCat = result$commCat, noMeasures = result$noMeasures, trpCodeHaul = result$trpCodeHaul))
  
  if (all(is.na(result$trpCode ) ) ) {
    result <- result[result$Var1 == "ksngpnfdspgfndplsngpsfengphd", ]
    
  } 
  
  return(result)
  
}