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

indMeasNoStr_count = function(iter, availStrat, availSamp, sampSizeRange, CSobject) {
    
  if (FALSE) {
   iter=n_it
  CSobject = CS_boot  
  } 


  tr = CSobject@tr
  hh = CSobject@hh
  sl = CSobject@sl
  hl = CSobject@hl
  
  for (indStr in 1:nrow(availStrat)) {
    
    str = availStrat[indStr,]
    tripCodeSelection = selectSampStr(indStr, availStrat, availSamp, sampSizeRange)
    
    if( length(tripCodeSelection) > 0 & !is.na(tripCodeSelection) ) {
      ## generating list of extention of each trip code
      extention.list = 1:length(tripCodeSelection)
      tripCodeSelection <- tripCodeSelection[!is.na(tripCodeSelection)]
      
      # subset CS tables
      select.tr = do.call("rbind", lapply(1 : length(tripCodeSelection), select.and.rename.tripcode2, tr, extention.list, tripCodeSelection))
      select.hh = do.call("rbind", lapply(1 : length(tripCodeSelection), select.and.rename.tripcode2, hh, extention.list, tripCodeSelection))
      select.sl = do.call("rbind", lapply(1 : length(tripCodeSelection), select.and.rename.tripcode2, sl, extention.list, tripCodeSelection))
      select.hl = do.call("rbind", lapply(1 : length(tripCodeSelection), select.and.rename.tripcode2, hl, extention.list, tripCodeSelection))
      
      # re-building the CS object
      subCS = csData(tr = select.tr,
                     hh = select.hh,
                     sl = select.sl,
                     hl = select.hl)
      
      result = sum(subCS@hl$lenNum, na.rm = TRUE)
      
    } else {
      
      result = NA
      
    }
    
    df_res <- data.frame(Var1 = availStrat$Var1[indStr], Var2 = availStrat$Var2[indStr], Var3 = availStrat$Var3[indStr], nInd = result)
    
   if (indStr == 1) {
     nMeas = df_res
   } else {
     nMeas = data.frame(rbind(nMeas, df_res))
   }
     
  }
  
  # nMeas = sapply(1 : nrow(availStrat), nMeasByStr, availStrat, availSamp, sampSizeRange, tr, hh, sl, hl)

  return(nMeas)
}
