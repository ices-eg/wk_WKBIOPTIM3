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

slSex <- function(slTab, hlTab) {
  if (names(slTab)[1]!=names(hlTab)[1]) stop("tables must be consistent!!")
  ind <- NULL
  if (names(slTab)[1]=="sampType") ind <- 1:14 
  if (names(slTab)[1]=="PSUid") ind <- 1:15
  if (is.null(ind)) stop("wrong input tables!!") 
  slTab <- slTab[,ind] ; slTab$lsex <- slTab$sex ; hlTab$N <- 1:nrow(hlTab)
  #
  #inter <- slTab$lsex ; indInter <- do.call("paste",slTab[,1:14])  #modif MM 19/02/2010  : memory issue for big datasets
  #index <- do.call("paste",hlTab[,1:14])
  #indic <- match(index,indInter)
  #hlTab$lsex <- inter[indic]
  hlTab <- merge(hlTab,slTab,all.x=TRUE)
  
  hlTab <- hlTab[order(hlTab$N),-match("N",names(hlTab))]
  sex <- as.character(hlTab$sex) 
  hlTab$sex <- as.character(hlTab$lsex)
  hlTab$lsex <- sex
  rownames(hlTab) <- 1:nrow(hlTab)
  return(hlTab)
}
