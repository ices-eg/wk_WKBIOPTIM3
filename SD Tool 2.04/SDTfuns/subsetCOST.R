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

subsetCOST <- function(x,subset,..., table="tr") {
  
  isVal <- class(x)=="csDataVal"
  #-----------------------------------------------------------------------------
  # Extraction of each table
  #-----------------------------------------------------------------------------
  
  tr <- tr(x)
  hh <- hh(x)
  sl <- sl(x)
  hl <- slSex(sl,hl(x))     #hl with an artificial 'sex' field to consider 'sex' field in sl as a 'full' key field
  ca <- csData()@ca  # ca <- ca(x)
  
  #-----------------------------------------------------------------------------
  # Function to build a primary or a foreign key for any table
  #-----------------------------------------------------------------------------
  
  fpKey <- function(tab,colIndex,sep=":-:") {
    key <- tab[,colIndex]
    key <- apply(key,1,paste,collapse=sep)
    key <- gsub("[[:space:]]","",key)
    return(key)
  }
  
  
  #-----------------------------------------------------------------------------
  # Parts of tr that are linked to hh & ca are identified
  #-----------------------------------------------------------------------------
  
  indca <- fpKey(tr,1:6) %in% fpKey(ca,1:6)
  #part of tr that is linked to ca, and index
  trca <- tr[indca,] ; trca$N <- (1:nrow(tr))[indca]
  #part of tr that is linked to hh (ie not linked to ca), and index
  trhh <- tr[!indca,] ; trhh$N <- (1:nrow(tr))[!indca]
  
  #-----------------------------------------------------------------------------
  # Specified table is subset according to 'subset' parameter
  #-----------------------------------------------------------------------------
  e <- substitute(subset)
  df0 <- eval(parse('',text=table))#do.call(table, list(object=x))  
  r <- eval(e, df0, parent.frame(n=1))
  eval(parse('',text=paste(table, "<- df0[r,]")))
  
  #-----------------------------------------------------------------------------
  # Keyfield indexes according to table hierarchy are defined in Up & Down tables
  #-----------------------------------------------------------------------------
  
  Up <- matrix(c("trhh","hh","sl","trca","1:6","1:7","1:14","1:6"),nrow=2,byrow=TRUE)
  dimnames(Up) <- list(c("tab","index"),c("hh","sl","hl","ca"))
  
  Down <- matrix(c("hh","ca","sl","hl","1:6","1:6","1:7","1:14"),nrow=2,byrow=TRUE)
  dimnames(Down) <- list(c("tab","index"),c("tr","tr","hh","sl"))
  
  #-----------------------------------------------------------------------------
  # Generic subsetting function using Up & Down table format
  #-----------------------------------------------------------------------------
  
  subs <- function(tabName,tabKey){
    if (tabName%in%dimnames(tabKey)[[2]]) {
      indSub <<- TRUE   #index that shows that the procedure has been used 
      mat <- tabKey[,dimnames(tabKey)[[2]]%in%tabName,drop=FALSE]
      eval(parse('',text=paste(mat["tab",], " <<- ", mat["tab",], "[fpKey(", mat["tab",], ",", mat["index",],           #warning : "<<-" might be replaced by 'assign(...)'
                               ")%in%fpKey(", tabName, ",", mat["index",], "),]",sep="",collapse=";")))
      Recall(mat["tab",1],tabKey)      #mat["tab",1] because if tabName=="tr" & tabKey=Down, mat["tab",] = c("hh",ca")
    }}
  
  #-----------------------------------------------------------------------------
  # Let's apply the subsetting "loop" 
  #-----------------------------------------------------------------------------
  
  indSub <- FALSE
  #first, upward from 'table'...
  subs(table,Up)
  
  #then paste trhh & trca, and reorder according to N field (if indSub=TRUE)
  if (indSub) {
    tr <- rbind.data.frame(trhh,trca)
    tr <- tr[order(tr$N),1:(ncol(tr)-1)]
  }
  
  #and finally, downward from "tr" (for consistency)
  subs("tr",Down)
  
  if (nrow(hl)>0) {hl$sex <- hl$lsex ; hl <- hl[,-ncol(hl)]}                  #modif 08/12/2008 MM
  
  #-----------------------------------------------------------------------------
  # Output
  #-----------------------------------------------------------------------------
  
  # if(nrow(tr)<1) res <- csData(desc=x@desc)                                                              #modif 19/01/2009
  #   else if ((nrow(hh)<1) & (nrow(ca)>0)) res <- csData(tr=tr,ca=ca,desc=x@desc)                         #
  # 	      else if(nrow(hh)<1) res <- csData(tr=tr,desc=x@desc)                                           #
  #             else if(nrow(sl)<1) res <- csData(tr=tr, hh=hh,desc=x@desc)                                #
  #                  else if(nrow(hl)<1) res <- csData(tr=tr, hh=hh, sl=sl,desc=x@desc)                    #
  #	                     else if(nrow(ca)<1) res <- csData(tr=tr, hh=hh, sl=sl, hl=hl,desc=x@desc)         #
  #	                          else res <- csData(tr=tr, hh=hh, sl=sl, hl=hl, ca=ca,desc=x@desc)            #
  #                                                                                                        #

  if(nrow(tr)<1) tr <- csData()@tr                                                                        #
  if(nrow(hh)<1) hh <- csData()@hh                                                                        #
  if(nrow(sl)<1) sl <- csData()@sl                                                                        #
  if(nrow(hl)<1) hl <- csData()@hl                                                                        #
  if(nrow(ca)<1) ca <- csData()@ca                                                                        #
  res <- csData(tr=tr,hh=hh,sl=sl,hl=hl,ca=ca,desc=x@desc)                                                 #
  
  if (isVal) res <- csDataVal(res)
  return(res)  
}
