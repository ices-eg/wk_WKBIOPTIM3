
# BioSim Tool 1.01 developed within STREAM (STrengthening REgional cooperation in the Area of 
#fisheries biological data collection in the Mediterranean and Black Sea (STREAM)) project

# SD Tool v.3 and WKBIOPTIM SimTool v.1  were taken into account for the developement of the present tool.
# SD tool has been developed in MARE/2014/19 project and updated in MARE/2016/22 project (STREAM)

# Authors of the first version of SD Tool: F. Gontrand, T. Rouyer, N. Billet, 2016
# IFREMER, UMR MARBEC, Avenue Jean Monnet, BP 171, 34203 S` ete, France 

# Authors of the first version of WKBIOPTIM scripts: Nuno Prista (SLU, Sweden) from a subgroup work carried out during a local Workshop on Sampling Design and Optimization (Lysekil, 31 October to 4 November 2016, unpublished)
# SLU Aqua, Institute of Marine Research, Department of Acquatic Resources - Swedish University of Agricultural Sciences

# Authors of this new tool based on SD tool and WKBIOPTIM SimTool: M.T. Facchini, I. Bitetto, 2017
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# The tool is documented in Deliverable D3.3 "Upgrade the methodological framework and tools for sampling optimization, implement and report case studies" (January 2019)

# In case of use of the tool, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail addresses: facchini@coispa.it, bitetto@coispa.it
# BioSimTool is believed to be reliable. However, we disclaim any implied warranty or representation 
# about its accuracy, completeness or appropriateness for any particular purpose.

rm(list=ls())		

# read packages
	library(rlist) # list.merge
	library(dplyr)
	library(parallel)	
  library(ggplot2)
  library(emdist)
  library(reshape2)


# set the working directory
myWD <- "C:\\Users\\Bitetto Isabella\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\MARE22\\STREAM\\FINAL REVISION OF DELIVERABLES\\To upload on GITHUB\\WP3\\Task 3.3\\BioSim Tool 1.01"
setwd(myWD)

caseStudy_path <-"C:\\Users\\Bitetto Isabella\\OneDrive - Coispa Tecnologia & Ricerca S.C.A.R.L\\MARE22\\STREAM\\FINAL REVISION OF DELIVERABLES\\To upload on GITHUB\\WP3\\Task 3.3\\BioSim Tool 1.01"

species_name <- "Parapenaeus longirostris"  
short_name <-  "DPS" 

GSA <- "GSA99" # "ALL"

# stratification of sampleId definition
by_cat="N"  # "Y" or "N"
by_EUlev6="N"  # "Y" or "N"
by_EUlev4="Y"
by_quart="N"  # "Y" or "N"

# for the name of the folder
thresholds_name <- "catN_lev4_quartY" 
thresholds_table <- read.csv2(paste(caseStudy_path, "\\input files\\B2_thresholds_template.csv", sep=""), as.is=TRUE)

	# SET THE CHOSEN THRESHOLD TO ESTIMATE THE RESPECTIVE SUBSAMPLE FACTOR
	# set threshold based on the number of categories
	# if category = "N", only one threshold is required
	
	# categ <- c("-1")
	# thresholds=c(45)
	
	# # categ <- c("1", "2", "3", "S")
	# #	thresholds=c(450,450,650,450)
	
	# load input data
	load(paste(caseStudy_path, "\\A_data_preparation\\input_data.rdata", sep=""))

	str_cat <- ifelse(by_cat == "Y", "catY", "catN")
	str_gear <- ifelse(by_EUlev6 == "Y", "lev6", ifelse(by_EUlev4 == "Y", "lev4", "gearN"))
	str_quart <- ifelse(by_quart == "Y", "quartY", "quartN")
	

	if (	GSA != "ALL") {
	  res_dir <- paste(caseStudy_path, "\\B_length_", short_name, "_", GSA, "_",str_cat, "_",str_gear, "_",str_quart, sep="")
	} else {
	  res_dir <- paste(caseStudy_path, "\\B_length_", short_name, "_",str_cat, "_",str_gear, "_",str_quart, sep="")
	}
	
	sim_dir <- paste(res_dir, "\\simulation_by_trip", sep="")
	
	dir.create(res_dir)
	dir.create(sim_dir)
	
	df0 <- df0[df0$spp == species_name, ]
	
	if (GSA != "ALL") {
	  df0 <- df0[df0$area == GSA, ]
	}
	
	# Isabella 28082018: summary of sample size by trip Id (to be eventually adapted using sampId_Cat) 
	if (by_cat=="Y") {
	  df0$sampId=df0$sampId_cat
	} 
	
	# Isabella 29082018: Estimation of subsample
	
	tab=aggregate(df0$indivId,by=list(df0$sampId),FUN="length")	
	colnames(tab) =c("sampId","Num_ind")
	
	
	if (by_cat=="Y"){
	  df0_ok=unique(df0[,c("sampId","commCat")])
	  tab=merge(tab,df0_ok,by=c("sampId")) 
	}
	
	if (by_EUlev6=="Y" | by_EUlev4=="Y" | by_quart=="Y")
	{
	  df0_ok=unique(df0[,c("sampId","foCatEu6_sc","foCatEu4","quarter")])
	  tab=merge(tab,df0_ok,by=c("sampId"))  
	  
	}
	
	if (by_cat=="N"){
	  tab$commCat="-1"
	}
	
	
	if (by_EUlev6=="N" & by_EUlev4=="N") {
	  tab$Metier_Gear="-1"
	} else if (by_EUlev6=="Y") {
	  tab$Metier_Gear=tab$foCatEu6_sc
	} else if (by_EUlev4=="Y") {
	  tab$Metier_Gear=tab$foCatEu4
	}
	
	if (by_quart=="N"){
	  tab$quarter="-1"
	}
	
	tab <- tab[, !(colnames(tab) %in% c("foCatEu6_sc", "foCatEu4")) ]

	# if (length(thresholds) >1) {
	#  	str_thre <- cat(thresholds, sep="_") 
	# } else {
	#   str_thre <- thresholds[1]
	# }

	str_thre <- thresholds_name
	
	# df=data.frame(comm_categ=categ,thr=thresholds)
	
	tab$thr=999
	tab$ratio=1
	
	for (i in c(1:nrow(tab))){
	  this_thresh <- thresholds_table$samp_size[thresholds_table$commCat==tab$commCat[i] &
	                                        thresholds_table$quarter == tab$quarter[i] &
	                                        thresholds_table$Metier_Gear == tab$Metier_Gear[i] ]
	  
	  if (length(this_thresh) >0) {
	    tab$thr[i] = this_thresh  
	    tab$ratio[i] = tab$Num_ind[i]/tab$thr[i]
	  } else {
	    tab$thr[i]= NA 
	    tab$ratio[i] = NA
	  }
	}
	
	tab_or=tab
	tab = tab[!is.na(tab$thr),]
	tab=tab[tab$ratio > 1, ]
	
	sub_sample = aggregate(tab$ratio,by=list(tab$commCat, tab$Metier_Gear, tab$quarter),FUN="mean")
	sub_sample2 = aggregate(tab$ratio,by=list(tab$commCat, tab$Metier_Gear, tab$quarter),FUN="length")
	sub_sample3 = aggregate(tab_or$ratio,by=list(tab_or$commCat, tab_or$Metier_Gear, tab_or$quarter),FUN="length")
	
	mer3=merge(sub_sample,sub_sample2,by=c("Group.1", "Group.2", "Group.3"))
	mer3=merge(mer3,sub_sample3,by=c("Group.1",  "Group.2", "Group.3"))
	
	colnames(mer3)=c("commCat","Metier_Gear","quarter", "Sub_sample","Nb_over_Thr","Tot")
	
	write.table(tab,paste(res_dir, "/Sub_sample_complete_",str_thre,".csv", sep=""),sep=";",row.names=F)
	write.table(mer3,paste(res_dir, "/Sub_sample_",str_thre,".csv", sep=""),sep=";",row.names=F)
	
	
	