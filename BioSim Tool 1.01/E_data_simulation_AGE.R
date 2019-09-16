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
	library(reshape2)
	library(emdist)

	
	# set the working directory
	myWD <- "C:\\BioSim Tool 1.01"

	setwd(myWD)
	
	caseStudy_path <-"C:\\BioSim Tool 1.01"
	
	species_name <- "Mullus barbatus"  # "Aristeus antennatus"
	short_name <-  "MUT" # "ARA"
	
	GSA <- "GSA99" # "ALL"
	
	# set sampling design of sample data
	sampling_design <- list (stratified = TRUE, strata_var = "lenCls")

	
	# expliciting the number of iterations and the sample sizes (numbers of readings by length class)
	n_sims<-10
	samp_sizes<-c(seq(2,8, by=1))
	
	# Sampling of different number of individuals without replacement (sample size dependent of size classes in the sample)
	sampling_options <- list (n_sims = n_sims, 
	                          stages="one", 		# no of stages
	                          stratified = TRUE, strata_var = "lenCls", 		# stratification details
	                          stage1_samp_size=NA, samp_sizes = samp_sizes , 		# samp sizes
	                          replacement=FALSE, 	sample_all_available = TRUE, 
	                          sample_all_available_warning = FALSE, 	# replacement options
	                          vars_to_keep = c( "Age"))	
	
	# read variable table
	variable_table <- read.csv2(paste(caseStudy_path,"\\input files\\E_variable_table_biotic_AGE.csv", sep=""), as.is=TRUE)
	
	# 	stratification of sampleId definition
	by_cat="N"  # "Y" or "N"
	by_EUlev6="N"  # "Y" or "N"
	by_EUlev4="N"
	by_quart="N"  # "Y" or "N"
	
	# load input data
	load(paste(caseStudy_path, "\\A_data_preparation\\input_data - AGE.rdata", sep=""))
	
	str_cat <- ifelse(by_cat == "Y", "catY", "catN")
	str_gear <- ifelse(by_EUlev6 == "Y", "lev6", ifelse(by_EUlev4 == "Y", "lev4", "gearN"))
	str_quart <- ifelse(by_quart == "Y", "quartY", "quartN")
	
	if (	GSA != "ALL") {
	  res_dir <- paste(caseStudy_path, "\\E_age_", short_name, "_", GSA, "_",str_cat, "_",str_gear, "_",str_quart,  sep="")
	} else {
	  res_dir <- paste(caseStudy_path, "\\E_age_", short_name, "_",str_cat, "_",str_gear, "_",str_quart, sep="")
	}
	
	#sim_dir <- paste(res_dir, "\\simulation_by_trip", sep="")
	
	dir.create(res_dir)
	#dir.create(sim_dir)
	
	# read functions
	source("funs-26102018.R") # contains "expl.analysis.smooth.and.modes", sim_sample

	df0 <- df0[df0$spp == species_name, ]
	
	if (GSA != "ALL") {
	  df0 <- df0[df0$area == GSA, ]
	}
	
	df0$age[df0$age == -1] <- NA
	
	length(df0$age[!is.na(df0$age)])

	
	#check compatibility
	if (by_EUlev6=="Y" & by_EUlev4=="Y")   {
	  print("Incompatibility of by_EUlev6==Y & by_EUlev4==Y")
	}
	
	# setting of the minimum number of individuals considered representative
	# Isabella 28082018: summary of sample size by trip Id (to be eventually adapted using sampId_Cat) 
	
	
	
	if (by_cat=="Y") {
	  df0$sampId=df0$sampId_cat
	  df0$indivId=df0$indivId_cat
	} 
	
	
	
	df0_temp_ <- df0
	
	if (by_cat=="N"){
	  df0_temp_$commCat="-1"
	}
	
	
if (by_EUlev6=="N" & by_EUlev4=="N") {
	  df0_temp_$Metier_Gear="-1"
	} else if (by_EUlev6=="Y") {
	  df0_temp_$Metier_Gear=df0_temp_$foCatEu6_sc
	} else if (by_EUlev4=="Y") {
	  df0_temp_$Metier_Gear=df0_temp_$foCatEu4
	}
	
	if (by_quart=="N"){
	  df0_temp_$quarter="-1"
	}
	
	
	df0_temp_ <- df0_temp_[!is.na(df0_temp_$age) & df0_temp_$age != -1, ]
	
	df0_by_year <-  group_by(df0_temp_ , Metier_Gear,quarter,commCat, year )  
	df0_by_year <- data.frame(summarise(df0_by_year,  age_measures=length(age)))
	
	df0_avg <-  group_by(df0_by_year , Metier_Gear,quarter,commCat )  
	df0_avg <- data.frame(summarise(df0_avg,  age_meas=round(mean( age_measures), 0)))
	
	df0_avg$year <- paste(min(df0_by_year$year), max(df0_by_year$year), sep="-")
	df0_avg <- df0_avg[ , c(1:3, 5, 4)]
	colnames(df0_avg)[5] <- 	colnames(df0_by_year)[5]
	past_situation <- data.frame(rbind(df0_by_year, df0_avg))
	write.table(past_situation,paste(res_dir,"/Past situation.csv", sep=""),sep=";",row.names=F)

	
	
	#	TO CHECK the numbers of length measurement in the datasets
	
	tab=aggregate(df0_temp_$indivId,by=list(df0_temp_$sampId, df0_temp_$lenCls),FUN="length")	
	colnames(tab) =c("sampId","lenCls","Num_ind_with_age") 
	write.table(tab, paste(res_dir,"/Numbers with age by sampId and lenCls.csv", sep=""),sep=";",row.names=F)

	QUANTS <- group_by(tab, lenCls)
	QUANTS <- summarise(QUANTS, Min=min(Num_ind_with_age), 
	                    qu_1st = quantile(Num_ind_with_age,  probs = c(0.05, 0.25, 0.5, 0.75, 0.95))[2], 
	                    Median=median(Num_ind_with_age),
	                    Mean=mean(Num_ind_with_age),
	                    qu_3rd = quantile(Num_ind_with_age,  probs = c(0.05, 0.25, 0.5, 0.75, 0.95))[4],
	                    Max=max(Num_ind_with_age))
	
	write.table(QUANTS, paste(res_dir,"/Numbers with age by sampId and lenCls_quantiles.csv", sep=""),sep=";",row.names=F)
	
	
	# setting of the minimum number of individuals considered representative
		min_n<-20

		table_select_samples<-table(df0$sampId)[table(df0$sampId)>=min_n]; 
		samples_to_analyze<-names(table_select_samples)		
		

# ======================
# Weight - Length Relationship		
# ======================

	#coefs_weight_length<-coef(lm(log(df0$indWt)~log(df0$lenCls)))
#names(coefs_weight_length)<-c("a","b")
	
	 coefs_weight_length<- c(a= NA, b=NA)
	
	
# =======================
# Simulations	
# =======================

		# if your data is a simple random sample of individuals from which all biological variables were sampled you will be able to test all sampling strategies
		# if your data was stratified with regards to one of the variables you have to take that into account
			# you can mantain the stratification and look into the consequences of a reduction or increase in the number of individuals samples per strata
			# you can attemp to de-stratify, creating a pseudo random sample and then test scenarios in it

															# check			

	seed<-1
	set.seed(seed)
	ptc1<-Sys.time()	
	
	df0_temp <- df0[!is.na(df0$age),]
	df0_noages <- group_by(df0_temp, sampId)
	df0_noages_ <- data.frame(summarise(df0_noages,  tot_ages=length(lenCls) ))	
	
	samples_to_analyze <- samples_to_analyze[samples_to_analyze %in% df0_noages_$sampId ]

	# creates a storage object
	ls_DT_compiled<-sapply(samples_to_analyze, function(x) NULL)
	
	for (sampId in samples_to_analyze)  {
	  # sampId =  "59_18_2016_ITA_999"
	  #"2001_999"
	 
	  
	  print("==========================")
	  print(sampId)      
	  print("==========================")
	  
	  # selects sample
	  df1<-df0[df0$sampId == sampId,]


# ===============			
# Simulations of samples
# ===============
	
											
		df1_no_NAs <- df1[!is.na(df1$age), ]				
	  		
	#	samples_to_analyze
		
		ls_sims1 <- faz_sim_sample(sampDes = sampling_design, sampOpt = sampling_options, df1o = df1_no_NAs)	
				
		
# ====================
# Building of sample statistics
# ====================


	# creates storage object
	ls_sims_stats<-lapply(sapply(as.character(sampling_options$samp_sizes), function(x) NULL), function(x) sapply(variable_table$variable, function(x) NULL)) 

	vars_numerical<-variable_table$variable[variable_table$type=="numerical"]
	vars_categorical<-variable_table$variable[variable_table$type=="categorical"]
	
	detectCores(all.tests = FALSE, logical = TRUE)
	
	# set the no of cores 
	# detectCores()
	cl <- makeCluster(5)
	#clusterExport(cl, varlist = c("make_summary_numeric","variable","df1","ls_auto_modes","coefs_weight_length","localMaxima2","localMaxima","vars_numerical","vars_categorical"))		
	clusterExport(cl, varlist = c("make_summary_numeric","df1","coefs_weight_length","localMaxima2","localMaxima","vars_numerical","vars_categorical"))		
	
	# ls_auto_modes_sample<-ls_auto_modes[[sampId]]

	ls_DT_compiled_ss <-vector(mode="list", length=length(sampling_options$samp_sizes))
	
	for (j in 1:length(sampling_options$samp_sizes))	{
	  
	  # j=1
	  
	  variable = "age"
	  
	  if(sampling_options$stratified & (sampling_options$stages =="one" | sampling_options$stages =="two")) # not stratified, one or two stages
	  {
	    print(paste("Processing sample size", sampling_options$samp_sizes[j]))	
	    if (sampling_options$stages == "one") w <- "1st_Stage" else w <- "2nd_Stage"
	    
	    res_ <- do.call("rbind",lapply(ls_sims1[[j]], function(x){ make_summary_proportion_by_length(x[[w]], variable)}))
	    res_$n_it <- do.call("rbind",strsplit(as.character(rownames(res_)), "[.]"))[,1]
	    res_$samp_size <- sampling_options$samp_sizes[j] 
	    
# 	    res_ <- do.call("rbind",lapply(ls_sims1[[j]], function(x){ make_summary_numeric_stratified(x, variable,1,1)}))
# 	    res_$n_it <- do.call("rbind",strsplit(as.character(rownames(res_)), "[.]"))[,1]
# 	    res_$samp_size <- sampling_options$samp_sizes[j] 
	    			
	  } else {
	    print("NOT ENTER!!!")
	  }
	  
	  ls_DT_compiled_ss[[j]] <- res_	
	}
	
	#browser()	
	
	# 	ls_DT_compiled[["59_18_2016_ITA_999"]][[10]][ls_DT_compiled[["59_18_2016_ITA_999"]][[10]]$lenCls == 55, ]
	ls_DT_compiled[[sampId]] <- ls_DT_compiled_ss
	
	stopCluster(cl)
# str(	ls_DT_compiled[["81_18_2017_ITA_999"]][1])
	
	}

	
	tab=aggregate(df0_temp_$indivId,by=list(df0_temp_$sampId, df0_temp_$lenCls),FUN="length")	
	colnames(tab) =c("sampId","lenCls","Num_ind_with_age")
	
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
	
	
	tab$is_BASELINE = FALSE 
	tab$is_BASELINE[tab$sampId %in% samples_to_analyze ] = TRUE 
	
	BASELINE_VALUE_tab <- group_by(tab, commCat, Metier_Gear,quarter, lenCls)
	BASELINE_VALUE_tab <- data.frame(summarise(BASELINE_VALUE_tab, BASELINE_VALUE= max(Num_ind_with_age) ))
	BASELINE_VALUE_tab$BASELINE_VALUE <- round(BASELINE_VALUE_tab$BASELINE_VALUE, 0)
	
	BASELINE_VALUE_tab$BASELINE_VALUE[which(	BASELINE_VALUE_tab$BASELINE_VALUE %in% samp_sizes)] <- BASELINE_VALUE_tab$BASELINE_VALUE[which(	BASELINE_VALUE_tab$BASELINE_VALUE %in% samp_sizes)] +1
	
	BASELINE_VALUE_tab <- group_by(BASELINE_VALUE_tab, commCat, Metier_Gear,quarter)
	BASELINE_VALUE_tab <- data.frame(summarise(BASELINE_VALUE_tab, BASELINE_VAL= max(BASELINE_VALUE) ))
	BASELINE_VALUE_tab$BASELINE_VAL <- round(BASELINE_VALUE_tab$BASELINE_VAL, 0)
	
	BASELINE_VALUE_tab$BASELINE_NAME <-  paste("Baseline [", BASELINE_VALUE_tab$BASELINE_VAL , "]", sep="")	


	for (sampId in samples_to_analyze)  {
	  
	  if (sampId == samples_to_analyze[1]) {
	    total_alk_res <-	do.call("rbind", ls_DT_compiled[[sampId]])
	    
	  } else {
	    total_alk_res <- rbind(total_alk_res, do.call("rbind", ls_DT_compiled[[sampId]]))
	    
	  }
	  
	}
	

	
	if (by_cat=="Y"){
	  df0_ok=unique(df0[,c("sampId","commCat")])
	  total_alk_res=merge(total_alk_res,df0_ok,by=c("sampId")) 
	  
	}
	
	if (by_EUlev6=="Y" | by_EUlev4=="Y" | by_quart=="Y")
	{
	  df0_ok=unique(df0[,c("sampId","foCatEu6_sc","foCatEu4","quarter")])
	  total_alk_res=merge(total_alk_res,df0_ok,by=c("sampId"))  
	  
	}
	
	# aggregate ALK from simulations by settings: category, metier, quarter 
	
	if (by_cat=="N"){
	  total_alk_res$commCat="-1"
	}
	
	if (by_EUlev6=="N" & by_EUlev4=="N") {
	  total_alk_res$Metier_Gear="-1"
	} else if (by_EUlev6=="Y") {
	  total_alk_res$Metier_Gear=total_alk_res$foCatEu6_sc
	} else if (by_EUlev4=="Y") {
	  total_alk_res$Metier_Gear=total_alk_res$foCatEu4
	}
	
	if (by_quart=="N"){
	  total_alk_res$quarter="-1"
	}
	
	total_alk_res <- total_alk_res[, !(colnames(total_alk_res) %in% c("foCatEu6_sc", "foCatEu4")) ]
	
	total_alk_res_sum <- group_by(total_alk_res, Metier_Gear, quarter, samp_size,	n_it, lenCls, age, commCat)
	
	
	total_alk_res_sum <- data.frame(summarise(total_alk_res_sum,  no_ages = length(age) ))
	

	# dcast ALK from simulations according to settings
		
	total_alk_res_sum$no_ages <- as.numeric(as.character(total_alk_res_sum$no_ages))
	

	  total_alk_res_2 <- dcast(total_alk_res_sum, Metier_Gear + quarter  + commCat + lenCls + n_it + samp_size ~ age)
	write.table(total_alk_res_2,paste(res_dir,"/ALK_from_simulations.csv",sep=""),sep=";",row.names=F)
	

	# Creation of ALK for the sample data
	
	df0_noNAs <- df0[!is.na(df0$age), ]
	samp= df0_noNAs[,c("sampId","foCatEu6_sc","foCatEu4","quarter","commCat","lenCls","age")]
	
	# aggregate ALK from sample data according to the settings

	if (by_cat=="N"){
	  samp$commCat="-1"
	}
	
	if (by_EUlev6=="N" & by_EUlev4=="N") {
	  samp$Metier_Gear="-1"
	} else if (by_EUlev6=="Y") {
	  samp$Metier_Gear=samp$foCatEu6_sc
	} else if (by_EUlev4=="Y") {
	  samp$Metier_Gear=samp$foCatEu4
	}
	
	if (by_quart=="N"){
	  samp$quarter="-1"
	}
	
	samp <- samp[, !(colnames(samp) %in% c("foCatEu6_sc", "foCatEu4")) ]
	
	samp_ <- group_by(samp , Metier_Gear, quarter,	 lenCls, age, commCat) 

	ALK <- data.frame(summarise(samp_, no_ages = length(age)  ))
	
	
	ALK$samp_size=samp_sizes[1]
	ALK$n_it=-1
	
	# dcast ALK from sample data according to the settings
	ALK <- ALK[, c(1:5, 7:8, 6)]
	ALK_2 <- dcast(ALK, Metier_Gear + quarter  + commCat + lenCls + n_it + samp_size ~ age)

	# Creation of measured lengths from the sample data
	
		samp_length= df0[,c("sampId","foCatEu6_sc","foCatEu4","quarter","commCat","lenCls","age")]
		
		if (by_cat=="N"){
		  samp_length$commCat="-1"
		}
		
		if (by_EUlev6=="N" & by_EUlev4=="N") {
		  samp_length$Metier_Gear="-1"
		} else if (by_EUlev6=="Y") {
		  samp_length$Metier_Gear=samp_length$foCatEu6_sc
		} else if (by_EUlev4=="Y") {
		  samp_length$Metier_Gear=samp_length$foCatEu4
		}
		
		if (by_quart=="N"){
		  samp_length$quarter="-1"
		}
		
		samp_length <- samp_length[, !(colnames(samp_length) %in% c("foCatEu6_sc", "foCatEu4")) ]
		
		# aggregate measured from sample data according to the settings	
		samp_length_2 <- group_by(samp_length , Metier_Gear, quarter,	 lenCls,  commCat)  
	
	NO_measured_bylen <- data.frame(summarise(samp_length_2, total_lengths = length(lenCls)  ))

	categ=unique(total_alk_res_2$commCat)
	met=as.character(unique(total_alk_res_2$Metier_Gear))
	quart=as.character(unique(total_alk_res_2$quarter))
	

	for (mett in met) {
	  
	  for (quartt in quart) {
	    
	    for (catt in categ) {
	
	for (no_it in 1:n_sims) {
	  for (ss in 1:length(samp_sizes)) {
	    
	    alk_this <- total_alk_res_2[total_alk_res_2$n_it == no_it & total_alk_res_2$samp_size == samp_sizes[ss] & 
	                                  total_alk_res_2$Metier_Gear == mett  & total_alk_res_2$ quarter == quartt & 
	                                  total_alk_res_2$commCat == catt, !( colnames(total_alk_res_2) %in% c("n_it", "samp_size", "commCat", "Metier_Gear", "quarter") )]
	   
	   # 
	     alk_this$total_ages <-  rowSums(alk_this[,2:ncol(alk_this)], na.rm=T)
	   
	     NO_measured_bylen_this <- NO_measured_bylen[ NO_measured_bylen$Metier_Gear == mett  & NO_measured_bylen$ quarter == quartt & 
	                                                    NO_measured_bylen$commCat == catt, 
	                                                  !( colnames(NO_measured_bylen) %in% c( "commCat", "Metier_Gear", "quarter") )]
	      
	    alk_this <- merge(alk_this, NO_measured_bylen_this, ALL=T)
	    
	    if (nrow(alk_this) > 0) {
	     res_this = CV_ALK(alk_this)
	     
	     to_add <- data.frame(metier= mett, quarter = quartt , commCat = catt,  no_it = no_it, CV = unique(res_this$total_CV), sample_size = samp_sizes[ss])
	     
	     if (no_it == 1 & ss == 1 & mett==met[1] & quartt == quart[1] & catt == catt[1]) {
	       res_CV_ALK = to_add
	     } else {
	       res_CV_ALK = data.frame(rbind(res_CV_ALK, to_add))
	     }
	    }
	    
	  }
	  
	}
	      
	    }
	  }
	}
	
	
	
	for (mett in met) {
	  for (quartt in quart) {
	    for (catt in categ) {
	      
	      ALK_2_this <- ALK_2[ALK_2$n_it == -1 & ALK_2$samp_size == samp_sizes[1] & 
	                            ALK_2$Metier_Gear == mett  & ALK_2$ quarter == quartt & 
	                            ALK_2$commCat == catt, !( colnames(ALK_2) %in% c("n_it", "samp_size", "commCat", "Metier_Gear", "quarter") )]
	      
	      ALK_2_this$total_ages <-  rowSums(ALK_2_this[,2:ncol(ALK_2_this)], na.rm=T)
	      NO_measured_bylen_this <- NO_measured_bylen[ NO_measured_bylen$Metier_Gear == mett  & NO_measured_bylen$ quarter == quartt & 
	                                                     NO_measured_bylen$commCat == catt, 
	                                                   !( colnames(NO_measured_bylen) %in% c( "commCat", "Metier_Gear", "quarter") )]
	      
	      ALK_2_this <- merge(ALK_2_this, NO_measured_bylen_this, ALL=T)
	      
	      if (nrow(ALK_2_this) > 0) {
	        
	      res_ALK_from_sample = CV_ALK(ALK_2_this)
	      to_add <- data.frame(metier= mett, quarter = quartt , commCat = catt, 
	                           no_it = -1, CV = unique(res_ALK_from_sample$total_CV), sample_size = samp_sizes[1])
	      
	      if ( mett==met[1] & quartt == quart[1] & catt == catt[1]) {
	        res_CV_ALK_from_sample = to_add
	      } else {
	        res_CV_ALK_from_sample = data.frame(rbind(res_CV_ALK_from_sample, to_add))
	      }
	      }
	    }
	  }
	}
	

	# GROUP OF PLOTS NUMBER 1: calculate CV from sampled data
	
	res_CV_ALK<-data.frame(res_CV_ALK)
	
	for (mett in met) {
	  
	  for (quartt in quart){
	    
	    for (catt in categ){
	      
	      this <- res_CV_ALK[res_CV_ALK$commCat==catt & res_CV_ALK[,1]==mett & res_CV_ALK$quarter==quartt, ] 
	      
	      BASELINE_VALUE <- BASELINE_VALUE_tab$BASELINE_VAL[ BASELINE_VALUE_tab$commCat==catt & 
	                                                             BASELINE_VALUE_tab$ Metier_Gear==mett & 
	                                                             BASELINE_VALUE_tab$quarter==quartt]
	      
	      BASELINE_CV <-  res_CV_ALK_from_sample$CV[ res_CV_ALK_from_sample$commCat==catt & 
	                                                   res_CV_ALK_from_sample$metier==mett & 
	                                                   res_CV_ALK_from_sample$quarter==quartt ] 
	      
	      if (nrow(this)>0){
	        STEP = unique(this$sample_size)[2] - unique(this$sample_size )[1]
	         this <- this[order(as.numeric(as.character(this$CV)),decreasing =TRUE) , ]

	        b <- ggplot(this, aes(x = sample_size,y=CV))
	        b + geom_point(stat = "identity") +  xlab("") + ylab("")   + ylim(c(0,max(this$CV))) + 
	          theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
	          ggtitle(paste("cat=",catt, "Metier/Gear=", mett, "Quarter=", quartt))  + 
	          geom_hline(aes(yintercept = quantile(this$CV, probs =c(0.05) )),col=as.character("blue"),linetype="dashed") + 
	          geom_hline(aes(yintercept = quantile(this$CV, probs =c(0.25)) ),col=as.character("blue"),linetype="dashed") + 
	          geom_hline(aes(yintercept = quantile(this$CV, probs =c(0.5) )),col=as.character("blue"),linetype="dashed") + 
	          geom_hline(aes(yintercept = quantile(this$CV, probs =c(0.75) )),col=as.character("blue"),linetype="dashed") + 
	          geom_hline(aes(yintercept = quantile(this$CV, probs =c(0.95) )),col=as.character("blue"),linetype="dashed") +
	          geom_hline(aes(yintercept = BASELINE_CV),col=as.character("red")) + 
	          geom_text(aes(label = paste("Baseline [", BASELINE_VALUE, "]", sep="") , y = BASELINE_CV + 0.05, 
	                        x=max(this$sample_size)-STEP*0.5 ),	 position = position_dodge(0.9),	 vjust = 0 ) +
	          scale_x_continuous(breaks=unique(this$sample_size), labels= as.character(unique(this$sample_size)) )
	        ggsave(paste(res_dir,"/Cat_",as.character(catt),"_met",mett,"_Q",quartt,"_CV_age.jpg", sep=""), last_plot(),  
	               height=8, width=8) 
	      
	        
	        }
	    }
	    
	  }
	}
	

	
	
	#GROUP OF PLOTS NUMBER 2  
	
	ALK$Type="Sampling"
	total_alk_res_sum$Type="Simulations"
	
	overall <- total_alk_res_sum
	
	for (sp in samp_sizes) {
	  ALK$samp_size = sp
	  overall <- data.frame(	rbind(overall, ALK))
	}

# 	overall <- overall[order(overall$Type, decreasing =T), ]
	
	# sum by age for each iteration and sample size
	overall_2 <- group_by( overall, Type, commCat , Metier_Gear, quarter, samp_size, n_it,	age)  
	overall_2 <- data.frame(summarise(overall_2, numbers = sum(no_ages)  ))
	
	# mean numbers by age on the n iteration
	overall_3 <- group_by( overall_2, Type, commCat , Metier_Gear, quarter, samp_size, age)  
	overall_3 <- data.frame(summarise(overall_3, no_age = mean(numbers)  ))
	
	# sum of numbers by length
	overall_4 <- group_by( overall_3, Type, commCat , Metier_Gear, quarter, samp_size)  
	overall_4 <- data.frame(summarise(overall_4, total_no = sum(no_age)  ))
	
	overall_5 <- merge(overall_3, overall_4)
	
	overall_5$percNo_age <- overall_5$no_age/ overall_5$total_no
	
#	overall <- total_alk_res_sum
	
# 	for (sp in samp_sizes) {
# 	  ALK$samp_size = sp
# 	  overall <- data.frame(	rbind(overall, ALK))
# 	}
 	
	
	for (mett in met) {
	  for (quartt in quart) {
	    for (catt in categ) {
	      
	      this <- overall_5[overall_5$commCat==catt & overall_5$Metier_Gear==mett & overall_5$quarter==quartt, ] 
	      
	      if (nrow(this) > 0) {
	      b <- ggplot(this, aes(x = age, y=percNo_age, col=Type, type=Type))
	      b + geom_line() +  xlab("") + ylab("") +  facet_grid(.~samp_size)  + # stat = "identity"
	        
	        theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
	        ggtitle(paste("cat=",catt, "Metier/Gear=", mett, "Quarter=", quartt))  
	      ggsave(paste(res_dir,"/Cat ",catt,"_met",mett,"_Q",quartt,"_age_distribution.jpg", sep=""), last_plot(),  height=8, width=16) 
	      }
	    }
	  }
	}
	


	# SUMMARY TABLE
	# res_CV_ALK_from_sample contains the sample CV
	# res_CV_ALK contains the simulations
	
	summ_tab=aggregate(res_CV_ALK$CV,by=list(Metier_Gear=res_CV_ALK$metier,quarter=res_CV_ALK$quarter,samp_size=res_CV_ALK$sample_size,
	                                         commCat=res_CV_ALK$commCat),FUN="mean")
	colnames(summ_tab)[ncol(summ_tab)]="CV"  
	
	res_CV_ALK_from_sample$sample_size="Baseline"
	colnames(res_CV_ALK_from_sample)[colnames(res_CV_ALK_from_sample) == "sample_size"] = "samp_size"
	colnames(res_CV_ALK_from_sample)[colnames(res_CV_ALK_from_sample) == "metier"] = "Metier_Gear"
	
	res_CV_ALK_from_sample = res_CV_ALK_from_sample[,colnames(summ_tab)]
	
	tab=rbind(summ_tab,res_CV_ALK_from_sample)
	
	tab=tab[order(tab[,1]),]
	tab=tab[order(tab[,2]),]
	
	tab$EMD= 0
	
	for (i in c(1:nrow(tab)))
	  if (as.character(tab$samp_size[i])!="Baseline") {
	    {
	      ref=as.numeric(tab$CV[as.character(tab$samp_size) == "Baseline" & 
	                              as.character(tab$commCat)==as.character(tab$commCat[i]) & 
	                              as.character(tab$Metier_Gear)==as.character(tab$Metier_Gear[i]) & 
	                              as.character(tab$quarter)==as.character(tab$quarter[i]) ] )

	      ref_age_structure=overall_5[as.character(overall_5$samp_size) ==  as.character(samp_sizes[1]) & 
	                                    as.character(overall_5$Type) == "Sampling" & 
	                                    as.character(overall_5$commCat) == as.character(tab$commCat[i]) & 
	                                    as.character(overall_5$Metier_Gear) == as.character(tab$Metier_Gear[i]) & 
	                                    as.character(overall_5$quarter) == as.character(tab$quarter[i]) , c("age", "percNo_age") ] 
	    
	      this_age_structure=overall_5[as.character(overall_5$samp_size) == as.character(tab$samp_size[i]) & 
	                                     as.character(overall_5$Type) == "Simulations" &
	                                     as.character(overall_5$commCat) == as.character(tab$commCat[i]) & 
	                                     as.character(overall_5$Metier_Gear) == as.character(tab$Metier_Gear[i]) & 
	                                     as.character(overall_5$quarter) == as.character(tab$quarter[i]) , c("age", "percNo_age") ] 
	      
	      the_two_struct <- merge(ref_age_structure, this_age_structure, by =c("age") , all=T)
	      
	      the_two_struct[is.na(the_two_struct[,])] <- 0
	      
	      tab$EMD[i] <- round(emd2d(matrix(the_two_struct$percNo_age.x), matrix(the_two_struct$percNo_age.y) ), 2)
	      
	      } 
	    
	  }
	
	A <- suppressWarnings(merge(tab, BASELINE_VALUE_tab, all.x=T))
	A$samp_size[A$samp_size == "Baseline"] <- A$BASELINE_NAME[A$samp_size == "Baseline"]
	
	A <- A[order(A$Metier_Gear, A$quarter, A$commCat, A$samp_size),]
	
	write.table(A[, c(1:6)], paste(res_dir,"/Summary_table_age.csv", sep=""),sep=";",row.names=F)
	
	
