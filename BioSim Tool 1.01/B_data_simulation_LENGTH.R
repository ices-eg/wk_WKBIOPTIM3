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
  library(png)

# set the working directory
myWD <- "C:\\BioSim Tool 1.01"
setwd(myWD)

caseStudy_path <-"C:\\BioSim Tool 1.01"
species_name <- "Parapenaeus longirostris"  
short_name <-  "DPS" 
	
GSA <- "GSA99" # "ALL"

# stratification of sampleId definition
by_cat="N"  # "Y" or "N"
by_EUlev6="N"  # "Y" or "N"
by_EUlev4="Y"
by_quart="N"  # "Y" or "N"
	
	# read variable table
	variable_table <- read.csv2(paste(caseStudy_path, "\\input files\\B_variable_table.csv", sep=""), as.is=TRUE)
	
	# expliciting the number of iterations and the sample sizes
	n_sims <- 10
	samp_sizes <- c(seq(100,1000, by=100))
	# e.g. between the intermediate quantiles
	
	# set sampling design of sample data
	sampling_design <- list (stratified = FALSE, strata_var = "")
	
	sampling_options <- list (n_sims = n_sims, 
	                          stages="one", 				# no of stages
	                          stratified = FALSE, strata_var = "", 	# stratification details
	                          stage1_samp_size=NA, samp_sizes = samp_sizes, 		# samp sizes
	                          replacement=FALSE, 	sample_all_available = TRUE, 
	                          sample_all_available_warning = FALSE, # replacement options
	                          vars_to_keep = c(""))		

	SAVE_PLOTS_BY_TRIP <- TRUE
	
	# load input data
	load(paste(caseStudy_path, "\\A_data_preparation\\input_data.rdata", sep=""))

	# read functions
	source("funs-26102018.R")

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
	if(SAVE_PLOTS_BY_TRIP){
	dir.create(sim_dir)
	}
	
	df0 <- df0[df0$spp == species_name, ]
	
	if (GSA != "ALL") {
	  df0 <- df0[df0$area == GSA, ]
	}
	
	# Isabella 28082018: summary of sample size by trip Id (to be eventually adapted using sampId_Cat) 
	if (by_cat=="Y") {
	  df0$sampId=df0$sampId_cat
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
	
	
	df0_by_year <-  group_by(df0_temp_ , Metier_Gear,quarter,commCat, year )  
	df0_by_year <- data.frame(summarise(df0_by_year,  length_measures=length(lenCls)))
	
	df0_avg <-  group_by(df0_by_year , Metier_Gear,quarter,commCat )  
	df0_avg <- data.frame(summarise(df0_avg,  length_meas=round(mean( length_measures), 0)))
	
	df0_avg$year <- paste(min(df0_by_year$year), max(df0_by_year$year), sep="-")
	df0_avg <- df0_avg[ , c(1:3, 5, 4)]
	colnames(df0_avg)[5] <- 	colnames(df0_by_year)[5]
	past_situation <- data.frame(rbind(df0_by_year, df0_avg))
	write.table(past_situation,paste(res_dir,"/Past situation.csv", sep=""),sep=";",row.names=F)
	
	

#	TO CHECK the numbers of length measurement in the datasets
	
		tab=aggregate(df0$indivId,by=list(df0$sampId),FUN="length")	
	colnames(tab) =c("sampId","Num_ind") 
	write.table(tab, paste(res_dir,"/Numbers by sampId.csv", sep=""),sep=";",row.names=F)
	
	summary(tab[,2])
	
	quants <- summary(tab[,2])
	QUANTS <- data.frame(Min = as.numeric(quants[1]), qu_1st= as.numeric(quants[2]),  Median = as.numeric(quants[3]),  
	                         Mean = as.numeric(quants[4]), qu_3rd = as.numeric(quants[5]), Max = as.numeric(quants[6]) )
	write.table(QUANTS, paste(res_dir,"/Numbers by sampId_quantiles.csv", sep=""),sep=";",row.names=F)
	
	# setting of the minimum number of individuals considered representative
	min_n <- 1000
	# should cover all the sample size (as the bootstrap is done without replacement)
	
# =================	
# Sim definitions 
# =================
	
		table_select_samples<-table(df0$sampId)[table(df0$sampId)>=min_n]; 
	samples_to_analyze<-names(table_select_samples)		
		
# ======================
# Mode determination		
# ======================
	
	# wishlist:
		# include ouputs from mixdist package in LocalMaxima2
		# compare ouputs with Julia's find_mode()
		# rename "ls_auto_modes" to something more appropriate

		for (var1 in variable_table$variable)
			{
			print(var1)
			if (var1==variable_table$variable[1])
				{
				ls_auto_modes<-func_detect_modes_in_samples(x = droplevels(df0[df0$sampId %in% samples_to_analyze,]), variable = var1, original_class_span = variable_table[variable_table$variable == var1, "original_class_span"], smooth_class_span = variable_table[variable_table$variable == var1, "smooth_class_span"], min_proportion_to_accept_mode = variable_table[variable_table$variable == var1, "min_proportion_to_accept_mode"])	
				} else {
						ls_auto_modes<-list.merge(ls_auto_modes, func_detect_modes_in_samples(x = droplevels(df0[df0$sampId %in% samples_to_analyze,]), variable = var1, original_class_span = variable_table[variable_table$variable == var1, "original_class_span"], smooth_class_span = variable_table[variable_table$variable == var1, "smooth_class_span"], min_proportion_to_accept_mode = variable_table[variable_table$variable == var1, "min_proportion_to_accept_mode"])	)
						#ls_auto_modes<-list.merge(ls_auto_modes, func_detect_modes_in_samples(x = droplevels(df0[df0$sampId %in% "2029_999",]), variable = var1, original_class_span = variable_table[variable_table$variable == var1, "original_class_span"], smooth_class_span = variable_table[variable_table$variable == var1, "smooth_class_span"], min_proportion_to_accept_mode = variable_table[variable_table$variable == var1, "min_proportion_to_accept_mode"])	)
						}
		}

	# Isabella 28082018: commented *******************	
	# str(ls_auto_modes,3)
	# 
	# ls_auto_modes[[1]]$lenCls  # 2001_999
	# 
	# ls_auto_modes[["2001_999"]]$matStage
	# ls_auto_modes[["2001_999"]]$age
	# ls_auto_modes[["2001_999"]]$sex
	# ls_auto_modes[["2001_999"]]$mature
	# Isabella 28082018: commented *******************	
		

# Isabella 28082018: MODIFIED *******************		
# ======================
# Weight - Length Relationship		
# ======================
 coefs_weight_length<- c(a= NA, b=NA)	
# Isabella28082018: in case you want estimate the individual weight, enter the a and b		

	#	coefs_weight_length<-coef(lm(log(df0$indWt)~log(df0$lenCls)))
	# names(coefs_weight_length)<-c("a","b")

# Isabella 28082018: MODIFIED ******************	
	
# =======================
# Simulations	
# =======================

		# if your data is a simple random sample of individuals from which all biological variables were sampled you will be able to test all sampling strategies
		# if your data was stratified with regards to one of the variables you have to take that into account
			# you can mantain the stratification and look into the consequences of a reduction or increase in the number of individuals samples per strata
			# you can attemp to de-stratify, creating a pseudo random sample and then test scenarios in it

		# check			

	# creates a storage object
	ls_DT_compiled<-sapply(samples_to_analyze, function(x) NULL)
	
	ls_DT_compiled_lenStr<-sapply(samples_to_analyze, function(x) NULL)
	

	seed<-1
	set.seed(seed)
	ptc1<-Sys.time()	

	for (sampId in samples_to_analyze)
	{

	 # sampId = samples_to_analyze[1]
	print("==========================")
	print(sampId)
	print("==========================")
	
	# selects sample
		df1<-df0[df0$sampId == sampId,]
		# use this instead if you want to look at a specific sample
			#df1<-df0[df0$sampId == "2001_999",]

# ===============			
# Simulations of samples
# ===============
	if ( nrow(df1) > samp_sizes[length(samp_sizes)] ) {
	  sampling_options$samp_sizes <- c(samp_sizes, nrow(df1))
	} else {
	  	sampling_options$samp_sizes <- c(samp_sizes, (nrow(df1)+1))
	}
	
		
ls_sims1<-faz_sim_sample(sampDes = sampling_design, sampOpt = sampling_options, df1o = df1)	
		
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
	clusterExport(cl, varlist = c("make_summary_numeric","df1","ls_auto_modes","coefs_weight_length","localMaxima2","localMaxima","vars_numerical","vars_categorical"))		
	
	ls_auto_modes_sample<-ls_auto_modes[[sampId]]
	
	ls_DT_compiled_ss_lenStr <-vector(mode="list", length=length(sampling_options$samp_sizes))
	
	for (j in 1:length(sampling_options$samp_sizes))
			{
			if(!sampling_options$stratified & (sampling_options$stages =="one" | sampling_options$stages =="two")) # not stratified, one or two stages
				{
				print(paste("Processing sample size", sampling_options$samp_sizes[j]))	
				if(sampling_options$stages == "one") w <- "1st_Stage" else w <- "2nd_Stage"
				 for (variable in vars_numerical)
					{	
					print(paste(".",variable, sep=""))
				
					ls_sims_stats[[j]][[variable]] <- do.call("rbind",lapply(ls_sims1[[j]], function(x){make_summary_numeric(x[[w]], variable,
					                                               a= coefs_weight_length[["a"]], b=coefs_weight_length[["b"]])}))
					}
	
	# added 15 sept
	      	res_ <- do.call("rbind",lapply(ls_sims1[[j]], function(x){ make_summary_proportion_by_length(x[[w]], variable)}))
				res_$n_it <- do.call("rbind",strsplit(as.character(rownames(res_)), "[.]"))[,1]
				res_$samp_size <- sampling_options$samp_sizes[j] 
				res_ <- res_[, colnames(res_) != "lenCls.1"]	 # ---------------------
				
				#ls_sims_stats_lenStruct[[j]][["length_structure"]]<-res_
	 
				# adds weight estimate from lenCls to all variables
					if ("lenCls" %in% vars_numerical)
						{
						for (variable in c(vars_numerical, vars_categorical)[c(vars_numerical, vars_categorical) != "lenCls"] )
							{
						ls_sims_stats[[j]][[variable]]$estim_weight<-ls_sims_stats[[j]][["lenCls"]]$estim_weight
							}
						}
			
				}
	  ls_DT_compiled_ss_lenStr[[j]] <- res_	
	  
	  }  
			
			#browser()
	ls_DT_compiled_lenStr[[sampId]] <- ls_DT_compiled_ss_lenStr
	
	
				ls_DT_compiled[[sampId]]<-sapply(c(vars_categorical, vars_numerical), function(x) NULL)
				
				DT<-sapply(c(vars_categorical, vars_numerical), function(x) NULL)
				
				target_object = ls_sims_stats
				for (variable in c(vars_categorical, vars_numerical))
				{
				# compilation of results
				DT[[variable]]<-data.frame()
				for (i in names(target_object))
					{
					DT_sim<-data.frame(sampId = df1$sampId[1], sim = as.numeric(as.character(i)), target_object[[i]][[variable]])
					DT[[variable]]<-rbind(DT[[variable]], DT_sim)
					}
				ls_DT_compiled[[sampId]][[variable]]<-DT[[variable]]
				}
			stopCluster(cl)

			
# 			mean length,
# 			MWCV of lengths
# 			median length
# 			min length
# 			max length 
# 			number of sampled classes
# 			number of modes
			
		#	if (SAVE_PLOTS_BY_TRIP) {
			ylimite_cv = c(0,5)
			ylimite_mwcv = c(0,100)
			
			DT2<-ls_DT_compiled[[sampId]][[variable]]	
			plot_path <- paste(sim_dir, "/Sample_",sampId,"_1.png", sep="")
			
			jpeg(file=plot_path, width=40, height=48, bg="white", units="cm",res=200)
			par(mfrow=c(4,2))
			boxplot(mean~sim, data=DT2, main="mean of the sample", xlab = "sample size", las=2, cex.axis=1.1, cex.main=1.5)
			boxplot(min~sim, data=DT2, main="minimum of the sample", xlab = "sample size", las=2, cex.axis=1.1, cex.main=1.5)
			boxplot(max~sim, data=DT2, main="maximum of the sample", xlab = "sample size", las=2, cex.axis=1.1, cex.main=1.5)
			boxplot(median~sim, data=DT2, main="median of the sample", xlab = "sample size", las=2, cex.axis=1.1, cex.main=1.5)
		  # dev.off()	
		  # plot_path <- paste(sim_dir, "/Length/Sample_",sampId,"_2.png", sep="")
		 # jpeg(file=plot_path, width=36, height=21, bg="white", units="cm",res=200)
		 # par(mfrow=c(2,2))
		  boxplot(cv~sim, data=DT2, main="cv of the mean", xlab = "sample size", las=2, cex.axis=1.1, cex.main=1.5)
		  boxplot(MWCV~sim, data=DT2, main="MWCV of the sample", xlab = "sample size", las=2, cex.axis=1.1, cex.main=1.5)
		  boxplot(n_class_sampled~sim, data=DT2, main="No. length classes sampled", xlab = "sample size", las=2, cex.axis=1.1, cex.main=1.5)
	    boxplot(n_modes_smooth~sim, data=DT2, main="Number of modes (smooth)", xlab = "sample size", las=2, cex.axis=1.1, cex.main=1.5)
		  dev.off()
			
			}
		  
		  #} 	# close sampId
	
	
	for (sampId in samples_to_analyze)  {
	  if (sampId == samples_to_analyze[1]) {
	    total_lfd <- do.call("rbind", ls_DT_compiled_lenStr[[sampId]])
	  } else {
	    total_lfd <- rbind(total_lfd, do.call("rbind", ls_DT_compiled_lenStr[[sampId]]))
	  }
	}
	
	
	
	# ------------------------------- start NEW! (25/09/2018)

	if (by_cat=="Y"){
	  df0_ok=unique(df0[,c("sampId","commCat")])
	  total_lfd=merge(total_lfd,df0_ok,by=c("sampId")) 
	}
	
	if (by_EUlev6=="Y" | by_EUlev4=="Y" | by_quart=="Y")
	{
	  df0_ok=unique(df0[,c("sampId","foCatEu6_sc","foCatEu4","quarter")])
	  total_lfd=merge(total_lfd,df0_ok,by=c("sampId"))  
	  
	}
	
	if (by_cat=="N"){
	  total_lfd$commCat="-1"
	}
	
	if (by_EUlev6=="N" & by_EUlev4=="N") {
	  total_lfd$Metier_Gear="-1"
	} else if (by_EUlev6=="Y") {
	  total_lfd$Metier_Gear=total_lfd$foCatEu6_sc
	} else if (by_EUlev4=="Y") {
	  total_lfd$Metier_Gear=total_lfd$foCatEu4
	}
	
	if (by_quart=="N"){
	  total_lfd$quarter="-1"
	}
	
	total_lfd <- total_lfd[, !(colnames(total_lfd) %in% c("foCatEu6_sc", "foCatEu4")) ]
	
	
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
	
	tab$is_BASELINE = FALSE 
	tab$is_BASELINE[tab$sampId %in% samples_to_analyze ] = TRUE 
	
	#write.table(tab, file="test.csv", sep=";", row.names=F)
	
	BASELINE_VALUE_tab <- group_by(tab, commCat, Metier_Gear,quarter)
	BASELINE_VALUE_tab <- data.frame(summarise(BASELINE_VALUE_tab, BASELINE_VALUE= mean(Num_ind) ))
	BASELINE_VALUE_tab$BASELINE_VALUE <- round(BASELINE_VALUE_tab$BASELINE_VALUE, 0)
	
	BASELINE_VALUE_tab$BASELINE_VALUE[which(	BASELINE_VALUE_tab$BASELINE_VALUE %in% samp_sizes)] <- BASELINE_VALUE_tab$BASELINE_VALUE[which(	BASELINE_VALUE_tab$BASELINE_VALUE %in% samp_sizes)] +1
	
	BASELINE_VALUE_tab$BASELINE_NAME <- paste("Baseline [", BASELINE_VALUE_tab$BASELINE_VALUE , "]", sep="")
	
# 	BASELINE_NAME <-  paste("Baseline [", round(mean(	tab$Num_ind[tab$sampId %in% samples_to_analyze ] ) ,0) , "]", sep="")
# 	BASELINE_VALUE <-  round(mean(	tab$Num_ind[tab$sampId %in% samples_to_analyze ] ) ,0)
	
	total_lfd <- merge(total_lfd, BASELINE_VALUE_tab)
	
	total_lfd$samp_size = apply(total_lfd, 1,function(x) ifelse(!((as.numeric(x[7])) %in% samp_sizes), as.numeric(x[8]), as.numeric(x[7]) ))
	
	# total_lfd$samp_size[!(	total_lfd$samp_size %in% samp_sizes) ] <- BASELINE_VALUE
	
	total_lfd <- total_lfd[(total_lfd$samp_size %in% samp_sizes  ) | (!(	total_lfd$samp_size %in% samp_sizes) & 	total_lfd$n_it == 1), ]
	
	total_lfd_sum <- group_by(total_lfd , commCat, Metier_Gear, quarter,samp_size,	n_it, lenCls) 

#	write.table(total_lfd_sum,paste(res_dir,"/Length/Summary_table_by_length.csv",sep=""),sep=";",row.names=F)
	
	total_lfd_sum_ <- data.frame(summarise(total_lfd_sum, no_lengths= length(lenCls) ))
	
	# mean weighed CV	
# 	sigma_i<-sqrt(nrow(x)*as.matrix(prop.table(table(x[,variable]))*(1-prop.table(table(x[,variable])))))
# 	cv_i <- sigma_i / (nrow(x)*as.matrix(prop.table(table(x[,variable]))))
# 	MWCV<-round(sum(sigma_i)/nrow(x)*100,1)
	
	total_lfd_sum_2 <- group_by(total_lfd_sum_ , commCat, Metier_Gear, quarter, samp_size,	n_it)


	# mean weighed CV	
	total_lfd_sum_3 <- data.frame(summarise(total_lfd_sum_2, total_lengths= sum(no_lengths)  ))
	total_lfd_sum_4 <- merge(total_lfd_sum_2, total_lfd_sum_3)
	
	total_lfd_sum_4$prop <- total_lfd_sum_4$no_lengths/total_lfd_sum_4$total_lengths
	total_lfd_sum_4$compl_prop <- 1 -	total_lfd_sum_4$prop 
	

	total_lfd_sum_4$sigma_i <- with(total_lfd_sum_4, sqrt(total_lengths * prop*compl_prop ) )
	total_lfd_sum_4$cv_i <- with(total_lfd_sum_4, sigma_i / (total_lengths * prop)  )
	
	total_lfd_sum_5 <- group_by(total_lfd_sum_4 ,commCat, Metier_Gear, quarter,samp_size,	n_it)


	 
	# MWCV <- round(sum(sigma_i)/nrow(total_lfd_sum_)*100,1)
	total_lfd_sum_6 <- data.frame(summarise(total_lfd_sum_5, sum_sigma = sum(sigma_i), tot_lengths = mean(total_lengths)  ) )
	
	total_lfd_sum_6$MWCV <- with(total_lfd_sum_6, round(sum_sigma/tot_lengths * 100, 1) )
	
	dataset<-total_lfd_sum_6
	# colnames(dataset)[which(colnames(dataset)=="foCatEu6" | colnames(dataset)=="foCatEu4")] = "Metier_Gear"
	
	# Saving plots

	
	categ=unique(dataset$commCat)
	met=as.character(unique(dataset$Metier_Gear))
	quart=as.character(unique(dataset$quarter))
	
	
	
	dataset_by_length <- total_lfd_sum_4
#	colnames(dataset_by_length)[which(colnames(dataset_by_length)=="foCatEu6" | colnames(dataset_by_length)=="foCatEu4")] = "Metier_Gear"
	
	# Saving plots
	
	categ=unique(dataset_by_length$commCat)
	met=as.character(unique(dataset_by_length$Metier_Gear))
	quart=as.character(unique(dataset_by_length$quarter))
	
# 	write.table(total_lfd_sum,paste(res_dir,"/Length/Summary_table_by_length.csv",sep=""),sep=";",row.names=F)
  
  
 sss <- as.numeric(unique(dataset_by_length$samp_size) ) 

 
 iii <- unique(dataset_by_length$n_it) 
 EMD_df_head <- c("commCat", "Metier_Gear", "quarter", "samp_size", "n_it", "EMD")
 EMD_df <- data.frame(matrix(nrow=0, ncol=length(EMD_df_head)))
 colnames(EMD_df) <- EMD_df_head
 
 for (mett in met) {
   
   for (quartt in quart){
     
     for (catt in categ){
 
  for (sam_sa in sss) {
    for (it in iii) {
      
      BASELINE_VALUE <- BASELINE_VALUE_tab$BASELINE_VALUE[as.character(BASELINE_VALUE_tab$commCat)==catt & 
                                                                         as.character(BASELINE_VALUE_tab$ Metier_Gear)==mett & 
                                                                                        as.character(BASELINE_VALUE_tab$quarter)==quartt]
      
      if (length(BASELINE_VALUE) >0) {
      ref_sa <- dataset_by_length[dataset_by_length$samp_size == BASELINE_VALUE & 
                                   as.character(dataset_by_length$commCat)==catt & 
                                    as.character(dataset_by_length$Metier_Gear)==mett & 
                                    as.character(dataset_by_length$quarter)==quartt,]
      

      sim_this <- dataset_by_length[dataset_by_length$samp_size == sam_sa & 
                                    as.character(dataset_by_length$n_it) == it & 
                                     as.character(dataset_by_length$commCat) ==catt & 
                                     as.character(dataset_by_length$ Metier_Gear) ==mett & 
                                      as.character(dataset_by_length$quarter) ==quartt,]
      
      the_two_struct <- merge(ref_sa, sim_this, by =c("lenCls") , all=T)
      the_two_struct[is.na(the_two_struct[,])] <- 0
      
      if (nrow(the_two_struct) > 0) {
      emd_this <-  round(emd2d(matrix(the_two_struct$prop.x), matrix(the_two_struct$prop.y) ), 2) 
      to_add <-	data.frame(commCat=catt, Metier_Gear=mett, quarter=quartt, samp_size = sam_sa, n_it = it, EMD= emd_this )
      EMD_df <- data.frame(rbind(EMD_df, to_add))
      }
      }
      
    }
  }
     }
   }
 }

EMD_df <- EMD_df[!(EMD_df$samp_size %in% BASELINE_VALUE_tab$BASELINE_VALUE), ]


categ=as.character(unique(EMD_df$cat))
samp_size_vec <- as.character(unique(EMD_df$samp_size[EMD_df$samp_size %in% samp_sizes] )) #< 1000

categ=unique(dataset$commCat)
met=as.character(unique(dataset$Metier_Gear))
quart=as.character(unique(dataset$quarter))


for (mett in met){
  
  for (quartt in quart){
    
    for (catt in categ){
      
      this <- EMD_df[as.character(EMD_df$commCat) ==catt & 
                                    as.character(EMD_df$ Metier_Gear) ==mett & 
                                                   as.character(EMD_df$quarter) ==quartt, ] 
      
      if (nrow(this)>0){
        
  this$samp_size <- as.numeric(as.character(this$samp_size))
  this <- this[order(this$samp_size) , ]
  
  b <- ggplot(this, aes(x = samp_size, y=EMD, color=EMD, size=3))
  b + geom_point() +  xlab("") + ylab("")   +
    ylim(c(0,max(EMD_df$EMD))) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    ggtitle(paste(species_name , "- EMD - cat=",as.character(catt), " met=", mett, " quarter=", quartt)) + 
    scale_x_continuous(breaks=unique(this$samp_size), labels= as.character(unique(this$samp_size)) )
  
  ggsave(paste(res_dir, "/Cat_",as.character(catt),"_met",mett,"_Q",quartt,"_EMD.png", sep=""), last_plot(),  
         height=8, width=8) 
   
      }
      
    }
    
  }
  
}

# mean(	tab$Num_ind ) # [tab$sampId %in% samples_to_analyze ]

# commCat Metier_Gear quarter samp_size Statistic  Value

temp_999 <- group_by(EMD_df, samp_size , commCat, Metier_Gear, quarter)
temp_999_2 <- data.frame(summarise(temp_999, mean_EMD = mean(EMD)  ) )

temp_999_2$Statistic <- "EMD"
temp_999_2 <- temp_999_2[,c(2:4, 1, 6, 5)]

colnames(temp_999_2)[6] <- "Value"

	categ=unique(total_lfd_sum_6$commCat)
	met=as.character(unique(total_lfd_sum_6$Metier_Gear))
	quart=as.character(unique(total_lfd_sum_6$quarter))
	
	for (mett in met) {
	  
	  for (quartt in quart) {
	    
	    for (catt in categ) {
	      
	      BASELINE_VALUE <- BASELINE_VALUE_tab$BASELINE_VALUE[ as.character(BASELINE_VALUE_tab$commCat)==catt & 
	                                                              as.character(BASELINE_VALUE_tab$ Metier_Gear)==mett & 
	                                                                as.character(BASELINE_VALUE_tab$quarter)==quartt]
	      
	      this <- total_lfd_sum_6[as.character(total_lfd_sum_6$commCat) ==catt & 
	                                as.character(total_lfd_sum_6$Metier_Gear) ==mett & 
	                                as.character(total_lfd_sum_6$quarter) ==quartt, ] 
	      
	      this$samp_size <- as.numeric(as.character(this$samp_size))
	      this <- this[order(this$samp_size) , ]
	      
	      BASELINE_CV <-  this$MWCV[ this$samp_size  == BASELINE_VALUE ] 
	      
	    this <- this[ this$samp_size  != BASELINE_VALUE, ] 
	      
	      if (nrow(this)>0) {
	        
	  STEP = unique(this$samp_size)[2] - unique(this$samp_size )[1]
	  b <- ggplot(this, aes(x = samp_size, y= MWCV ))
	  b + geom_point(stat = "identity") +  xlab("sample size") + ylab("Mean weighted CV of the sample")   + ylim(c(0,max(total_lfd_sum_6$MWCV))) + 
	    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
	    ggtitle(paste(species_name, "- MWCV - Cat=",as.character(catt),"met=",mett,"quarter=",quartt)) + 
    geom_hline(aes(yintercept = quantile(total_lfd_sum_6$MWCV, probs =c(0.05) )),col=as.character("blue"),linetype="dashed") + 
    geom_hline(aes(yintercept = quantile(total_lfd_sum_6$MWCV, probs =c(0.25)) ),col=as.character("blue"),linetype="dashed") + 
	    geom_hline(aes(yintercept = quantile(total_lfd_sum_6$MWCV, probs =c(0.5) )),col=as.character("blue"),linetype="dashed") + 
	    geom_hline(aes(yintercept = quantile(total_lfd_sum_6$MWCV, probs =c(0.75) )),col=as.character("blue"),linetype="dashed") + 
	    geom_hline(aes(yintercept = quantile(total_lfd_sum_6$MWCV, probs =c(0.95) )),col=as.character("blue"),linetype="dashed") +
	    geom_hline(aes(yintercept = BASELINE_CV),col=as.character("red")) + 
	    geom_text(aes(label = paste("Baseline [", BASELINE_VALUE, "]", sep="") , y = BASELINE_CV + 0.05, x=max(this$samp_size)-STEP*0.5 ),
	      position = position_dodge(0.9),	 vjust = 0 ) +
	    scale_x_continuous(breaks=unique(this$samp_size), labels= as.character(unique(this$samp_size)) )

ggsave(paste(res_dir, "/Cat_",as.character(catt),"_met",mett,"_Q",quartt,"_MWCV.png", sep=""), last_plot(),  
       height=8, width=8) 
	  

	}  
	    }
	  }
	}
	
 total_lfd_sum_7 <- group_by(total_lfd_sum_6 ,samp_size,commCat, Metier_Gear , quarter)
 total_lfd_sum_7 <- data.frame(summarise(total_lfd_sum_7, avg_MWCV = mean(MWCV), total_lengths = mean(tot_lengths)  ) )
  # commCat Metier_Gear quarter samp_size Statistic  Value
 total_lfd_sum_7$Statistic <- "MWCV"
 temp_1 <- total_lfd_sum_7[, c(2:4, 1, 7, 5)]
 colnames(temp_1)[6] <- "Value"
 temp_1$Value <- round(temp_1$Value, 1)
 temp_2= data.frame(rbind(temp_999_2, temp_1) )
 temp_3 <- dcast(temp_2 ,  commCat + Metier_Gear + quarter  + samp_size  ~ Statistic)
 
 temp_3 <- merge( temp_3, BASELINE_VALUE_tab, all.x = T)
 
 temp_3$samp_size[temp_3$samp_size == temp_3$BASELINE_VALUE] <- temp_3$BASELINE_NAME[temp_3$samp_size == temp_3$BASELINE_VALUE]
 temp_3 <- temp_3[,1:6]
 
 temp_3$EMD <- round(temp_3$EMD, 3)
 colnames(temp_3)[colnames(temp_3) == "MWCV" ] <- "MWCV %"
 write.table(temp_3,paste(res_dir, "/Summary_table_on_overall_LFD.csv", sep=""),sep=";",row.names=F)
 
 
 # ----------------------------------------- end NEW! (25/09/2018)

 
lfd_sum_of_samples <- group_by(total_lfd , commCat, Metier_Gear, quarter, samp_size,	n_it, sampId, lenCls)
 lfd_sum_of_samples <- data.frame(summarise(lfd_sum_of_samples,  no_len=length(lenCls) ))

 lfd_sum_of_samples <- group_by(total_lfd , commCat, Metier_Gear, quarter, samp_size,	n_it, sampId, lenCls)  

 lfd_sum_of_samples <- data.frame(summarise(lfd_sum_of_samples,  no_len=length(lenCls) ))
 
 lfd_sum_of_samples <- merge(lfd_sum_of_samples, BASELINE_VALUE_tab)
 
 lfd_sum_of_samples$samp_size = apply(lfd_sum_of_samples, 1,function(x) ifelse(!((as.numeric(x[4])) %in% samp_sizes), as.numeric(x[9]), as.numeric(x[4]) ))
 
 #lfd_sum_of_samples$samp_size[!(lfd_sum_of_samples$samp_size %in% samp_sizes) ] <- BASELINE_VALUE
 
# EMD_df_head <- c("commCat", "Metier_Gear", "quarter", "samp_size", "n_it", "EMD")
 
 EMD_df <-	data.frame( matrix(nrow=0, ncol=7))
 colnames(EMD_df) <- c("commCat", "Metier_Gear", "quarter", "samp_size",	"sampId", "n_it", "EMD")
 
 categ=unique(lfd_sum_of_samples$commCat)
 met=as.character(unique(lfd_sum_of_samples$Metier_Gear))
 quart=as.character(unique(lfd_sum_of_samples$quarter))
 
 # length(samples_) * length(sss) * length(iii)
 
 for (mett in met) {
   
   for (quartt in quart) {
     
     for (catt in categ) {
       
       BASELINE_VALUE <- BASELINE_VALUE_tab$BASELINE_VALUE[ as.character(BASELINE_VALUE_tab$commCat)==catt & 
                                                                           as.character(BASELINE_VALUE_tab$ Metier_Gear)==mett & 
                                                                                          as.character( BASELINE_VALUE_tab$quarter)==quartt]
       
       samples_ <- unique(lfd_sum_of_samples$sampId[as.character(lfd_sum_of_samples$commCat)==catt & 
                                                                   as.character(lfd_sum_of_samples$ Metier_Gear)==mett & 
                                                                                  as.character(lfd_sum_of_samples$quarter)==quartt])
       
 for (sa in samples_) {
   lfd_sum_of_samples_sa <- lfd_sum_of_samples[as.character(lfd_sum_of_samples$sampId) == sa &
                                                              as.character(lfd_sum_of_samples$commCat) ==catt & 
                                                              as.character( lfd_sum_of_samples$Metier_Gear) ==mett & 
                                                                              as.character(lfd_sum_of_samples$quarter) ==quartt,]
   
   ref_sa <- lfd_sum_of_samples_sa[lfd_sum_of_samples_sa$samp_size == BASELINE_VALUE & lfd_sum_of_samples_sa$n_it == 1,]
   ref_sa$tot <- sum(ref_sa$no_len) 
   ref_sa$prop <- ref_sa$no_len / ref_sa$tot * 100
   
   
   sss <- unique(lfd_sum_of_samples_sa$samp_size)   
   iii <- unique(lfd_sum_of_samples_sa$n_it) 
   
   for (sam_sa in sss) {
     if (sam_sa != BASELINE_VALUE) {
     for (it in iii) {
       
       sim_this <- lfd_sum_of_samples_sa[lfd_sum_of_samples_sa$samp_size == sam_sa & lfd_sum_of_samples_sa$n_it == it,]
       
       sim_this$tot <- sum(sim_this$no_len) 
       sim_this$prop <- sim_this$no_len / sim_this$tot * 100
       
       the_two_struct <- merge(ref_sa, sim_this, by =c("lenCls") , all=T)
       the_two_struct[is.na(the_two_struct[,])] <- 0
       
       if (nrow(the_two_struct) >0) {
       emd_this <-  round(emd2d(matrix(the_two_struct$prop.x), matrix(the_two_struct$prop.y) ), 2) 
       to_add <-	data.frame(commCat=catt, Metier_Gear=mett, quarter=quartt, samp_size = sam_sa, sampId = sa, n_it = it, EMD= emd_this )
       EMD_df <- data.frame(rbind(EMD_df, to_add))
       }
     }
     }
   }
   
 }
       
     }
   }
 }
 

 EMD_df <- EMD_df[!(EMD_df$samp_size %in% BASELINE_VALUE_tab$BASELINE_VALUE), ]

 samp_size_vec <- as.character(unique(EMD_df$samp_size[EMD_df$samp_size %in% samp_sizes] )) #< 1000
 categ=unique(EMD_df$commCat)
 met=as.character(unique(EMD_df$Metier_Gear))
 quart=as.character(unique(EMD_df$quarter))
 
 for (mett in met) {
   
   for (quartt in quart) {
     
     for (catt in categ) {
       
   this <- EMD_df[as.character(EMD_df$commCat)==catt & 
                    as.character(EMD_df$quarter)==quartt & 
                    as.character(EMD_df$Metier_Gear)==mett & 
                    EMD_df$samp_size %in% samp_size_vec, ] # e == samp_size_vec[si] & 
   
   if (nrow(this) >0) {
     this <- this[order(as.numeric(as.character(this$EMD))) , ]
   
   b <- ggplot(this, aes(x = samp_size, y=EMD, color=EMD, size=3))
   b + geom_point() +  xlab("") + ylab("")   +
     ylim(c(0,max(EMD_df$EMD))) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
     ggtitle(paste(species_name, "- EMD by trip - Cat=",as.character(catt),"met=",mett,"quarter=",quartt)) +
     scale_x_continuous(breaks=unique(this$samp_size), labels= as.character(unique(this$samp_size)) )
   
     ggsave(paste(sim_dir, "/Cat_",as.character(catt),"_met",mett,"_Q",quartt,"_EMD_by_trip.png", sep=""), last_plot(),
            height=8, width=8)  
   }
  
   
 }
 }
}
 

#  commCat	Metier_Gear	quarter	samp_size	EMD	MWCV %

 temp_999 <- EMD_df  # [,c(5:7, 1:4 )]
 
 temp_999$Statistic <- "EMD"
 colnames(temp_999)[7] <- "Value"
 
 # Exploratory: MWCV to sample size relationship
 
 res_CV <-data.frame()
 for (i in 1:length(names(ls_DT_compiled)))	{    
   this <- ls_DT_compiled[[i]]$ lenCls
   res_CV<-rbind(res_CV, data.frame(sampId= this$sampId , sim = this$sim,   sample_size = this$n, MWCV = this$MWCV))		
 }
 res_CV$sampId=as.character(res_CV$sampId)
 
 
 if (by_cat=="Y"){
   df0_ok=unique(df0[,c("sampId","commCat")])
   res_CV=merge(res_CV,df0_ok,by=c("sampId")) 
 }
 
 if (by_EUlev6=="Y" | by_EUlev4=="Y" | by_quart=="Y")
 {
   df0_ok=unique(df0[,c("sampId","foCatEu6_sc","foCatEu4","quarter")])
   res_CV=merge(res_CV,df0_ok,by=c("sampId"))  
   
 }
 
 if (by_cat=="N"){
   res_CV$commCat="-1"
 }
 
 
 if (by_EUlev6=="N" & by_EUlev4=="N") {
   res_CV$Metier_Gear="-1"
 } else if (by_EUlev6=="Y") {
   res_CV$Metier_Gear=res_CV$foCatEu6_sc
 } else if (by_EUlev4=="Y") {
   res_CV$Metier_Gear=res_CV$foCatEu4
 }
 
 if (by_quart=="N"){
   res_CV$quarter="-1"
 }
 
 res_CV <- res_CV[, !(colnames(res_CV) %in% c("foCatEu6_sc", "foCatEu4")) ]
 
 
 write.table(res_CV,paste(sim_dir, "/MWCV values.csv", sep=""), sep=";", row.names=F)

 samp_size_vec <- as.character(unique(res_CV$sample_size[res_CV$sample_size %in% samp_sizes] )) #< 1000
 categ=unique(res_CV$commCat)
 met=as.character(unique(res_CV$Metier_Gear))
 quart=as.character(unique(res_CV$quarter))
 
# res_CV_without_real_sampled <- res_CV[res_CV$sample_size %in% samp_size_vec,]
 
 for (ss in 1:length(samp_size_vec) ) {
   
   for (mett in met) {
     
     for (quartt in quart) {
       
       for (catt in categ) {
                
     this <- res_CV[res_CV$sample_size==samp_size_vec[ss] & 
                      as.character(res_CV$commCat) ==catt & 
                                     as.character(res_CV$quarter) ==quartt & 
                                                    as.character(res_CV$Metier_Gear) ==mett, ] 
     if (nrow(this) >0) {
       
     this <- this[order(as.numeric(as.character(this$MWCV))) , ]
     
    this_aggr  <- group_by(this,  sampId, sample_size)
    this_aggr_2 <-  summarise(this_aggr,  q_05 =  quantile(MWCV, probs =c(0.05, 0.25, 0.5, 0.75, 0.95) )[1], 
                                q_25 =  quantile(MWCV, probs =c(0.05, 0.25, 0.5, 0.75, 0.95) )[2], 
                                q_5 =  quantile(MWCV, probs =c(0.05, 0.25, 0.5, 0.75, 0.95) )[3], 
                                q_75 =  quantile(MWCV, probs =c(0.05, 0.25, 0.5, 0.75, 0.95) )[4], 
                                q_95 =  quantile(MWCV, probs =c(0.05, 0.25, 0.5, 0.75, 0.95) )[5] )
     
     b <- ggplot(this_aggr_2, aes(x = sampId, ymin = `q_05`, lower = `q_25`, middle = `q_5`, upper = `q_75`, ymax = `q_95`))
     b + geom_boxplot(stat = "identity") +  xlab("") + ylab("")   + ylim(c(0,max(res_CV$MWCV))) + 
       theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
       ggtitle(paste(species_name, "- MWCV by trip - Cat=",as.character(catt),"met=",mett,"quarter=",quartt, " - sample size=", samp_size_vec[ss], sep="")) + 
       geom_hline(aes(yintercept = quantile(res_CV$MWCV, probs =c(0.05) )),col=as.character("blue"),linetype="dashed") + 
       geom_hline(aes(yintercept = quantile(res_CV$MWCV, probs =c(0.25)) ),col=as.character("blue"),linetype="dashed") + 
       geom_hline(aes(yintercept = quantile(res_CV$MWCV, probs =c(0.5) )),col=as.character("blue"),linetype="dashed") + 
       geom_hline(aes(yintercept = quantile(res_CV$MWCV, probs =c(0.75) )),col=as.character("blue"),linetype="dashed") + 
       geom_hline(aes(yintercept = quantile(res_CV$MWCV, probs =c(0.95) )),col=as.character("blue"),linetype="dashed") 
     
     ggsave(paste(sim_dir, "/Cat_",as.character(catt),"_met",mett,"_Q",quartt,"_MWCV_",samp_size_vec[ss],"_by_trip.png", 
                  sep=""), last_plot(),  height=8, width=8) 
     
     }
   }  
   
 }
 }

 }
 
 
 for (mett in met) {
   
   for (quartt in quart) {
     
     for (catt in categ) {
       
     this <- res_CV[ as.character(res_CV$commCat) ==catt & 
                                    as.character( res_CV$quarter) ==quartt & 
                                                    as.character(res_CV$Metier_Gear) ==mett & 
                       res_CV$sample_size %in% samp_size_vec, ] 
  
     if (nrow(this) >0) {
       
      this <- this[order(as.numeric(as.character(this$MWCV))) , ]
   
   this_aggr  <- group_by(this,  sampId, sample_size)
   this_aggr_2 <-  summarise(this_aggr,  q_05 =  quantile(MWCV, probs =c(0.05, 0.25, 0.5, 0.75, 0.95) )[1], 
                             q_25 =  quantile(MWCV, probs =c(0.05, 0.25, 0.5, 0.75, 0.95) )[2], 
                             q_5 =  quantile(MWCV, probs =c(0.05, 0.25, 0.5, 0.75, 0.95) )[3], 
                             q_75 =  quantile(MWCV, probs =c(0.05, 0.25, 0.5, 0.75, 0.95) )[4], 
                             q_95 =  quantile(MWCV, probs =c(0.05, 0.25, 0.5, 0.75, 0.95) )[5] )
   
   b <- ggplot(this_aggr_2, aes(x = sampId, ymin = `q_05`, lower = `q_25`, middle = `q_5`, upper = `q_75`, ymax = `q_95`))
   b + geom_boxplot(stat = "identity") + facet_grid(sample_size~.) +  xlab("") + ylab("")   + 
     ylim(c(0,max(res_CV$MWCV))) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
     ggtitle(paste(species_name, "- MWCV by trip - Cat=",as.character(catt),"met=",mett,"quarter=",quartt, sep=""))  + 
     geom_hline(aes(yintercept = quantile(res_CV$MWCV, probs =c(0.05) )),col=as.character("blue"),linetype="dashed") +
     geom_hline(aes(yintercept = quantile(res_CV$MWCV, probs =c(0.25)) ),col=as.character("blue"),linetype="dashed") +
     geom_hline(aes(yintercept = quantile(res_CV$MWCV, probs =c(0.5) )),col=as.character("blue"),linetype="dashed") + 
     geom_hline(aes(yintercept = quantile(res_CV$MWCV, probs =c(0.75) )),col=as.character("blue"),linetype="dashed") + 
     geom_hline(aes(yintercept = quantile(res_CV$MWCV, probs =c(0.95) )),col=as.character("blue"),linetype="dashed") 
   
   ggsave(paste(sim_dir, "/Cat_",as.character(catt),"_met",mett,"_Q",quartt,"_MWCV_by_trip.png", sep=""), 
          last_plot(),  height=24, width=16) 
     }
   
     }
   }
 }
 
 
 
 # Isabella 29082018: Exploratory: number of length classes to sample size relationship
 temp0=cbind(res_CV,rep("MWCV",nrow(res_CV)))
 rm(res_CV)
 
 
 
 res2 <-data.frame()
 for (i in 1:length(names(ls_DT_compiled)))	{    
   this <- ls_DT_compiled[[i]]$ lenCls
   res2<-rbind(res2, data.frame(sampId= this$sampId , sim = this$sim,   sample_size = this$n, n_class_sampled = this$n_class_sampled))		
 }
 res2$sampId=as.character(res2$sampId)
 
 
 if (by_cat=="Y"){
   df0_ok=unique(df0[,c("sampId","commCat")])
   res2=merge(res2,df0_ok,by=c("sampId")) 
 }
 
 if (by_EUlev6=="Y" | by_EUlev4=="Y" | by_quart=="Y")
 {
   df0_ok=unique(df0[,c("sampId","foCatEu6_sc","foCatEu4","quarter")])
   res2=merge(res2,df0_ok,by=c("sampId"))  
   
 }
 
 if (by_cat=="N"){
   res2$commCat="-1"
 }
 
 
 if (by_EUlev6=="N" & by_EUlev4=="N") {
   res2$Metier_Gear="-1"
 } else if (by_EUlev6=="Y") {
   res2$Metier_Gear=res2$foCatEu6_sc
 } else if (by_EUlev4=="Y") {
   res2$Metier_Gear=res2$foCatEu4
 }
 
 if (by_quart=="N"){
   res2$quarter="-1"
 }
 
 res2 <- res2[, !(colnames(res2) %in% c("foCatEu6_sc", "foCatEu4")) ]
 
 write.table(res2,paste(sim_dir, "/n_class_sampled.csv", sep=""), sep=";", row.names=F)
 
 
 for (ss in 1:length(samp_size_vec)) {
   
   for (mett in met) {
     
     for (quartt in quart) {
       
       for (catt in categ) {
         
         this <- res2[res2$sample_size==samp_size_vec[ss] & 
                        as.character(res2$commCat) == catt & 
                        as.character(res2$quarter) == quartt & 
                        as.character(res2$Metier_Gear) == mett, ] # e == samp_size_vec[si] & 
         
         if (nrow(this) >0) {
           
          this <- this[order(as.numeric(as.character(this$n_class_sampled))) , ]
     this_aggr  <- group_by(this,  sampId,sample_size)
     this_aggr_2 <-  summarise(this_aggr,  q_05 =  quantile(n_class_sampled, probs =c(0.05, 0.25, 0.5, 0.75, 0.95) )[1], 
                               q_25 =  quantile(n_class_sampled, probs =c(0.05, 0.25, 0.5, 0.75, 0.95) )[2], 
                               q_5 =  quantile(n_class_sampled, probs =c(0.05, 0.25, 0.5, 0.75, 0.95) )[3], 
                               q_75 =  quantile(n_class_sampled, probs =c(0.05, 0.25, 0.5, 0.75, 0.95) )[4], 
                               q_95 =  quantile(n_class_sampled, probs =c(0.05, 0.25, 0.5, 0.75, 0.95) )[5] )
     
     
     b <- ggplot(this_aggr_2, aes(x = sampId, ymin = `q_05`, lower = `q_25`, middle = `q_5`, upper = `q_75`, ymax = `q_95`))
     b + geom_boxplot(stat = "identity") + xlab("") + ylab("")   + ylim(c(0,max(res2$n_class_sampled))) +
       theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
       ggtitle(paste(species_name, "- Nb classes by trip - Cat=",as.character(catt),"met=",mett,"quarter=",quartt," - sample size=", samp_size_vec[ss])) +
       geom_hline(aes(yintercept = quantile(res2$n_class_sampled, probs =c(0.05) )),col=as.character("blue"),linetype="dashed") + 
       geom_hline(aes(yintercept = quantile(res2$n_class_sampled, probs =c(0.25)) ),col=as.character("blue"),linetype="dashed") + 
       geom_hline(aes(yintercept = quantile(res2$n_class_sampled, probs =c(0.5) )),col=as.character("blue"),linetype="dashed") + 
       geom_hline(aes(yintercept = quantile(res2$n_class_sampled, probs =c(0.75) )),col=as.character("blue"),linetype="dashed") +
       geom_hline(aes(yintercept = quantile(res2$n_class_sampled, probs =c(0.95) )),col=as.character("blue"),linetype="dashed") 
       
     ggsave(paste(sim_dir, "/Cat_",as.character(catt),"_met",mett,"_Q",quartt,"_nb_classes_",samp_size_vec[ss],"_by_trip.png",
                  sep=""), last_plot(),  height=8, width=8) 
     
         }
     }   
     }
   }
 }
 
 
 
 
 for (mett in met) {
   
   for (quartt in quart) {
     
     for (catt in categ) {
   
       this <- res2[as.character(res2$commCat) ==catt & 
                                   as.character(res2$quarter) ==quartt &  
                                                  as.character(res2$Metier_Gear) ==mett & 
                      res2$sample_size %in% samp_size_vec, ] # e == samp_size_vec[si] & 
      
       if (nrow(this) >0) {
         
        this <- this[order(as.numeric(as.character(this$n_class_sampled))) , ]
   this_aggr  <- group_by(this,  sampId,sample_size)
   this_aggr_2 <-  summarise(this_aggr,  q_05 =  quantile(n_class_sampled, probs =c(0.05, 0.25, 0.5, 0.75, 0.95) )[1], 
                             q_25 =  quantile(n_class_sampled, probs =c(0.05, 0.25, 0.5, 0.75, 0.95) )[2], 
                             q_5 =  quantile(n_class_sampled, probs =c(0.05, 0.25, 0.5, 0.75, 0.95) )[3], 
                             q_75 =  quantile(n_class_sampled, probs =c(0.05, 0.25, 0.5, 0.75, 0.95) )[4], 
                             q_95 =  quantile(n_class_sampled, probs =c(0.05, 0.25, 0.5, 0.75, 0.95) )[5] )
   
   
   b <- ggplot(this_aggr_2, aes(x = sampId, ymin = `q_05`, lower = `q_25`, middle = `q_5`, upper = `q_75`, ymax = `q_95`))
   b + geom_boxplot(stat = "identity") + facet_grid(sample_size~.) +  xlab("") + ylab("")   +
     ylim(c(0,max(res2$n_class_sampled))) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
     ggtitle(paste(species_name, "- Nb classes by trip - Cat=",as.character(catt),"met=",mett,"quarter=",quartt)) +
     geom_hline(aes(yintercept = quantile(res2$n_class_sampled, probs =c(0.05) )),col=as.character("blue"),linetype="dashed") + 
     geom_hline(aes(yintercept = quantile(res2$n_class_sampled, probs =c(0.25)) ),col=as.character("blue"),linetype="dashed") + 
     geom_hline(aes(yintercept = quantile(res2$n_class_sampled, probs =c(0.5) )),col=as.character("blue"),linetype="dashed") + 
     geom_hline(aes(yintercept = quantile(res2$n_class_sampled, probs =c(0.75) )),col=as.character("blue"),linetype="dashed") + 
     geom_hline(aes(yintercept = quantile(res2$n_class_sampled, probs =c(0.95) )),col=as.character("blue"),linetype="dashed") 
   
   ggsave(paste(sim_dir, "/Cat_",as.character(catt),"_met",mett,"_Q",quartt,"_nb_classes_by_trip.png", sep=""), last_plot(),  height=16, width=16) 
       }
     }
   }
 }
 
 temp1=cbind(res2,rep("n_class_sampled",nrow(res2)))
 
 rm(res2)	
 
 # Isabella 29082018: Exploratory: mean length to sample size relationship
 
 res3 <-data.frame()
 
 for (i in 1:length(names(ls_DT_compiled)))	{    
   this <- ls_DT_compiled[[i]]$ lenCls
   res3<-rbind(res3, data.frame(sampId= this$sampId , sim = this$sim,   sample_size = this$n, 
                                n_modes_correct = this$n_modes_correct))		
 }
 res3$sampId=as.character(res3$sampId)
 
 
 
 if (by_cat=="Y"){
   df0_ok=unique(df0[,c("sampId","commCat")])
   res3=merge(res3,df0_ok,by=c("sampId")) 
 }
 
 if (by_EUlev6=="Y" | by_EUlev4=="Y" | by_quart=="Y")
 {
   df0_ok=unique(df0[,c("sampId","foCatEu6_sc","foCatEu4","quarter")])
   res3=merge(res3,df0_ok,by=c("sampId"))  
   
 }
 
 if (by_cat=="N"){
   res3$commCat="-1"
 }
 
 
 if (by_EUlev6=="N" & by_EUlev4=="N") {
   res3$Metier_Gear="-1"
 } else if (by_EUlev6=="Y") {
   res3$Metier_Gear=res3$foCatEu6_sc
 } else if (by_EUlev4=="Y") {
   res3$Metier_Gear=res3$foCatEu4
 }
 
 if (by_quart=="N"){
   res3$quarter="-1"
 }
 
 res3 <- res3[, !(colnames(res3) %in% c("foCatEu6_sc", "foCatEu4")) ]
 
 write.table(res3,paste(sim_dir, "/n_modes_correct.csv", sep=""), sep=";", row.names=F)
 
 
 
 for (ss in 1:length(samp_size_vec)) {
   
   for (mett in met) {
     
     for (quartt in quart) {
       
       for (catt in categ) {
         
         this <- res3[res3$sample_size==samp_size_vec[ss] & 
                        as.character(res3$commCat)==catt & 
                        as.character(res3$quarter) ==quartt & 
                        as.character(res3$Metier_Gear) ==mett, ] # e == samp_size_vec[si] & 
         if (nrow(this) >0) {
           
     this <- this[order(as.numeric(as.character(this$n_modes_correct))) , ]
     
     this_aggr  <- group_by(this, sampId, sample_size)
     this_aggr_2 <-  summarise(this_aggr,  q_05 =  quantile(n_modes_correct, probs =c(0.05, 0.25, 0.5, 0.75, 0.95))[1], 
                               q_25 =  quantile(n_modes_correct, probs =c(0.05, 0.25, 0.5, 0.75, 0.95) )[2], 
                               q_5 =  quantile(n_modes_correct, probs =c(0.05, 0.25, 0.5, 0.75, 0.95) )[3], 
                               q_75 =  quantile(n_modes_correct, probs =c(0.05, 0.25, 0.5, 0.75, 0.95) )[4], 
                               q_95 =  quantile(n_modes_correct, probs =c(0.05, 0.25, 0.5, 0.75, 0.95) )[5] )
     
     b <- ggplot(this_aggr_2, aes(x = sampId, ymin = `q_05`, lower = `q_25`, middle = `q_5`, upper = `q_75`, ymax = `q_95`))
     b + geom_boxplot(stat = "identity") +  xlab("") + ylab("")   + ylim(c(0,max(res3$n_modes_correct))) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
       ggtitle(paste(species_name, "- Nb modes correct by trip - Cat=",as.character(catt),"met=",mett,"quarter=",quartt," - sample size=" ,samp_size_vec[ss] )) +
       geom_hline(aes(yintercept = quantile(res3$n_modes_correct, probs =c(0.05) )),col=as.character("blue"),linetype="dashed") + 
       geom_hline(aes(yintercept = quantile(res3$n_modes_correct, probs =c(0.25)) ),col=as.character("blue"),linetype="dashed") + 
       geom_hline(aes(yintercept = quantile(res3$n_modes_correct, probs =c(0.5) )),col=as.character("blue"),linetype="dashed") + 
       geom_hline(aes(yintercept = quantile(res3$n_modes_correct, probs =c(0.75) )),col=as.character("blue"),linetype="dashed") + 
       geom_hline(aes(yintercept = quantile(res3$n_modes_correct, probs =c(0.95) )),col=as.character("blue"),linetype="dashed") 
     
     ggsave(paste(sim_dir, "/Cat_",as.character(catt),"_met",mett,"_Q",quartt,"_n_modes_correct_",samp_size_vec[ss] , 
                  "_by_trip.png", sep=""), last_plot(),  height=8, width=8) 
         }
     }         
     }
   }
 }
 
 
 
 for (mett in met) {
   
   for (quartt in quart) {
     
     for (catt in categ) {
       
       this <- res3[ as.character(res3$commCat)==catt & 
                       as.character(res3$quarter)==quartt &  
                       as.character(res3$Metier_Gear)==mett & 
                       res3$sample_size %in% samp_size_vec, ] 
       if (nrow(this) >0) {
         
       
        this <- this[order(as.numeric(as.character(this$n_modes_correct))) , ]
   
   
   this_aggr  <- group_by(this,  sampId,sample_size)
   this_aggr_2 <-  summarise(this_aggr,  q_05 =  quantile(n_modes_correct, probs =c(0.05, 0.25, 0.5, 0.75, 0.95))[1], 
                             q_25 =  quantile(n_modes_correct, probs =c(0.05, 0.25, 0.5, 0.75, 0.95) )[2], 
                             q_5 =  quantile(n_modes_correct, probs =c(0.05, 0.25, 0.5, 0.75, 0.95) )[3], 
                             q_75 =  quantile(n_modes_correct, probs =c(0.05, 0.25, 0.5, 0.75, 0.95) )[4], 
                             q_95 =  quantile(n_modes_correct, probs =c(0.05, 0.25, 0.5, 0.75, 0.95) )[5] )
 
   # 
   b <- ggplot(this_aggr_2, aes(x = sampId, ymin = `q_05`, lower = `q_25`, middle = `q_5`, upper = `q_75`, ymax = `q_95`))
   b + geom_boxplot(stat = "identity") + facet_grid(sample_size~.) +  xlab("") + ylab("")   + 
     ylim(c(0,max(res3$n_modes_correct))) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
     ggtitle(paste(species_name, "- Nb modes correct by trip - Cat=",as.character(catt),"met=",mett,"quarter=",quartt )) +
     geom_hline(aes(yintercept = quantile(res3$n_modes_correct, probs =c(0.05) )),col=as.character("blue"),linetype="dashed") + 
     geom_hline(aes(yintercept = quantile(res3$n_modes_correct, probs =c(0.25)) ),col=as.character("blue"),linetype="dashed") + 
     geom_hline(aes(yintercept = quantile(res3$n_modes_correct, probs =c(0.5) )),col=as.character("blue"),linetype="dashed") + 
     geom_hline(aes(yintercept = quantile(res3$n_modes_correct, probs =c(0.75) )),col=as.character("blue"),linetype="dashed") + 
     geom_hline(aes(yintercept = quantile(res3$n_modes_correct, probs =c(0.95) )),col=as.character("blue"),linetype="dashed") 
   
   ggsave(paste(sim_dir, "/Cat_",as.character(catt),"_met",mett,"_Q",quartt,"_n_modes_correct_by_trip.png", sep=""), last_plot(),  height=16, width=16) 
       }
     }
   }
 }
 
 temp2=cbind(res3,rep("n_modes_correct",nrow(res3)))
 rm(res3)
 # Isabella 29082018: Summary table
 
 colnames(temp0)[ncol(temp0)]="Statistic"
 colnames(temp1)[ncol(temp1)]="Statistic"
 colnames(temp2)[ncol(temp2)]="Statistic"
 colnames(temp0)[4]="Value"
 colnames(temp1)[4]="Value"
 colnames(temp2)[4]="Value"
 
 temp_999 <- temp_999[, c(5, 4, 4, 7, 1,3,2, 8)]
  colnames(temp_999) = colnames(temp0)
temp_999$sample_size = 	as.numeric(as.character(temp_999$sample_size)) 
 tot=rbind(temp0,temp1,temp2,temp_999)
 
 tot1 = tot[tot$sample_size %in% samp_sizes, ]
 
   tab1=aggregate(tot1$Value,by=list(commCat = tot1$commCat, Metier_Gear = tot1$Metier_Gear, quarter=tot1$quarter, 
                                     sample_size=tot1$sample_size,Statistic=tot1$Statistic),FUN="mean")
 
 tot2=tot[!tot$sample_size %in% samp_sizes, ]
 

   tab2=aggregate(tot2$Value,by=list(commCat = tot2$commCat, Metier_Gear = tot2$Metier_Gear, quarter=tot2$quarter,
                                     Statistic=tot2$Statistic),FUN="mean")
   tab3=aggregate(tot2$sample_size,by=list(commCat = tot2$commCat, Metier_Gear = tot2$Metier_Gear, quarter=tot2$quarter
                                           ,Statistic=tot2$Statistic),FUN="mean")

   mer1=merge(tab2,tab3,by=c("commCat", "Metier_Gear", "quarter","Statistic"))
  
   # tab4=original sample
   
   # tab1=simulated sample
   
    tab4=mer1[,c(1:3,6,4:5)]
   colnames(tab4)=colnames(tab1)

 colnames(tab4)[ncol(tab4)]="Value"	
 colnames(tab1)[ncol(tab1)]="Value"
 

 
 if (FALSE) {
 # Missing categories (it happens when the actual sample sizes is higher than the n_min)
 if (by_cat == "Y") {
   
   met=as.character(unique(AAA$Metier_Gear))
   quart=as.character(unique(AAA$quarter))
   
   for (mett in met) {
      for (quartt in quart) {
       
        tab4_this <- tab4[as.character(tab4$Metier_Gear) == mett & as.character(tab4$quarter) == quartt, ]
        tab1_this <-  tab1[as.character(tab1$Metier_Gear) == mett & as.character(tab1$quarter) == quartt, ] 
        
     categ <- unique(AAA$commCat[as.character(AAA$Metier_Gear) == mett & as.character(AAA$quarter) == quartt ])
     categ_NA <- unique(AAA$commCat[as.character(AAA$Metier_Gear) == mett & as.character(AAA$quarter) == quartt & is.na(AAA$sample_size.x)])
     miss_cat = categ_NA 
   
   if (length(miss_cat)>0) {
     tab4_m_t=data.frame()
     
     for (ii in 1: length(miss_cat)){
       tab4_m = tab4_this[tab4_this$commCat==tab4_this$commCat[1],]
       tab4_m$tab4_this = miss_cat[ii]
       tab4_m$sample_size=max(samp_sizes)
       
       tab4_m[tab4_m$Statistic=="MWCV",]$Value = tab1_this[tab1_this$Statistic=="MWCV" & 
                                                           tab1_this$sample_size==max(samp_sizes) &
                                                           as.character(tab1_this$commCat)==miss_cat[ii],]$Value
       
       tab4_m[tab4_m$Statistic=="n_modes_correct",]$Value=tab1_this[tab1_this$Statistic=="n_modes_correct" & 
                                                                      tab1_this$sample_size==max(samp_sizes) & 
                                                                      as.character(tab1_this$commCat)==miss_cat[ii],]$Value
       
       tab4_m[tab4_m$Statistic=="n_class_sampled",]$Value=tab1_this[tab1_this$Statistic=="n_class_sampled" & 
                                                                      tab1_this$sample_size==max(samp_sizes) & 
                                                                      as.character(tab1_this$commCat)==miss_cat[ii],]$Value
       tab4_m_t=rbind(tab4_m,tab4_m_t)
       
     }
     
     tab4_m_t$commCat <- tab4_m_t$tab4_this
     tab4_m_t <- tab4_m_t[, c(1:6)]
     tab4=rbind(tab4,tab4_m_t)
   } 
   
 }
 }
 }
 
 }
 
 
 tab4$sample_size=max(tab1$sample_size)+1
 
 tab=rbind(tab1,tab4)
 
 tab$sample_size = as.numeric(	tab$sample_size)
 

   tab=tab[order(tab$Metier_Gear, tab$quarter, tab$commCat,  tab$Statistic, tab$sample_size),]

 
 tab$sample_size [tab$sample_size  == (max(tab1$sample_size)+1) ] = "Baseline"
 
 tab[tab$Statistic=="n_class_sampled",]$Value=round(tab[tab$Statistic=="n_class_sampled",]$Value,0)
 tab[tab$Statistic=="n_modes_correct",]$Value=round(tab[tab$Statistic=="n_modes_correct",]$Value,0)
 tab[tab$Statistic=="MWCV",]$Value=round(tab[tab$Statistic=="MWCV",]$Value,1)
 
 tab_final <- dcast(tab ,  commCat + Metier_Gear + quarter  + sample_size  ~ Statistic)
 
 tab_final <- merge(tab_final, BASELINE_VALUE_tab)
 
 tab_final$sample_size[tab_final$sample_size == "Baseline"] <- tab_final$BASELINE_VALUE[tab_final$sample_size == "Baseline"]
 
 colnames(tab_final)[colnames(tab_final) == "MWCV" ] <- "MWCV %"
 
 tab_final <- tab_final[order( tab_final$commCat,	tab_final$Metier_Gear,	tab_final$quarter, as.numeric(tab_final$sample_size)), ]
 tab_final$sample_size[!(tab_final$sample_size %in% samp_sizes) ] <- tab_final$BASELINE_NAME[!(tab_final$sample_size %in% samp_sizes) ]
 
 tab_final[,5] <- round( tab_final[,5] ,0)
 tab_final[,6] <- round( tab_final[,6] ,0)
 tab_final[,7] <- round( tab_final[,7] ,0)
 tab_final[,8] <- round( tab_final[,8] ,2)
 
 write.table(tab_final[, c(1:8)],paste(sim_dir, "/Summary_table_by_trip.csv", sep=""),sep=";",row.names=F)

 save.image(file=paste(res_dir, "\\B_length_output.RData", sep=""))		
	
