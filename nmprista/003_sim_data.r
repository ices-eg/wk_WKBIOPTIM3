
# 2016-10-30: adaptacao do script para casos em que var2=""
# 2017-06-08: import script from WKSDO to WKBIOPTIM
# 2017-06-08: removed implementation of localMaxima that was giving some problems; new localMaxima2 solves problems of plateaus while ascending and plateaus in maxima 
# 2017-06-10: removed implementation of localMaxima that was giving some problems; new localMaxima2 solves problems of plateaus while ascending and plateaus in maxima 
# 2017-06-10: fixed error in sims sample size per strata
# 2017-06-11: added target_var3 [version V3]
# 2017-06-11: added sample_size dependency on number of length classes
# 2017-06-12: significant change to simulation algorithm: simplification and adaptation to two stage sampling [version V4] and remaining types of sampling [version V5]
# 2017-06-18: creations of faz_sim_sample and significant other changes and simplifications [version V7]
# 2017-06-19: reviewed code, added graphs; added median to summary
# 2017-06-19: adaptated to RDB CA
# 2017-06-21: fix one issue with mode determination during wkbioptim
# 2017-06-22: simplification, for cycle 
# 2017-06-28: ?? [version V9]
# 2017-08-XX: improved annotation and handling of results [version V10]
# 2017-08-XX: exploratory analysis of smooth and proportion for mode acceptance moved outside loop [version V10]
# 2017-08-XX: included parallel processing to speed up time ~ 5 times faster [version V11]
# 2017-09-15: open project Shrimp RCM MED [Maria Falciani] 
# 2017-09-15: added lookup conversion table for column names
# 2017-09-15: added check on var distribution of samples selected for simulation
# 2018-05-22: renamed to "002_sim_data.r" [previously "teste2_v11_share_pand_clean.r"]
# 2018-05-22: renamed var sample_id->sampId
# 2018-05-22: added check of consequences of min_n selection [now also in no of samples]
# 2018-05-22: added automatic detection of original_class_span
# 2018-05-27: added automatic determination of weight-length parameters
# 2018-05-28: added mode tests on categorical variables
# 2018-09-10: annotation and directory structure improved
# 2018-09-10: renamed to 003_sim_data to match new directory structure
# 2018-09-10: moved sampling options to the outside of the loop (makes more sense, offers more flexibility if that is ever needed)
# 2018-09-12: improved model section:
				# added VBGF (from Patricia)
				# fixed output (function was not compiling model results)
				# added rules to issue different combinations of models
# 2019-05-25: new version of the script
				# adaptation to run sims in parallel (using pkg snowfall)
				# further speed up (using data.table)
				# deletion of leftovers of code
				# rename of several objects including final (now res_sims)
				# several other (minor)

		# Wishlist
			# couple sampling by weight [see fishPI algorithm...]
			# adapt to CA table
			# estratificacao desigual (e.g., 1 por class comp nos pequenos, todos os grandes)
			# improve format specification
			
			# add details of original design
			# add emdist
			# Kullback, S., Leibler, R.A. (1951). On information and sufficiency. Annals of Mathematical Statistics, 22: 79-86

			# add simulation of population from stratified samples (via hans gerritsen var?)

			# # Sampling of different number of individuals without replacement (sample size dependent of size classes in the sample)
				# tmp.n_classes<-length(ls_auto_modes[["Length_class"]][["original_breaks"]])
				# sampling_options = list (n_sims = 100, stratified = FALSE, replacement=FALSE, sample_all_available = TRUE, sample_all_available_warning = TRUE, stages="one", samp_sizes = c(1:5*tmp.n_classes), strata_var = "none", vars_to_keep = c("Length_class", "Weight", "Age", "Sex", "Maturity_stage", "Mature"))
				
			
# =================	
# Preamble
# =================			
			
	rm(list=ls())			

# read functions
	#source("sample_level_funs1.R") # contains "expl.analysis.smooth.and.modes", sim_sample

# read packages
	library(rlist) # list.merge
	library(snowfall)  # parallel computing
	library(data.table)  
	library(beepr) # beep
	
# load file
	load("001_Prepared_Inputs\\Input_data.Rdata")
	head(df0)

# read variable table
	variable_table <- read.csv2("000_Auxiliary_Tables\\variable_table.csv", as.is=TRUE)
	variable_table <- variable_table[1:2,]
	head(variable_table)
	
# =================	
# Sampling design
# =================

		# set sampling design of sample data
		sampling_design <- list (stratified = FALSE, strata_var = "")
		# CHECK IF DEFINED
		# sampling_design <- list (stratified = TRUE, strata_var = "lenCls")
		# sampling_design <- list (stratified = TRUE, strata_var = "matStage")
		# sampling_design <- list (stratified = TRUE, strata_var = "sex")
	
	
# =================	
# Define Sim population 
# =================

	# setting of the minimum number of individuals considered representative
		load("001_Prepared_Inputs\\Min_n.Rdata")
		table_select_samples<-table(df0$sampId)[table(df0$sampId)>=min_n]; 
		samples_to_analyze<-names(table_select_samples)		
		
		
# ======================
# Mode determination		
# ======================
	
	# wishlist:
		# include ouputs from mixdist package in LocalMaxima2
		# compare outputs with Julia's find_mode()
		# rename "ls_auto_modes" to something more appropriate

	source("000_Auxiliary_Funs\\func_detect_modes_in_samples.r")
	source("000_Auxiliary_Funs\\func_localMaxima2.r")
	source("000_Auxiliary_Funs\\func_localMaxima.r")
	#source("000_Auxiliary_Funs\\func_expl_analysis_smooth_and_modes.r")

			
		
		for (var1 in variable_table$variable)
			{
			#source("sample_level_funs1.R")
			print(var1)
			if (var1==variable_table$variable[1])
				{
				ls_original_modes<-func_detect_modes_in_samples(x = droplevels(df0[df0$sampId %in% samples_to_analyze,]), variable = var1, original_class_span = variable_table[variable_table$variable == var1, "original_class_span"], smooth_class_span = variable_table[variable_table$variable == var1, "smooth_class_span"], min_proportion_to_accept_mode = variable_table[variable_table$variable == var1, "min_proportion_to_accept_mode"])	
				} else {
						ls_original_modes<-list.merge(ls_original_modes, func_detect_modes_in_samples(x = droplevels(df0[df0$sampId %in% samples_to_analyze,]), variable = var1, original_class_span = variable_table[variable_table$variable == var1, "original_class_span"], smooth_class_span = variable_table[variable_table$variable == var1, "smooth_class_span"], min_proportion_to_accept_mode = variable_table[variable_table$variable == var1, "min_proportion_to_accept_mode"])	)
						}
			}
	
	# check object structure
	str(ls_original_modes,3)
	sampId <- "2017_2049"
	ls_original_modes[[sampId]]$lenCls
	ls_original_modes[[sampId]]$age
	# the following are not defined for example
	#ls_original_modes[["2017_2049"]]$matStage
	#ls_original_modes[["2017_2049"]]$sex
	#ls_original_modes[["2017_2049"]]$mature
	
	
# ======================
# Weight - Length Relationship		
# ======================

	# determines an overall [unweighed] Weight - Length Relationship to be used in determining weights of samples
		# automatic determination
			# this is a bit brute force but provides an approximation - check the parameters against known values and if needed define them manually
			coefs_weight_length<-coef(lm(log(df0$indWt)~log(df0$lenCls)))
			names(coefs_weight_length)<-c("a","b")
			coefs_weight_length
		# use code below if you was to define manually
			# coefs_weight_length <- c(a= -7.40, b=3.04)
	
# =======================
# Starting values for VBGF
# =======================

	# automatic
		require(FSA)
		start_values_vbgf<-unlist(vbStarts(lenCls~age, data = df0))
		names(start_values_vbgf)<-c("Linf_teo","K_teo","t0_teo")
	# manual
		start_values_vbgf<-c(Linf_teo = 140, K_teo = 0.40, t0_teo = 1.6)
		svTypical <- list(Linf = start_values_vbgf[["Linf_teo"]],K = start_values_vbgf[["K_teo"]],t0 = start_values_vbgf[["t0_teo"]]) ##Initial parameters to run the VBGM
		vbTypical <- lenCls~Linf*(1-exp(-K*(age-t0)))
		control<- nls.control(maxiter=100)
		fitTypical<-nls(vbTypical,data=df0,start=svTypical,control)
		start_values_vbgf<-c(Linf_teo = 215.6136, K_teo = 0.4365, t0_teo = -2.7327)
		
# =======================
# Simulation settings	
# =======================

		# if your data is a simple random sample of individuals from which all biological variables were sampled you will be able to test all sampling strategies
		# if your data was stratified with regards to one of the variables you have to take that into account. This means:
			# you can maintain the stratification and look into the consequences of a reduction (or increase, by recycling) in the number of individuals samples per strata
			# you can attempt to unstratify, creating a pseudo random sample and then test scenarios in it - this will only be simple if you are dealing with 1 variable only

			
	# sets sampling options	

		ls_sampling_options<-sapply(samples_to_analyze, function(x) NULL)
		for (sampId in samples_to_analyze)
			{
			ls_sampling_options[[sampId]] <- list (n_sims = 1, 
												stages="one", 																							# no of stages
													stratified = FALSE, strata_var = "", 																# stratification details
														#stage1_samp_size=NA, samp_sizes = c(10), 														# samp sizes
														stage1_samp_size = NA, samp_sizes = c(seq(10,90, by=10), nrow(df0[df0$sampId == sampId,])), 	# samp sizes
															replacement = TRUE, 	sample_all_available = FALSE, sample_all_available_warning = FALSE, # replacement options
																models = c("weight-length","VBGF"))														# models to run: "weight-length","sex-ratio","L50","VBGF"
			}
	
	
	# select number of sims
	nsim <- 500
	
	# select number of CPUs
		# use code below to check how many CPUs you have - it is better you leave some for other stuff your computer is running (I use 5/8 in my sims)
		# detectCores(all.tests = FALSE, logical = TRUE)
		n_CPUs <- 5
	
	# creates storage object	
		sim_res<-sapply(samples_to_analyze, function(x) NULL)	

# =======================
# Simulations
# =======================
		
for (sampId in samples_to_analyze)
	{
	# initiates time and sim counters 
	run <- which(sampId==samples_to_analyze)
	runs_left <- length(samples_to_analyze) - run

	# some prints [always good to know where you are...right?!]
		print("==========================")
		print(paste("Simulating ", sampId,": ", runs_left ," samples left in cue", sep=""))
		print("==========================")
	
	# initiates 
	if ( run == 1)
		{
		# initializes CPUs
		sfInit (parallel = TRUE, cpus = n_CPUs)  
		# exports main source
		sfSource("000_Auxiliary_Funs\\func_faz_sim_sample.r")
		sfSource("000_Auxiliary_Funs\\func_make_summary_numeric.r")
		sfSource("000_Auxiliary_Funs\\func_make_summary_categorical.r")
		sfSource("000_Auxiliary_Funs\\func_make_models_random.r")
		sfSource("000_Auxiliary_Funs\\func_lrPerc.r")
		sfSource("000_Auxiliary_Funs\\func_prn.r")	
		sfSource("000_Auxiliary_Funs\\func_do_sim_job.r")	
		sfSource("000_Auxiliary_Funs\\func_localMaxima2.r")	
		sfSource("000_Auxiliary_Funs\\func_localMaxima.r")	
		# exports global objects
		sfExport ("sampling_design", "variable_table", "ls_original_modes",'coefs_weight_length',"start_values_vbgf")
		# initiates time counter
		ptc_ini <- ptc <- Sys.time()
		} else ptc <- Sys.time()
	
	# subsets sample and sampling options
		df1<-df0[df0$sampId == sampId,]
		sampling_options<-ls_sampling_options[[sampId]]
	# simulates samples
		# exports sample objects to CPUs
			sfExport ("sampling_options", "df1", "sampId")
		# creates and exports seed to CPUs 
			seed<-1
			sfLapply(1:nsim, prn)
		# carries out simulations of sampId in CPUs
			out_sim<-sfLapply(1:nsim, do_sim_job)
		# cleans objects from sampId from CPUs
			sfRemove("sampling_options", "df1","sampId")
		
		# reorganizes list and 
		DT<-sapply(c(variable_table$variable,sampling_options$models), function(x) NULL)	
		for (variable in c(variable_table$variable,sampling_options$models))
				{
				# compilation of results
				DT[[variable]]<-data.table()
				for (i in 1:length(out_sim))
					{
					DT_sim<-data.table(out_sim[[i]][[variable]])
					DT[[variable]]<-rbind(DT[[variable]], DT_sim)
					}
				sim_res[[sampId]][[variable]]<-DT[[variable]]
				}
		# cleans objects from master
			rm(out_sim, sampling_options, df1)
			gc()
		# issues a motivating ping
			beep("ping")
		# gives processors a bit of rest
			Sys.sleep(5)
		# issues a few useful time estimates [...because everyone wants to go to lunch :P ]
			print(paste("time spent:",round(difftime(Sys.time(),ptc, units="mins"),2),"mins"))
			print(paste("average time spent:", round(difftime(Sys.time(),ptc_ini, units="mins")/(run),2),"mins")) 
			print(paste("estimate time left:", round(difftime(ptc <- Sys.time(), ptc_ini, units="mins")/run*(runs_left-1),2),"mins"))
	# finalizes	
		if(runs_left == 0){ 
			# cleans and stops CPUs	
			sfRemoveAll()
			sfStop()			
			# The comforting message
			print(paste("Done! total time: ", round(difftime(Sys.time(), ptc_ini, units="mins"),2),"mins", sep=""))		
			}
	}
	
	
# =======================
# Save results
# =======================
	
	save(df0, min_n, sim_res, nsim, sampling_design, ls_sampling_options, file=paste("003_Sim_Results\\Sim_results_",format(Sys.time(), "%Y%m%d%H%M"),".rdata", sep=""))

	



	

	


	
	