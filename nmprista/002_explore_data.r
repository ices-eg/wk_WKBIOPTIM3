# ====================	
# Exploratory Analyses 
# ====================	

# Nuno Prista, 2017-2019
# adapted to HER_SD_25 dataset

	# 2018-09-10: annotation and directory structure improved
	# 2018-09-10: renamed to 002_explore_data to match new directory structure
	# 2019-05-23: improved min_n determination
	# 2019-05-23: restructured and clarified steps and annotations

	# wishlist
		# if design 

	
	rm(list=ls())
	
	
	# read and display data
		load("001_Prepared_Inputs\\input_data.rdata")
		ls()
		head(df0)
	
	# No of samples
		cat("No. of samples:",length(unique(df0$sampId)), "\n")

	# No indiv per sample
		windows(7,5)
		barplot(table(table(df0$sampId)), xlab="sample size (indivs)", ylab = "frequency in original data", las=2)
		savePlot(file="002_Exploratory_Analyses\\000_Barplot_Sample_Size_Dist.png",type="png")
		
	# ==================	
	# quality checks
	# ==================
	
	# define bio variable of interest
		target_vars <- c("lenCls","age","sex","matStage","mature")
	
	# summary analyses
		for(variable in target_vars)
		{
		cat("--------", "\n")
		cat("-",variable,"-", "\n")
		cat("--------", "\n")
		cat("No. of NA in", variable,":",sum(is.na(df0[[variable]])), "\n")
		cat("%. of NA in", variable, ":",sum(is.na(df0[[variable]]))/nrow(df0)*100, "\n")
		cat("Max No. of fish in a sample:",max(table(df0$sampId)), "\n")
		cat("Min No. of fish in a sample:",min(table(df0$sampId)), "\n")
		cat("Max No. of NAs in a sample: ",max(tapply(df0[[variable]],df0$sampId, function(x){sum(is.na(x))})), "\n")
		cat("No samples with NAs: ",sum(tapply(df0[[variable]],df0$sampId, function(x){sum(is.na(x))})>0), "\n")
		cat("% samples with NAs: ",sum(tapply(df0[[variable]],df0$sampId, function(x){sum(is.na(x))})>0)/length(unique(df0$sampId))*100, "\n")
		if(variable %in% c("lenCls","age"))
			{
			cat("Min",variable,":",min(df0[[variable]], na.rm=T), "\n")
			cat("Max",variable,":",max(df0[[variable]], na.rm=T), "\n")
			cat("width of", variable,"(as auto detected):",median(diff(sort(unique(df0[[variable]])))), "\n") # NEW
			}
		}
	
	# barplot of all variables
		for(variable in target_vars)
		{
		if(variable %in% c("lenCls","age")) {niveis<-seq(min(df0[[variable]],na.rm=T), max(df0[[variable]],na.rm=T), by=median(diff(sort(unique(df0[[variable]])))))} else {niveis=unique(sort(df0[[variable]]))}
		if (variable == "lenCls") {xname = "length class"; yname = "No. of individuals"; titlename = "Length distribution - all samples combined"}
		if (variable == "age") {xname = "age class"; yname = "No. of individuals"; titlename = "Age distribution - all samples combined"}
		if (variable == "sex") {xname = "sex"; yname = "No. of individuals"; titlename = "Sex distribution - all samples combined"}
		if (variable == "matStage") {xname = "matStage"; yname = "No. of individuals"; titlename = "matStage distribution - all samples combined"}
		if (variable == "mature") {xname = "mature"; yname = "No. of individuals"; titlename = "mature distribution - all samples combined"}
		barplot(table(factor(df0[[variable]], levels=niveis)), las=2, cex.names=0.7, xlab = xname, ylab =yname, main =titlename)
		
		savePlot(filename = paste("002_Exploratory_analyses\\001_Barplot_All_",variable,".png", sep=""), type = "png")
		graphics.off()
		}
	
	# example of a simulation on the entire dataset
		# you need to change the sampleId and samp_sizes
		
		# note: can produce many graphs so it is good to define target_sampId manually
		# do not run
			#target_sampId<-unique(df0$sampId)
		
		target_sampId<-c("2014_2016")
		samp_sizes <- c(200, 150, 100, 50)
		
		for (variable in target_vars)
		{
		if (variable %in% c("lenCls","age")) {niveis = seq(min(df0[[variable]], na.rm=T), max(df0[[variable]], na.rm=T), by=median(diff(sort(unique(df0[[variable]]))))) }
		if (variable %in% c("sex","matStage","mature")) {niveis<-levels(df0[[variable]])}

			# Examples of possibilities of simulation/optimization
				# without replacement
				for( i in target_sampId)
					{

					df2<-df0[df0$sampId==i & !is.na(df0[[variable]]),]
					# sampling the lf with various sizes
						windows(15,7)
						par(mfrow=c(2,3))			
						sampsize<-nrow(df2)
						barplot(table(factor(df2[[variable]], levels=niveis)), las=2, cex.names=0.7, main=paste("original n (NAs excluded) =", nrow(df2)))	
						barplot(table(sample(factor(df2[[variable]], levels=niveis), size=sampsize, replace=FALSE)), las=2, cex.names=0.7,  main=paste("sampled",sampsize,"wor repl"))	
						for (j in samp_sizes)
						if(sampsize>=j) 
							{
							barplot(table(sample(factor(df2[[variable]], levels=niveis), size=j, replace=FALSE)), las=2, cex.names=0.7,  main=paste("sampled",j,"wor repl"))
							} else {
									plot.new()
									}
			
					savePlot(filename = paste("002_Exploratory_analyses\\002_sampId_testsims\\002_sampId_testsim_",i,"_",variable,"_without_replacement.png", sep=""), type = "png")
					dev.off()
					}

				# with replacement	
				for( i in target_sampId)
					{
					# Example of possibilities of simulation/optimization
					df2<-df0[df0$sampId==i & !is.na(df0[[variable]]),]
					# sampling the lf with various sizes
						windows(15,7)
						par(mfrow=c(2,3))			
						sampsize<-nrow(df2)
						barplot(table(factor(df2[[variable]], levels=niveis)), las=2, cex.names=0.7, main=paste("original n (NAs excluded) =", nrow(df2)))	
						barplot(table(sample(factor(df2[[variable]], levels=niveis), size=sampsize, replace=TRUE)), las=2, cex.names=0.7,  main=paste("sampled",sampsize,"wr repl"))	
						for (j in samp_sizes))
						if(sampsize>=j) 
							{
							barplot(table(sample(factor(df2[[variable]], levels=niveis), size=j, replace=TRUE)), las=2, cex.names=0.7,  main=paste("sampled",j,"wr repl"))
							} else {
									plot.new()
									}
					savePlot(filename = paste("002_Exploratory_analyses\\002_sampId_testsims\\002_sampId_testsims_",i,"_",variable,"_with_replacement.png", sep=""), type = "png")
					dev.off()
					}	
			
		}	


	# =========================	
	# Select samples to analyze [you will need to adapt this to your case]
	# =========================
		
		# we will need to feed the script with a minimum number of fish (Min_n) that we consider provides an acceptable distribution (i.e., good enough for use in the simulations)
		# the algorithm and graphs below help you determine that value
		

		# 1st step: explore tables below
			table(df0$trpCode)
			sort(table(df0$sampId))
	
		# 2nd step: determine CV and MWCV for variables of interest and see how much they vary across your samples
			# note: CV will depend on n and the mean value (they interact)
			
			source("000_Auxiliary_Funs\\func_do_MWCV.r")
			source("000_Auxiliary_Funs\\func_do_CV_mean.r")
	
			sampsize<-table(df0$sampId)
			mwcv_lenCls<-tapply(df0$lenCls, df0$sampId, do_MWCV)
			cv_lenCls<-tapply(df0$lenCls, df0$sampId, do_CV_mean)
			mean_lenCls<-tapply(df0$lenCls, df0$sampId, mean, na.rm=T)
			mwcv_age<-tapply(df0$age, df0$sampId, do_MWCV)
			cv_age<-tapply(df0$age, df0$sampId, do_CV_mean)
			mean_age<-tapply(df0$age, df0$sampId, mean, na.rm=T)
			n_lenCls<-tapply(df0$lenCls[!is.na(df0$lenCls)], df0$sampId[!is.na(df0$lenCls)],length); ; n_lenCls[is.na(n_lenCls)]<-0
			n_age<-tapply(df0$age, df0$sampId, length); n_age[is.na(n_age)]<-0

			
			# issues table with results (by sampleId)			
			out<-data.frame(sampId = names(mwcv_lenCls), sampsize, mwcv_lenCls,cv_lenCls, mwcv_age, cv_age, row.names=NULL)
			write.csv2(out,  file= "002_Exploratory_analyses\\003_Min_n_determination_sampId_info.csv")
			
		# 3rd step: look at the indicator values and distributions together
			# note: you will have to set target variable, original_class_span, smooth_class_span, and threshold % for mode consideration [as proportion of individuals in sample]
			
			source("000_Auxiliary_Funs\\func_expl_analysis_smooth_and_modes.r") # contains "expl_analysis_smooth_and_modes"
			source("000_Auxiliary_Funs\\func_localMaxima.r") # used in "expl_analysis_smooth_and_modes"
			source("000_Auxiliary_Funs\\func_localMaxima2.r") # used in "expl_analysis_smooth_and_modes"
			
			# set target_variable
			target_variable = "lenCls" # "age"
		
			# set the original class width/span for variable [default is automatic but you can choose manually]
				# do it automatically [better you check the result...]
					original_class_span <- median(diff(sort(unique(df0[[target_variable]])))) 	# automatic detection of the original length class
				# or do it manually 
					# original_class_span <- 1
		

			# exploratory analysis for determination of best smooth_class_span and min_proportion_to_accept_mode
				# outputs graphs that help determine the two parameters 
				expl_analysis_smooth_and_modes (df0 = df0[!is.na(df0[[target_variable]]),], variable =  target_variable,
											samples_to_analyze = unique(df0[!is.na(df0[[target_variable]]),"sampId"]), 
											smooth_class_span = 2*original_class_span, 
											min_proportion_to_accept_mode = 0.05,
											save_plot = TRUE, 
											dir_save = "002_Exploratory_Analyses\\004_sampId_barplots\\", 
											file_root = paste("004_sampId_barplot_with_mode_detection_", sep=""), 
											include_CV_MWCV_in_title = TRUE)
					
		# 4th step: evaluate summary graphs

			# support graph 1	
			windows(10,10); par(mfrow=c(4,2) ,mar=c(5,4,2,2))
			plot(mwcv_lenCls~mean_lenCls, ylim = c(0,max(mwcv_lenCls,na.rm=T)), main = "MWCV of length vs. mean length", ylab="MWCV (length)", xlab="mean (length)" )
			plot(cv_lenCls~mean_lenCls, ylim = c(0,max(cv_lenCls,na.rm=T)), main = "CV of length vs. mean length", ylab="CV (length)", xlab="mean (length)" )
			plot(mwcv_lenCls~n_lenCls, ylim = c(0,max(mwcv_lenCls,na.rm=T)), main = "MWCV of length vs. n length", ylab="MWCV (length)", xlab="n (length)" )
			plot(cv_lenCls~n_lenCls, ylim = c(0,max(cv_lenCls,na.rm=T)), main = "CV of length vs. n length", ylab="CV (length)", xlab="n (length)" )
			plot(mwcv_age~mean_age, ylim = c(0,max(mwcv_age,na.rm=T)), main = "MWCV of age vs. mean age", ylab="MWCV (age)", xlab="mean (age)" )
			plot(cv_age~mean_age, ylim = c(0,max(cv_age,na.rm=T)), main = "CV of age vs. mean age", ylab="CV (age)", xlab="mean (age)" )
			plot(mwcv_age~n_age, ylim = c(0,max(mwcv_age,na.rm=T)), main = "MWCV of age vs. n age", ylab="MWCV (age)", xlab="n (age)" )
			plot(cv_age~n_age, ylim = c(0,max(cv_age,na.rm=T)), main = "CV of age vs. n age", ylab="CV (age)", xlab="n (age)" )
			savePlot(filename = paste("002_Exploratory_analyses\\005_Min_n_determination_plot1_initial.png", sep=""), type = "png")		
			dev.off()			
			
			# support graph 2		
			windows(7,5)
			xlabels = paste("[",sort(unique(sampsize%/%20))*20,"-",sort(unique(sampsize%/%20+1))*20,"[", sep="")
			par(mfrow=c(2,2) ,mar=c(5,4,2,2))
			boxplot(mwcv_lenCls~c(sampsize%/%20), varwidth=T, ylim=c(0, max(mwcv_lenCls, na.rm=T)), names=xlabels, las=2, cex.axis=0.9, main="MWCV of lenCls")
			#abline(v=4.5, col=2, lty=2)
			boxplot(cv_lenCls~c(sampsize%/%20), varwidth=T, ylim=c(0, max(cv_lenCls, na.rm=T)), names=xlabels, las=2, cex.axis=0.9, main="CV of the mean lenCls")
			#abline(v=4.5, col=2, lty=2)
			boxplot(mwcv_age~c(sampsize%/%20), varwidth=T, ylim=c(0, max(mwcv_age, na.rm=T)), names=xlabels, las=2, cex.axis=0.9, main="MWCV of age")
			#abline(v=4.5, col=2, lty=2)
			boxplot(cv_age~c(sampsize%/%20), varwidth=T, ylim=c(0, max(cv_age, na.rm=T), na.rm=T)), names=xlabels, las=2, cex.axis=0.9, main="CV of the mean age")
			#abline(v=4.5, col=2, lty=2)
			savePlot(filename = paste("002_Exploratory_analyses\\005_Min_n_determination_plot2_initial.png", sep=""), type = "png")		
			dev.off()			

			# support graph 3				
			windows(7,5); par(mfrow=c(1,2) ,mar=c(5,4,2,2))			
			plot(mwcv_age~mwcv_lenCls, ylim = c(0,100), xlim = c(0,100), main = "MWCV of length vs. age", xlab="MWCV (length)", ylab="MWCV (age)" )
			cor(mwcv_lenCls,mwcv_age)
			abline(1,1, lty=2, col=1)
			plot(cv_age~cv_lenCls , ylim = c(0,20), xlim = c(0,20), main = "CV of the mean of length vs. age", xlab="CV of the mean (length)", ylab="CV of the mean (age)" )
			cor(cv_lenCls,cv_age)
			abline(1,1, lty=2, col=1)
			savePlot(filename = paste("002_Exploratory_analyses\\005_Min_n_determination_plot3_initial.png", sep=""), type = "png")		
			dev.off()


		# 5th step: by now you should be able to set some test values for "good enough samples" and evaluate coverage of the data you are retaining for analysis
		
			
			# define minimum number of individuals required for samples to be considered "representative" [adapt to your situation]
			# Note: 
				# if your protocol establishes the collection of a specific volume/weight than your sample size might be related to mean size of the individuals
				# under those circumstances it may be important to ensure that min_n is sufficiently low so that samples with larger individuals are not excluded from simulations
			# Cautionary note:
				# the exact implications of sampling with replacement from samples that have quite different sample sizes needs to be looked up
					# not sure how long into sample_size>>real_size we can go in different samples

				# set 		
				min_n_test<-100
				table_select_samples<-table(df0$sampId)[table(df0$sampId)>=min_n_test]; 
				# prints the number of samples being considered and the proportion of total samples they represent
				cat("No. selected samples:",length(table_select_samples),"\n")
				cat("% selected samples:",length(table_select_samples)/length(unique(df0$sampId))*100,"\n")
				samples_to_analyze<-names(table_select_samples)
				# adjust ctr_var1 and ctr_var2 to your needs to check if selected samples are (minimally) representative of original data (if they are not, try adjusting min_n_test down and see if coverage improves)
					ctr_var1<-"quarter"
					ctr_var2<-"foCatEu6"
					original_prop_samples<-round(prop.table(table(unique(df0[,c("sampId",ctr_var1,ctr_var2)])[,ctr_var1], unique(df0[,c("sampId",ctr_var1,ctr_var2)])[, ctr_var2]))*100,2)
					final_prop_samples<-round(prop.table(table(unique(df0[df0$sampId %in% samples_to_analyze,c("sampId",ctr_var1,ctr_var2)])[,ctr_var1], unique(df0[df0$sampId %in% samples_to_analyze,c("sampId",ctr_var1,ctr_var2)])[,ctr_var2]))*100,2)
					original_prop_indiv<-round(prop.table(table(df0[,ctr_var1], df0[, ctr_var2]))*100,2)
					final_prop_indiv<-round(prop.table(table(df0[df0$sampId %in% samples_to_analyze,ctr_var1], df0[df0$sampId %in% samples_to_analyze, ctr_var2]))*100,2)
					# differences in percent - if too much, try adjusting min_n_test down and see if it improves
					cat("% dif between original and final dataset in terms of No. of samples: \n",original_prop_samples-final_prop_samples,"\n") 	
					cat("% dif between original and final dataset in terms of No. of indiv: \n",original_prop_indiv-final_prop_indiv,"\n") 	
					write.csv2(original_prop_samples-final_prop_samples,  file= "002_Exploratory_analyses\\006_Min_n_coverage_samples.csv")
					write.csv2(original_prop_indiv-final_prop_indiv,  file= "002_Exploratory_analyses\\006_Min_n_coverage_indivs.csv")
					rm(ctr_var1, ctr_var2, original_prop_samples, final_prop_samples, original_prop_indiv, final_prop_indiv)
					
					
			# check on No classes per sample
			df1<-df0[df0$sampId %in% names(table_select_samples),]
			tapply(df1$lenCls, df1$sampId, function(x) length(unique(x)))
			tapply(df1$age, df1$sampId, function(x) length(unique(x)))
		
		# 6th step: re-evaluate summary graphs

			# support graph 1	
			windows(10,10); par(mfrow=c(4,2) ,mar=c(5,4,2,2))
			plot(mwcv_lenCls~mean_lenCls, ylim = c(0,max(mwcv_lenCls,na.rm=T)), main = "MWCV of length vs. mean length", ylab="MWCV (length)", xlab="mean (length)" )
			points(mean_lenCls[names(mean_lenCls) %in% names(table_select_samples)], mwcv_lenCls[names(mwcv_lenCls) %in% names(table_select_samples)], col="red")
			plot(cv_lenCls~mean_lenCls, ylim = c(0,max(cv_lenCls,na.rm=T)), main = "CV of length vs. mean length", ylab="CV (length)", xlab="mean (length)" )
			points(mean_lenCls[names(mean_lenCls) %in% names(table_select_samples)], cv_lenCls[names(cv_lenCls) %in% names(table_select_samples)], col="red")
			plot(mwcv_lenCls~n_lenCls, ylim = c(0,max(mwcv_lenCls,na.rm=T)), main = "MWCV of length vs. n length", ylab="MWCV (length)", xlab="n (length)" )
			points(n_lenCls[names(n_lenCls) %in% names(table_select_samples)], mwcv_lenCls[names(mwcv_lenCls) %in% names(table_select_samples)], col="red")
			plot(cv_lenCls~n_lenCls, ylim = c(0,max(cv_lenCls,na.rm=T)), main = "CV of length vs. n length", ylab="CV (length)", xlab="n (length)" )
			points(n_lenCls[names(n_lenCls) %in% names(table_select_samples)], cv_lenCls[names(cv_lenCls) %in% names(table_select_samples)], col="red")
			plot(mwcv_age~mean_age, ylim = c(0,max(mwcv_age,na.rm=T)), main = "MWCV of age vs. mean age", ylab="MWCV (age)", xlab="mean (age)" )
			points(mean_age[names(mean_age) %in% names(table_select_samples)], mwcv_age[names(mwcv_age) %in% names(table_select_samples)], col="red")
			plot(cv_age~mean_age, ylim = c(0,max(cv_age,na.rm=T)), main = "CV of age vs. mean age", ylab="CV (age)", xlab="mean (age)" )
			points(mean_age[names(mean_age) %in% names(table_select_samples)], cv_age[names(cv_age) %in% names(table_select_samples)], col="red")
			plot(mwcv_age~n_age, ylim = c(0,max(mwcv_age,na.rm=T)), main = "MWCV of age vs. n age", ylab="MWCV (age)", xlab="n (age)" )
			points(n_age[names(n_age) %in% names(table_select_samples)], mwcv_age[names(mwcv_age) %in% names(table_select_samples)], col="red")
			plot(cv_age~n_age, ylim = c(0,max(cv_age,na.rm=T)), main = "CV of age vs. n age", ylab="CV (age)", xlab="n (age)" )
			points(n_age[names(n_age) %in% names(table_select_samples)], cv_age[names(cv_age) %in% names(table_select_samples)], col="red")
			savePlot(filename = paste("002_Exploratory_analyses\\006_Min_n_determination_plot1_final.png", sep=""), type = "png")		
			dev.off()			
			
			# support graph 2		
			windows(7,5)
			xlabels = paste("[",sort(unique(sampsize%/%20))*20,"-",sort(unique(sampsize%/%20+1))*20,"[", sep="")
			par(mfrow=c(2,2) ,mar=c(5,4,2,2))
			boxplot(mwcv_lenCls~c(sampsize%/%20), varwidth=T, ylim=c(0, max(mwcv_lenCls, na.rm=T)), names=xlabels, las=2, cex.axis=0.9, main="MWCV of lenCls")
			abline(v=min_n_test%/%20-0.5, col=2, lty=2)
			boxplot(cv_lenCls~c(sampsize%/%20), varwidth=T, ylim=c(0, max(cv_lenCls, na.rm=T)), names=xlabels, las=2, cex.axis=0.9, main="CV of the mean lenCls")
			abline(v=min_n_test%/%20-0.5, col=2, lty=2)
			boxplot(mwcv_age~c(sampsize%/%20), varwidth=T, ylim=c(0, max(mwcv_age, na.rm=T)), names=xlabels, las=2, cex.axis=0.9, main="MWCV of age")
			abline(v=min_n_test%/%20-0.5, col=2, lty=2)
			boxplot(cv_age~c(sampsize%/%20), varwidth=T, ylim=c(0, max(cv_age, na.rm=T)), names=xlabels, las=2, cex.axis=0.9, main="CV of the mean age")
			abline(v=min_n_test%/%20-0.5, col=2, lty=2)
			savePlot(filename = paste("002_Exploratory_analyses\\006_Min_n_determination_plot2_final.png", sep=""), type = "png")		
			dev.off()			

			# support graph 3				
			windows(7,5); par(mfrow=c(1,2) ,mar=c(5,4,2,2))			
			plot(mwcv_age~mwcv_lenCls, ylim = c(0,100), xlim = c(0,100), main = "MWCV of length vs. age", xlab="MWCV (length)", ylab="MWCV (age)" )
			points(mwcv_lenCls[names(cv_lenCls) %in% names(table_select_samples)], mwcv_age[names(cv_age) %in% names(table_select_samples)], col="red")
			cor(mwcv_lenCls,mwcv_age)
			abline(1,1, lty=2, col=1)
			plot(cv_age~cv_lenCls , ylim = c(0,20), xlim = c(0,20), main = "CV of the mean of length vs. age", xlab="CV of the mean (length)", ylab="CV of the mean (age)" )
			points(cv_lenCls[names(cv_lenCls) %in% names(table_select_samples)], cv_age[names(cv_age) %in% names(table_select_samples)], col="red")
			cor(cv_lenCls,cv_age)
			abline(1,1, lty=2, col=1)
			savePlot(filename = paste("002_Exploratory_analyses\\006_Min_n_determination_plot3_final.png", sep=""), type = "png")		
			dev.off()

		
		# 6th step: put here the min_n you select
			min_n <- min_n_test

		# save		
		save(min_n, file="001_Prepared_Inputs\\Min_n.Rdata")	