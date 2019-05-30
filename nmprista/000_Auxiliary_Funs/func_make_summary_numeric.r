


make_summary_numeric<-function(y, variable, a ,b, ls_original_modes_sample, repl = 1){#browser()
# Nuno Prista 2017
	# wishlist - add DI and other indexes from Chih, 2010
	
	# 20180526 - adapted to indivId as input
	# 20180526 - naming of objects improved (less portuguese now)
	# 20180526 - moved to arguments the parameters of the weight - length relationship
	# 20180526 - n_class_sampled_x now excludes NAs 
	# - CHECK - 20180526 - added original number of modes 
	# 20180913 - added condition of existence of non_NA observations 
	# 20180917 - added removal of NAs from stand_err_mean_x
	# 20190524 - bug fixed: added argument ls_auto_modes_sample
	# 20190524 - added argument repl - useful guide in simulations	
	# 20190524 - renamed ls_auto_modes_sample to ls_original_modes_sample

													x <- df1[variable][match(y, df1$indivId),variable]
													if(length(na.omit(x))>1){ # condition for existence of non_NA observations
													# adjustments for processing
														#if(is.vector(x)) {x<-data.frame(dummy=1,x); colnames(x)[2]<-variable}
														if(is.vector(x) | is.factor(x[variable])) {x<-data.frame(dummy=1,x); colnames(x)[2]<-variable}
														# if(is.factor(x[,variable])) x[,variable]<-as.numeric(as.character(x[,variable]))
														# if(is.factor(df1[,variable])) df1[,variable]<-as.numeric(as.character(df1[,variable]))
													# summary	
														n_indiv = nrow(x)
														NAs_x<-sum(is.na(x[,variable]))
														mean_x<-mean(x[,variable], na.rm=TRUE)
														stand_err_mean_x<-sd(x[,variable], na.rm=TRUE)/sqrt(length(x[!is.na(x[,variable]),variable]))
														median_x<-median(x[,variable], na.rm=TRUE)
														min_x<- min(x[,variable], na.rm=TRUE)
														max_x<-max(x[,variable], na.rm=TRUE)
														n_class_sampled_x<-length(unique(x[!is.na(x[,variable]),variable])) 
												#ATTT!!!!		
														if(variable == "lenCls") estim_weight_sample <- sum(exp(a)*x$lenCls^b) else {estim_weight_sample<-NA}
													# mean weighed CV	
														sigma_i<-sqrt(nrow(x)*as.matrix(prop.table(table(x[,variable]))*(1-prop.table(table(x[,variable])))))
														cv_i <- sigma_i / (nrow(x)*as.matrix(prop.table(table(x[,variable]))))
														MWCV<-round(sum(sigma_i)/nrow(x)*100,1)
													# analysis of frequency distributions (original)
														freq_dist_sampled_indivs <- table( factor(x[,variable]-x[,variable]%%ls_original_modes_sample[[variable]][["original_class_span"]], levels=ls_original_modes_sample[[variable]][["original_breaks"]]))
														if(length(unique(freq_dist_sampled_indivs))>1) # condicao de existencia de modas
															{modes_sampled_individuals <- localMaxima2(as.numeric(freq_dist_sampled_indivs))} else {modes_sampled_individuals<-rep(NA,length(freq_dist_sampled_indivs))}
														modes_sampled_individuals_after_threshold<-modes_sampled_individuals[modes_sampled_individuals %in% which(freq_dist_sampled_indivs> (ls_original_modes_sample[[variable]]$min_proportion_to_accept_mode * sum(freq_dist_sampled_indivs)))]	
														n_modes<-length(modes_sampled_individuals_after_threshold)
														# compares modes of individuals sampled with the original modes
															modes_correct<-identical(modes_sampled_individuals_after_threshold,ls_original_modes_sample[[variable]][["original_modes"]])
															n_modes_correct<-sum(modes_sampled_individuals_after_threshold %in% ls_original_modes_sample[[variable]][["original_modes"]])
													# analysis of frequency distributions (smooth)															
														freq_dist_sampled_indivs_smooth <- table( factor(x[,variable]-x[,variable]%%ls_original_modes_sample[[variable]][["smooth_class_span"]], levels=ls_original_modes_sample[[variable]][["smooth_breaks"]]))
														if(length(unique(freq_dist_sampled_indivs_smooth))>1)# condicao de existencia de modas
																{modes_sampled_individuals_smooth <- localMaxima2(as.numeric(freq_dist_sampled_indivs_smooth))} else {modes_sampled_individuals_smooth<-rep(NA,length(freq_dist_sampled_indivs_smooth))}
														modes_sampled_individuals_smooth_after_threshold<-modes_sampled_individuals_smooth[modes_sampled_individuals_smooth %in% which(freq_dist_sampled_indivs_smooth> (ls_original_modes_sample[[variable]]$min_proportion_to_accept_mode * sum(freq_dist_sampled_indivs_smooth)))]	
														n_modes_smooth = length(modes_sampled_individuals_smooth_after_threshold)
														# compares smoothed modes of individuals sampled with the original smoothed modes														
															modes_correct_smooth<-identical(modes_sampled_individuals_smooth_after_threshold,ls_original_modes_sample[[variable]][["smooth_modes"]])
															n_modes_correct_smooth<-sum(modes_sampled_individuals_smooth_after_threshold %in% ls_original_modes_sample[[variable]][["smooth_modes"]])														
													# tests on distributions
														

														ttest_prob <- t.test(x[,variable], df1[,variable])$p.value
														ttest_logic <- ttest_prob >= 0.05
														ks_prob<-ks.test(x[,variable], df1[,variable])$p.value
														kstest_logic <- ks_prob >= 0.05
													# output
														data.frame(repl = repl, var=variable, stages = sampling_options$stages, n_stage1 = sampling_options$stage1_samp_size, strata_var = sampling_options$strata_var, n = n_indiv, estim_weight = estim_weight_sample, NAs_x = NAs_x, mean=mean_x, se = stand_err_mean_x, cv=round(stand_err_mean_x/mean_x*100,1), min = min_x, median = median_x, max = max_x, n_class_sampled=n_class_sampled_x,
														n_modes = n_modes, n_modes_smooth = n_modes_smooth, modes_correct = modes_correct, n_modes_correct = n_modes_correct, modes_correct_smooth = modes_correct_smooth, n_modes_correct_smooth = n_modes_correct_smooth, 
														ttest_prob = ttest_prob, ttest_logic = ttest_logic, ks_prob = ks_prob, kstest_logic = kstest_logic, MWCV = MWCV)	
														
														} else { print("ahahah")
														# output
														data.frame(repl = repl, var=variable, stages = sampling_options$stages, n_stage1 = sampling_options$stage1_samp_size, strata_var = sampling_options$strata_var, n = length(x), estim_weight = NA, NAs_x = length(x) - length(na.omit(x)), mean=NA, se = NA, cv=NA, min = NA, median = NA, max = NA, n_class_sampled=NA,
														n_modes = NA, n_modes_smooth = NA, modes_correct = NA, n_modes_correct = NA, modes_correct_smooth = NA, n_modes_correct_smooth = NA, 
														ttest_prob = NA, ttest_logic = NA, ks_prob = NA, kstest_logic = NA, MWCV = NA)	
														
														}
															
												}		
														
