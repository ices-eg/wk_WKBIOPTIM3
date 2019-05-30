make_summary_categorical<-function(y, variable, ls_original_modes_sample, repl = 1){ #browser()
# Nuno Prista 2017
#browser()
	# 20180526 - adapted to indivId as input
	# 20180526 - naming of objects improved (less portuguese now)
	# 20180526 - n_class_sampled_x now excludes NAs 
	# 20180913 - added condition of existence of non_NA observations 
	# 20190524 - bug fixed: added argument ls_auto_modes_sample
	# 20190524 - added argument repl - useful guide in simulations
	# 20190524 - renamed ls_auto_modes_sample to ls_original_modes_sample
	
													x <- df1[variable][match(y, df1$indivId),variable]
													if(length(na.omit(x))>1){ # condition for existence of non_NA observations										
													# adjustments for processing
														x<-data.frame(dummy=1,x); colnames(x)[2]<-variable
														#x[,variable]<-factor(x[,variable], levels=sort(unique(df1[,variable])))
														#df1[,variable]<-factor(df1[,variable], levels=sort(unique(df1[,variable])))
													# summary	
														n_indiv = nrow(x)
														NAs_x<-sum(is.na(x[,variable]))
														n_classes_sampled_x<-length(unique(x[,variable]))
														sampled_classes_x <- paste(sort(unique(x[!is.na(x[,variable]),variable])), collapse=",")
													# analysis of frequency distributions (original) and modes
														freq_dist_sampled_indivs <- table( x[,variable])
														if(length(unique(freq_dist_sampled_indivs))>1)
															{
															modes_sampled_individuals <- localMaxima2(as.numeric(freq_dist_sampled_indivs))
															modes_sampled_individuals_after_threshold<-modes_sampled_individuals[modes_sampled_individuals %in% which(freq_dist_sampled_indivs> (ls_original_modes_sample[[variable]]$min_proportion_to_accept_mode * sum(freq_dist_sampled_indivs)))]	
															n_modes<-length(modes_sampled_individuals_after_threshold)
															modes_correct<-identical(modes_sampled_individuals_after_threshold,ls_original_modes_sample[[variable]][["original_modes"]])
															n_modes_correct<-sum(modes_sampled_individuals_after_threshold %in% ls_original_modes_sample[[variable]][["original_modes"]])
															} else {
																	n_modes = 0
																	modes_correct <- NA
																	n_modes_correct <- NA
													}
												
													#browser()				
													# chisq test
														if(variable == sampling_options$strata_var)
															{
															expected_probs<-prop.table(table(df1[,variable], useNA="al"))
															realized_sample<-table(x[,variable], useNA="al")
															} else {
																	expected_probs<-prop.table(table(df1[,variable]))
																	realized_sample<-table(x[,variable])
																	}
														if(variable == "matStage") {expected_probs<-expected_probs[expected_probs>0]; realized_sample<-realized_sample[names(expected_probs)]}
														chisq_test_prob <- chisq.test(x=as.vector(realized_sample),p = expected_probs)$p.value
														# if you notice issues, try deleting the line above and use the following 
														# implements simulated p.value for matStage where approximations seem worse [makes code slower]
														#if(variable != "matStage") chisq_test_prob <- chisq.test(x=as.vector(realized_sample),p = expected_probs)$p.value else {chisq_test_prob <- chisq.test(x=as.vector(realized_sample),p = expected_probs,simulate.p.value=T, B=1000)$p.value}
														chisq_test_logic<- chisq_test_prob >= 0.05

													# Julia's	
														if(variable == sampling_options$strata_var)
															{
															expected_probs<-prop.table(table(df1[,variable], useNA="al"))
															realized_probs<-prop.table(table(x[,variable], useNA="al"))
															} else {
																	expected_probs<-prop.table(table(df1[,variable]))
																	realized_probs<-prop.table(table(x[,variable]))
																	}
														
														L1 <- sum(abs(expected_probs-realized_probs))
														L2 <- sqrt(sum((expected_probs-realized_probs)^2))
														L2a <- sum((expected_probs-realized_probs)^2)
														L3 <- max(expected_probs-realized_probs)
														
													# output
														data.frame(repl = repl, var=variable, stages = sampling_options$stages, n_stage1 = sampling_options$stage1_samp_size, strata_var = sampling_options$strata_var, n = n_indiv, estim_weight=NA, NAs_x = NAs_x, n_class_sampled=n_classes_sampled_x, sampled_classes = sampled_classes_x,
																	n_modes = n_modes, modes_correct = modes_correct, n_modes_correct = n_modes_correct, 
																		chisq_test_prob = chisq_test_prob, chisq_test_logic = chisq_test_logic,																
																		L1 = L1, L2 = L2, L3 = L3, L2a = L2a)																
														} else {
														data.frame(repl = repl, var=variable, stages = sampling_options$stages, n_stage1 = sampling_options$stage1_samp_size, strata_var = sampling_options$strata_var, n = length(x), estim_weight=NA, NAs_x = length(x) - length(na.omit(x)), n_class_sampled=NA, sampled_classes = NA,
																	n_modes = NA, modes_correct = NA, n_modes_correct = NA, 
																		chisq_test_prob = NA, chisq_test_logic = NA,																
																		L1 = NA, L2 = NA, L3 = NA, L2a = NA)		
															}
}