# ====================	
# Analyses of Simulations
# ====================	
	
	# Nuno Prista, SLU Aqua, Sweden @ WKBIOPTIM 2017-201
		
	# 2018-09-10: extracted from "003_sim_data.r"
	# 2019-05-26: streamlined, annotated, split into "004/1 - within sample analyses" and "004/2 - modeling analysis"
	
	
		# wishlist:
			# improve graphical and table outputs

			
		rm(list=ls())
		
		library(data.table)
		
		load("003_Sim_Results\\Sim_results_201905252351.rdata")		
			
		samples_to_analyze <- names(sim_res)
		
		# 2014_2535 low MWCV
		# 2014_2542 intermediate MWCV
		# 2015_2562 2015_2565 similar (high MWCV)
	
		
		source("000_Auxiliary_Funs\\Sim_Analysis\\func_determine_best_scale.r") # used to determine the best scale of the graphs
		source("000_Auxiliary_Funs\\Sim_Analysis\\func_do_boxplot_sample_numeric.r")
		source("000_Auxiliary_Funs\\func_do_MWCV.r")
		source("000_Auxiliary_Funs\\func_do_CV_mean.r")
		

		
		# ===================
		# demonstration of some results: cv and MWCV
		# ===================	
		
			best_scale <- determine_best_scale(x = sim_res, variables = c("lenCls","age"), stats = c("cv","MWCV"), zero_is_lowest = TRUE)

			# aggregates sim_res by variable
				sim_res_var_pop<-sapply(names(sim_res[[1]]), function(x) NULL)	
				sim_res_var_nopop<-sapply(names(sim_res[[1]]), function(x) NULL)	
				for (variable in names(sim_res_var_pop))
				{
				# including sim of the population
				sim_res_var_pop[[variable]]<-rbindlist(lapply(sim_res[samples_to_analyze], function(x, var1 = variable){x[[var1]]}))
				# excluding sim of the population
				sim_res_var_nopop[[variable]]<-rbindlist(lapply(sim_res[samples_to_analyze], function(x, var1 = variable){x[[var1]][x[[var1]]$sim != tail(x[[var1]],1)$sim,]}))
				}	
			
			# select target_var
				target_var <- "lenCls"
			
			# subsets to target_var
				df1<-sim_res_var_pop[[target_var]]
			
			# adds ntotclass per sample and mean_samp
				ref<-tapply(df0$lenCls,df0$sampId, function(x){(range(x)[2]-range(x)[1])/5+1})
				df1$ntotclass<-ref[match(df1$sampId, names(ref))]	
				ref<-tapply(df0$lenCls,df0$sampId, function(x){mean(x)})
				df1$mean_samp<-ref[match(df1$sampId, names(ref))]	
				
			# plots
				plot(cv ~ sim, data = df1, ylim = best_scale[[target_var]]$cv)
				points(cv ~ sim, data = df1[df1$sampId=="2014_2017",], col=2)
				points(cv ~ sim, data = df1[df1$sampId=="2015_2031",], col=3)
				points(cv ~ sim, data = df1[df1$sampId=="2015_2032",], col=4)
				points(cv ~ sim, data = df1[df1$sampId=="2015_2033",], col=5)
				
				plot(MWCV ~ estim_weight, data = df1, ylim = best_scale[[target_var]]$MWCV)
				points(MWCV ~ estim_weight, data = df1[df1$sampId=="2014_2017",], col=2)
				points(MWCV ~ estim_weight, data = df1[df1$sampId=="2015_2031",], col=3)
				points(MWCV ~ estim_weight, data = df1[df1$sampId=="2015_2032",], col=4)
				points(MWCV ~ estim_weight, data = df1[df1$sampId=="2015_2033",], col=5)
			
				plot(cv~mean_samp, data = df1[sim>50,], ylim = best_scale[[target_var]]$cv)
				plot(MWCV~mean_samp, data = df1[sim>50,], ylim = best_scale[[target_var]]$MWCV)
				plot(MWCV~ntotclass, data = df1[sim>50,], ylim = best_scale[[target_var]]$MWCV)
				plot(cv~ntotclass, data = df1[sim==10,], ylim = best_scale[[target_var]]$cv)
				plot(cv~n_class_sampled, data = df1[sim>50,], ylim = best_scale[[target_var]]$cv)
				plot(MWCV~n_class_sampled, data = df1[sim>50,], ylim = best_scale[[target_var]]$MWCV)
				plot(MWCV~I(n_class_sampled/ntotclass), data = df1[sim>50,], ylim = best_scale[[target_var]]$MWCV)
			
		
		
		# boxplots of numeric variables per sample (2 variables * 2 stats, per graph]
		
			for (i in names(sim_res))
				 {
				 sim_res[[i]]$lenCls$rse<-sim_res[[i]]$lenCls$cv
				 sim_res[[i]]$age$rse<-sim_res[[i]]$age$cv
				 }
		
			# cv of mean and MWCV
			best_scale <- determine_best_scale(x = sim_res, variables = c("lenCls","age"), stats = c("cv","MWCV"), zero_is_lowest = TRUE)
			do_boxplox_sims_numeric(x = sim_res, variables = c("lenCls","age"), stats = c("MWCV","cv"), escala = best_scale, save_plot=TRUE, filename_root = "CV_MWCV_", dir_output = "004_Sim_Analysis/")
			
	
			# min and max 
			best_scale <- determine_best_scale(x = sim_res, variables = c("lenCls","age"), stats = c("min","max"), zero_is_lowest = TRUE)
			do_boxplox_sims_numeric(x = sim_res, y = df0, variables = c("lenCls","age"), stats = c("min","max"), escala = best_scale, save_plot=TRUE, filename_root = "Min_Max_", dir_output = "004_Sim_Analysis/")
		
			graphics.off()
		
		
		# results of original samples [note: if SRSWR than selects replicate 2 of each sim - a different replicate can be selected by tweaking the code]
				a<-sapply(names(sim_res[[1]]), function(x) NULL)	
				for (variable in names(a))
					{
					a[[variable]]<-unique(do.call("rbind", lapply(sim_res[samples_to_analyze], function(x, var1 = variable){x[[var1]][x[[var1]]$sim == tail(x[[var1]],1)$sim,][2,]})))
					}
		
			# exploratory analyses of correlations	
				plot(a[["lenCls"]]$MWCV ~ a[["age"]]$MWCV, xlab="MWCV (age)", ylab = "MWCV (length)", main = "MWCV and samp_size of original samples")
				text(a[["lenCls"]]$MWCV ~ a[["age"]]$MWCV, labels=a[["age"]]$sim, cex=.7, pos=1)
				text(a[["lenCls"]]$MWCV ~ a[["age"]]$MWCV, labels=a[["lenCls"]]$n_class_sampled, cex=.7, pos=2, col=2)
				text(a[["lenCls"]]$MWCV ~ a[["age"]]$MWCV, labels=a[["age"]]$n_class_sampled, cex=.7, pos=4, col=4)
				plot(a[["lenCls"]]$MWCV ~ a[["lenCls"]]$mean, xlab="mean", ylab = "MWCV (length)", main = "MWCV and samp_size of original samples")
				plot(a[["lenCls"]]$MWCV ~ a[["lenCls"]]$n_class_sampled, xlab="n_class_sampled", ylab = "MWCV (length)", main = "MWCV and samp_size of original samples")
				plot(a[["age"]]$MWCV ~ a[["age"]]$mean, xlab="mean", ylab = "MWCV (age)", main = "MWCV and samp_size of original samples")
				plot(a[["age"]]$MWCV ~ a[["age"]]$n_class_sampled, xlab="n_class_sampled", ylab = "MWCV (age)", main = "MWCV and samp_size of original samples")
				rm(a)
			# exploratory analyses of correlations (all sims)	
				plot(sim_res_var_pop[["lenCls"]]$MWCV ~ sim_res_var_pop[["age"]]$MWCV, xlab="MWCV (age)", ylab = "MWCV (length)", main = "MWCV and samp_size of original samples")
				plot(sim_res_var_pop[["lenCls"]]$MWCV ~ sim_res_var_pop[["lenCls"]]$mean, xlab="mean", ylab = "MWCV (lenCls)", main = "MWCV and samp_size of original samples")
				plot(sim_res_var_pop[["age"]]$MWCV ~ sim_res_var_pop[["age"]]$mean, xlab="mean", ylab = "MWCV (age)", main = "MWCV and samp_size of original samples")

				
				
		# ====================================================
		# selection of the most appropriate sample size [within sample sizes]
		# ====================================================
	
			# the following function is handy in producing tables of stats in sim_res_var* that can be used to select samp size
			
			f1<-function(x, variable, target_stat, FUN = "median")
							{
							# x is sim_res_var_pop or sim_res_var_nopop
							t1<-tapply(x[[variable]][[target_stat]],list(x[[variable]][["sim"]],x[[variable]][["sampId"]]), FUN)
							t1
							}
			# analysis
			f1(x = sim_res_var_nopop, variable = "lenCls", target_stat = "MWCV", FUN = "max")
			f1(x = sim_res_var_nopop, variable = "age", target_stat = "MWCV", FUN = "max")
			f1(x = sim_res_var_nopop, variable = "lenCls", target_stat = "cv", FUN = "max")
			f1(x = sim_res_var_nopop, variable = "age", target_stat = "cv", FUN = "max")
			f1(x = sim_res_var_nopop, variable = "lenCls", target_stat = "MWCV", FUN = function(x){sum(x<50)})
			f1(x = sim_res_var_nopop, variable = "lenCls", target_stat = "cv", FUN = function(x){sum(x<5)})
			f1(x = sim_res_var_nopop, variable = "weight-length", target_stat = "r.squared", FUN = function(x){sum(x>0.90)})
		
		# mean weights associated to sample sizes [generally better to communicate weights rather than numbers]
			
			apply(f1(x = sim_res_var_nopop, variable = "lenCls", target_stat = "estim_weight", FUN = "mean"),1,max)
		
		
			# analysis: two ways:
				# We set define a criteria (e.g., MWCV) and use it as a threshold to determine a sample size that meets it in the vast majority of times 
					# how we can define the criteria
						# by the book (if there is one)
						# from visual inspections of a few replicates
				# We determine the number based on the curve properties
					# e.g., when the curve slope is higher than -1
					# when the change in some property is less than x%
				# we determine the number from an accepted lost relative to a reference sample size or the one we obtained	
		
		# multivariate results of a sample size	
			
			selected_samp_size<-90	

			# the following function is handy in producing graphs showing what to expect in terms of MWCV or CV of different vars when a certain sample size is selected
			
			f2<-function(x, variable, target_stat, selected_samp_size = 20, escala = best_scale)
								{
								# x is sim_res_var_pop or sim_res_var_nopop								
								a<-x[[variable]]
								a<-a[a$sim==selected_samp_size,]
								boxplot(a[[target_stat]]~a$sampId, main= paste("samp size",selected_samp_size), ylab=paste(target_stat, variable), ylim=escala[[variable]][[target_stat]], par(las=2) )
								}

			best_scale <- determine_best_scale(x = sim_res, variables = c("lenCls","age"), stats = c("cv","MWCV"), zero_is_lowest = TRUE)
			f2(x = sim_res_var_nopop,variable = "lenCls", target_stat = "cv", selected_samp_size = 90)
			f2(x = sim_res_var_nopop,variable = "lenCls", target_stat = "MWCV", selected_samp_size = 90)
			f2(x = sim_res_var_nopop,variable = "lenCls", target_stat = "MWCV", selected_samp_size = 50)
			f2(x = sim_res_var_nopop,variable = "age", target_stat = "MWCV", selected_samp_size = 50)
			f2(x = sim_res_var_nopop,variable = "age", target_stat = "cv", selected_samp_size = 50)
	
		# The following function returns a table with FUN applied to all replicates of a give sample size
	
			f3 <- function(x, target_vars, target_stats, selected_samp_size = 20, FUN)
								{
								out<-data.frame()	
								# x is sim_res_var_pop or sim_res_var_nopop								
								for (target_var in target_vars)
									{
									a<-as.data.frame(x[[target_var]])
									out<-rbind(out, data.frame(target_var = target_var, a[a$sim==selected_samp_size,c("sim",target_stats)]))
									}
								ls1<-split(out[,target_stats], out$target_var)
								ls2<-lapply(ls1, function(x, f = FUN) apply(x, 2, f))
								out<-do.call("rbind",ls2)
								out
								}	
			
			f3(x = sim_res_var_nopop, target_vars = c("lenCls","age"), target_stats = c("cv","MWCV"), selected_samp_size = 90, FUN = "max")					
			f3(x = sim_res_var_nopop, target_vars = c("lenCls","age"), target_stats = c("cv","MWCV"), selected_samp_size = 90, FUN = "median")					
			
			# final graph (2 var)	
			par(mfrow=c(2,2))
			boxplot(cv~sim,	data=sim_res_var_nopop$age, main="cv age", ylim=best_scale$age$cv)
			boxplot(MWCV~sim,	data=sim_res_var_nopop$age, main="MWCV age", ylim=best_scale$age$MWCV)
			boxplot(cv~sim,	data=sim_res_var_nopop$lenCls, main="cv lenCls", ylim=best_scale$lenCls$cv)
			boxplot(MWCV~sim,	data=sim_res_var_nopop$lenCls, main="MWCV lenCls", ylim=best_scale$lenCls$MWCV)
			savePlot(filename = "004_Sim_Analysis/MWCV_CV_all_samples", type="png")