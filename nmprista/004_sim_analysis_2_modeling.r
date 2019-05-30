# ==========================================
# Analysis of sim_res: Modeling approach
# ==========================================

	# Nuno Prista, SLU Aqua, Sweden @ WKBIOPTIM 2017-2019
		
	# what happens here:
		# Exponential models are fit to MWCV or CV
		# these models are used to estimate sample size needed to attain a target MWCV or CV
		# in the end, a table with model predictions for each sample and sample_size is	produced that can be used e.g., to determine worst case scenario sample sizes
		# alternatively, predictions can be inversed and expectations of MWCV or CV can be obtained for target sample sizes

		
	# wishlist
		# evaluate box transformations
		# evaluate modeling of error

	
	rm(list=ls())
		
	library(data.table)
		
	#source("000_Auxiliary_Funs\\func_determine_best_scale.r") # used to determine the best scale of the graphs
	# source("000_Auxiliary_Funs\\func_do_boxplot_sample_numeric.r")

	# source("000_Auxiliary_Funs\\func_do_CV_mean.r")

	
	# loads sim results
	load("003_Sim_Results\\Sim_results_201905252351.rdata")		
			
	samples_to_analyze <- names(sim_res)
			
	# aggregates sim_res by variable
		# sim_res_var_pop - includes population level results
		# sim_res_var_nopop - excludes population level results
		sim_res_var_pop<-sapply(names(sim_res[[1]]), function(x) NULL)	
		sim_res_var_nopop<-sapply(names(sim_res[[1]]), function(x) NULL)	
		for (variable in names(sim_res_var_pop))
		{
		# including sim of the population
		sim_res_var_pop[[variable]]<-rbindlist(lapply(sim_res[samples_to_analyze], function(x, var1 = variable){x[[var1]]}))
		# excluding sim of the population
		sim_res_var_nopop[[variable]]<-rbindlist(lapply(sim_res[samples_to_analyze], function(x, var1 = variable){x[[var1]][x[[var1]]$sim != tail(x[[var1]],1)$sim,]}))
		}	

# =========================
# fit of exponential models
# =========================

	nls.control(maxiter = 100, tol = 1e-05, minFactor = 1/1024,printEval = FALSE, warnOnly = TRUE)

	# select samples to analyze	
	samples_to_analyze <- names(sim_res)
	#samples_to_analyze <- c("2014_2024","2016_2005","2016_2009","2015_2008")

	source("000_Auxiliary_Funs\\Sim_Modeling\\func_fitMod_MWCV.r")
	source("000_Auxiliary_Funs\\Sim_Modeling\\func_fitMod_CV.r")
	source("000_Auxiliary_Funs\\Sim_Modeling\\func_boxcox.nls2.R") # used in fitMod_MWCV and fitMod_CV
	
	lst.fitMod_MWCV<-fitMod_MWCV(dataset = sim_res_var_nopop, target_variable = "lenCls", sampIds = samples_to_analyze, lambda = seq(-3,-1, by=1/10))
	#lst.fitMod_MWCV<-fitMod_MWCV(dataset = sim_res_var_nopop, target_variable = "age", sampIds = samples_to_analyze, lambda = seq(-3,-1, by=1/10))


# =========================
# Prediction of 3 exponential models for different sample sizes [sample by sample, all models]
# =========================

	source("000_Auxiliary_Funs\\Sim_Modeling\\func_predAllMod_MWCV.r")
	for (i in samples_to_analyze)
		{
		out<-predAllMod_MWCV(lst.fitMod_MWCV, sampId = i, orig_data = sim_res, n_vec = seq(10,90, by=10), plot_it = TRUE, save_plot = TRUE, save_dir="004_Sim_Analysis\\Modeling\\")
		}
	out

# =========================
# Diagnostics of 3 exponential models for different sample sizes [sample by sample, all models]
# =========================
	
	source("000_Auxiliary_Funs\\Sim_Modeling\\func_diagAllMod_MWCV.r")
	for (i in samples_to_analyze[1])
		{
		diagAllMod_MWCV(lst.fitMod_MWCV, sampId = i, save_plot = TRUE, save_dir="004_Sim_Analysis\\Sim_Modeling\\")	
		}
	
	graphics.off()
	
# =========================
# prediction of selected model for different sample sizes [all samples]
# =========================
	
	# select prediction model and prediction range
		selModel = "mod3"
		n_vec<-seq(10,300, by=10)
	
	# storage object to hold the absolute values of predictions
		res_pred_abs<-matrix (ncol=length(samples_to_analyze), nrow=length(n_vec))
		colnames(res_pred_abs)<-samples_to_analyze
		rownames(res_pred_abs)<-as.character(n_vec)

	# storage object to hold the results of differences between predictions (% change between n_vec steps)
		res_pred_rel<-res_pred_abs[-1,]
	
	# computations
		source("000_Auxiliary_Funs\\Sim_Modeling\\func_predSelectMod_MWCV.r")
		source("000_Auxiliary_Funs\\func_do_MWCV.r")
		for( i in 1:length(lst.fitMod_MWCV))
		{
		res_pred_abs[,i]<-predSelectMod_MWCV(lst.fitMod_MWCV, selMod = selModel, sampId = names(lst.fitMod_MWCV)[i], sim_res = sim_res, orig_data = df0, n_vec = n_vec, plot_it = TRUE, save_plot = TRUE,  save_dir="004_Sim_Analysis\\Modeling\\")$pred	
		res_pred_rel[,i]<-round(diff(res_pred_abs[,i])/res_pred_abs[1:(nrow(res_pred_abs)-1),i]*100,1)
		}

	res_pred_abs #absolute values 
	res_pred_rel #relative improvement obtained 

		# worst case scenario
		apply(round(res_pred_abs,0),1, max)
		apply(round(res_pred_rel,2),1, min)
		# median
		apply(round(res_pred_abs,0),1, median)
		# quantiles
		apply(round(res_pred_abs,0),1, function(x) quantile(x, probs=c(0.025,0.975))
	
	
# =========================
# inverse prediction of selected model to obtain a target MWCV [all samples]
# =========================
	
	# set inverse prediction range
		invpred_range<-seq(70,20, by=-5)
	
	# storage object to hold the absolute values of predictions
		res_invpred_abs<-matrix (ncol=length(samples_to_analyze), nrow=length(invpred_range))
		colnames(res_invpred_abs)<-samples_to_analyze
		rownames(res_invpred_abs)<-as.character(invpred_range)

	# select model
		selModel = "mod3"
	
		# computations
		source("000_Auxiliary_Funs\\Sim_Modeling\\func_invpredSelectMod_MWCV.r")
		for( i in 1:length(lst.fitMod_MWCV))
		{
		res_invpred_abs[,i]<-round(invpredSelectMod_MWCV(lst.fitMod_MWCV, sampId = names(lst.fitMod_MWCV)[i], mwcv_vec = invpred_range, selMod = selModel)$invpred)	
		}

	# graph [No. Indivs]
		dt_pred<-reshape::melt(res_pred_rel)
		#boxplot(dt$value~I(dt$X1-10), main = "% improvement in MWCV obtained from +10 additional indivs") #improvement on 10 individuals
		windows(10,5); par(mfrow=c(1,2)) 
		boxplot(abs(dt_pred$value)~I(dt_pred$X1-10), main = "% improvement from + 10 indivs", las=2, cex.axis=.7, xlab= "sample size", ylab = "expect improv. in MWCV (%)", cex.main=1, ylim=c(0, max(abs(dt_pred$value))))  #improvement on 10 individuals (inversed)
		abline(h=5, col=2, lty=2)
		abline(h=2.5, col=4, lty=2)
		legend("topright", legend = c("5% decrease","2.5% decrease"), lty=2, col=c(2,4), cex=0.7)
		dt_invpred<-reshape::melt(res_invpred_abs)	
		boxplot(dt_invpred$value~dt_invpred$X1, main = "Expected n for target", xlab = "target MWCV", ylab = "expected n", las=2, cex.axis=.7, cex.main=1) #improvement on 10 individuals
		abline(h=200, col=2, lty=2)
		abline(h=100, col=4, lty=2)
		legend("topright", legend = c("200 indivs","100 indivs"), lty=2, col=c(2,4), cex=0.7)
		title(main=paste("MWCV", lst.fitMod_MWCV[[1]]$target_variable), line=-1, outer=T)
		savePlot(file=paste("004_Sim_Analysis\\Modeling\\SelectModel_MWCV_",lst.fitMod_MWCV[[1]]$target_variable,"_InvPred_graph.png",sep=""), type="png")

		# determination of sample size
			# median number of individuals requires to attain indicator levels
			apply(round(res_invpred_abs,0),1, median)
		
		
	# graph [Weight]
		# to do

	