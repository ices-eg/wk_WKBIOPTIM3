# =======================
# Function to predict exponential models [one sample]
# =======================
	
	# Nuno Prista, SLU Aqua, Sweden @ WKBIOPTIM 2018-2019 (based on previous work done 2011-2015 @ IPMA, Portugal, with Ernesto Jardim)

	# 2019-05-26: change in argument name: orig_data changed to sim_res
	# 2019-05-26: change to account for SRSWR: orig_data -> original sample data
	
	# use after fitMod_MWCV

predSelectMod_MWCV<-function(lst.fitMod, selMod = "mod3", sim_res, orig_data, sampId, n_vec, plot_it=TRUE, save_plot= FALSE, save_dir){

# lst.fitMod is output of fitMod_MWCV with 3 model fits
# sim_res is the original output of simulations - including the sample data
# sampId is the target sampId
# n_vec is a vector of sample sizes

mod<-lst.fitMod[[sampId]][[selMod]]
lambda<-lst.fitMod[[sampId]][["lambda"]]
target_variable = lst.fitMod[[sampId]]$target_variable

# model predictions
	out<-data.frame(n = n_vec, pred = NA)
	if(selMod == "mod1")
		{
		cat(".Mod1 Exp\n")
		out$pred<-coef(mod)[1]*exp(-coef(mod)[2]*n_vec)
		}
	if(selMod == "mod2")
		{		
		cat(".Mod2 Exp (boxcox.nls model)\n")
		out$pred<-coef(mod)[1]*exp(-coef(mod)[2]*n_vec)
		}
	if(selMod == "mod3")
		{	
		cat(paste(".Mod3 Exp (y-lambda (",lambda,") model)\n", sep=""))
		pred_transf<-coef(mod)[1]*exp(-coef(mod)[2]*n_vec)
		out$pred<-(pred_transf*lambda+1)^(1/lambda)
		out$pred<-((coef(mod)[1]*exp(-coef(mod)[2]*n_vec))*lambda+1)^(1/lambda)
		}

# model graphs
	if(plot_it)
	{
	par(mfrow=c(1,2))
	boxplot(MWCV~sim, data = lst.fitMod[[sampId]]$data)
	ylimite = c(0, max(sim_res[[sampId]][[target_variable]]$MWCV))
	plot(MWCV~sim, data = sim_res[[sampId]][[target_variable]], xlim=c(0, max(n_vec)), ylim=ylimite)
	orig_mwcv<-do_MWCV(orig_data[orig_data$sampId==sampId,target_variable])
	orig_n<-length(orig_data[orig_data$sampId==sampId,target_variable])
	points(orig_mwcv~orig_n, pch=21, bg="red")
		# Mod1 and Mod2 lines
		if(selMod %in% c("mod1","mod2"))
			{
			if(selMod == "mod1") points(coef(mod)[1]*exp(-coef(mod)[2]*n_vec)~n_vec, col=2, pch=19, type="l")
			if(selMod == "mod2") points(coef(mod)[1]*exp(-coef(mod)[2]*n_vec)~n_vec, col=3, pch=19, type="l")
			}
		# Mod3 lines
		if(selMod == "mod3")
			{				
			points(((coef(mod)[1]*exp(-coef(mod)[2]*n_vec))*lambda+1)^(1/lambda)~n_vec, col=4, pch=19, type="l")
			}
	title (main = paste(sampId,": ",target_variable, sep=""), outer=T, line=-2)
	}

	if(save_plot)
	{
	savePlot(file=paste(save_dir, "SelectModel_",sampId,"_MWCV_",target_variable,"_",selMod,".png", sep=""), type="png")
	dev.off()
	}	
	
out
} 