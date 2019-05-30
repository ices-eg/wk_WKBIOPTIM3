# =======================
# Function to predict exponential models [one sample]
# =======================
	
	# Nuno Prista, SLU Aqua, Sweden @ WKBIOPTIM 2018-2019 (based on previous work done 2011-2015 @ IPMA, Portugal, with Ernesto Jardim)

	# use after fitMod_MWCV

	# 20190526: added "MWCV" and "target_variable" to output name
	# 20190526: added "target_variable" to graph title
	
	
predAllMod_MWCV<-function(lst.fitMod, orig_data, sampId, n_vec, plot_it=TRUE, save_plot= FALSE, save_dir){

# lst.fitMod is output of fitMod_MWCV with 3 model fits
# orig_data is the original output of simulations - including the sample data
# sampId is the target sampId
# n_vec is a vector of sample sizes for predictions

mod1<-lst.fitMod[[sampId]]$mod1
mod2<-lst.fitMod[[sampId]]$mod2
mod3<-lst.fitMod[[sampId]]$mod3
target_variable <- lst.fitMod[[sampId]]$target_variable

# model predictions
	out<-data.frame(n = n_vec, pred1 = NA, pred2 = NA, pred3 = NA)
	cat(".Mod1 Exp\n")
	out$pred1<-coef(mod1)[1]*exp(-coef(mod1)[2]*n_vec)
	cat(".Mod2 Exp (boxcox.nls model)\n")
	out$pred2<-coef(mod2)[1]*exp(-coef(mod2)[2]*n_vec)
	cat(paste(".Mod3 Exp (y-lambda (",mod2$lambda$lambda,") model)\n", sep=""))
	pred_transf<-coef(mod3)[1]*exp(-coef(mod3)[2]*n_vec)
	out$pred3<-(pred_transf*mod2$lambda$lambda+1)^(1/mod2$lambda$lambda)


# model graphs
	if(plot_it)
	{
	windows(10,5); par(mfrow=c(1,2))
	ylimite = c(0, max(orig_data[[sampId]][[target_variable]]$MWCV))
	boxplot(MWCV~sim, data = lst.fitMod[[sampId]]$data, xlab="sim", ylab = "MWCV", ylim=ylimite)
	plot(MWCV~sim, data = orig_data[[sampId]][[target_variable]], xlim=c(0, max(n_vec)), ylim=ylimite)
	points(MWCV~sim, data = tail(orig_data[[sampId]][[target_variable]],1), pch=21, bg="red")
		# Mod1 lines
		points(coef(mod1)[1]*exp(-coef(mod1)[2]*n_vec)~n_vec, col=2, pch=19, type="o")
		# Mod2 lines
		points(coef(mod2)[1]*exp(-coef(mod2)[2]*n_vec)~n_vec, col=3, pch=19, type="o")
		# Mod3 lines
		points(((coef(mod3)[1]*exp(-coef(mod3)[2]*n_vec)*mod2$lambda$lambda+1)^(1/mod2$lambda$lambda))~n_vec, col=4, pch=19, type="o")
		legend (0,20, legend = c("mod1","mod2","mod3"), pch=19, col = c(2:4), lty=1, cex=0.9)
	title (main = paste(sampId,": ",target_variable, sep=""), outer =T, line = -2)
	}
# save graphs
	if(save_plot)
	{
	savePlot(file=paste(save_dir, "AllModels_",sampId,"_MWCV_",target_variable,".png", sep=""), type="png")
	dev.off()
	}
out
} 

# example
