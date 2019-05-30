# ====================	
# do_boxplox_sims_numeric
# ====================	
	
	
	# Nuno Prista, SLU Aqua, Sweden @ ICES WKBIOPTIM
		
	# 2018-09-10: created as faz_graph_sample_numeric
	# 2019-05-26: renamed from faz_graph_sample_numeric to do_boxplox_sims_numeric
	# 2019-05-26: annotations improved
	# 2019-05-26: extended to SRSWR case by adding y argument (when SRWR true values are not in x)

		do_boxplox_sims_numeric <- function (x, y, variables = c("lenCls","age"), stats = c("cv","MWCV"), escala = best_scale, save_plot = TRUE, filename_root, dir_output = "004_Sim_Analysis/")
		{
		# determines number of columns (numeric variables)
			# x is the output from 003_sim_data (res_sims)
			# y is the original dataset
			# variables is a string of variables (e.g., lenCls)
			# stats is a string of stats (e.g., MWCV)
			# requires do_CV_mean and do_MWCV
			
		ncolunas = length(variables)
		
		windows(10,7); par(mfcol=c(2,ncolunas))
			for (a in names(x))
				{
				for (variable in variables)
				{
				DT <- x[[a]][[variable]]
				df1 <- df0[df0$sampId == a, variable]
				for (stat in stats)
					{
					if (stat == "cv") {
								boxplot(cv~sim, data=DT, main=paste(variable,"CV of the mean",a), xlab = "sample size", las=2, cex.axis=1.1, cex.main=1.5, ylim=escala[[variable]][[stat]])
								abline(h=do_CV_mean(df1), col="blue", lty=2, lwd=2)
								}
					if (stat == "MWCV") {
								boxplot(MWCV~sim, data=DT, main=paste(variable,"MWCV of the sample",a), xlab = "sample size", las=2, cex.axis=1.1, cex.main=1.5, ylim=escala[[variable]][[stat]])
								abline(h=do_MWCV(df1), col="blue", lty=2, lwd=2)
								}
					if (stat == "max") {
								boxplot(max~sim, data=DT, main=paste(variable,"max of the sample",a), xlab = "sample size", las=2, cex.axis=1.1, cex.main=1.5, ylim=escala[[variable]][[stat]])
								abline(h=max(df1, na.rm=T), col="blue", lty=2, lwd=2)
								}
					if (stat == "min") {
								boxplot(min~sim, data=DT, main=paste(variable,"min of the sample",a), xlab = "sample size", las=2, cex.axis=1.1, cex.main=1.5, ylim=escala[[variable]][[stat]])
								abline(h=min(df1, na.rm=T), col="blue", lty=2, lwd=2)
								}
					}
			}	
			if(save_plot == TRUE)
				{
				savePlot(file = paste(dir_output, filename_root, a,".png", sep=""), type="png")
				} 
			}
}