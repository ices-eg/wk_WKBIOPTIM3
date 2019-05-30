# =======================
# Function to predict exponential models [one sample]
# =======================
	
	# Nuno Prista, SLU Aqua, Sweden @ WKBIOPTIM 2018-2019 (based on previous work done 2011-2015 @ IPMA, Portugal, with Ernesto Jardim)

	# use after fitMod_MWCV

diagAllMod_MWCV<-function(lst.fitMod, sampId, save_plot=TRUE, save_dir){

# diagnostics of one model

# lst.fitMod is output of fitMod_MWCV with 3 model fits
# sampId is the target sampId
# save_plot saves to target directory

mod1<-lst.fitMod[[sampId]]$mod1
mod2<-lst.fitMod[[sampId]]$mod2
mod3<-lst.fitMod[[sampId]]$mod3
target_variable <- lst.fitMod[[sampId]]$target_variable

windows(10,7); par(mfrow=c(3,4))
hist(residuals(mod1), col=2)
		plot(residuals(mod1)~predict(mod1))
		plot(scale(residuals(mod1)))
		qqnorm(residuals(mod1));qqline(residuals(mod1))	
hist(residuals(mod2), col=3)
		plot(residuals(mod2)~predict(mod2))
		plot(scale(residuals(mod2)))
		qqnorm(residuals(mod2));qqline(residuals(mod2))
hist(residuals(mod3), col=4)
		plot(residuals(mod3)~predict(mod3))
		plot(scale(residuals(mod3)))
		qqnorm(residuals(mod3));qqline(residuals(mod3))
title (main =  paste(sampId,": ",lst.fitMod[[sampId]]$target_variable, sep=""), outer=T, line=-2)

# save graphs
	if(save_plot)
	{
	savePlot(file=paste(save_dir, "ModelDiagnostics_",sampId,"_MWCV_",target_variable,".png", sep=""), type="png")
	dev.off()
	}
} 

# example:
