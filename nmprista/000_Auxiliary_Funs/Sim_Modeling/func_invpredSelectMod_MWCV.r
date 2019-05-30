# =======================
# Function to predict exponential models [one sample]
# =======================
	
	# Nuno Prista, SLU Aqua, Sweden @ WKBIOPTIM 2018-2019

	# use after fitMod_MWCV

invpredSelectMod_MWCV<-function(lst.fitMod, sampId, mwcv_vec, selMod = "mod3"){

# lst.fitMod is output of fitMod_MWCV with 3 model fits
# orig_data is the original output of simulations - including the sample data
# sampId is the target sampId
# n_vec is a vector of sample sizes

mod<-lst.fitMod[[sampId]][[selMod]]
lambda<-lst.fitMod[[sampId]][["lambda"]]

# model inverse predictions
	out<-data.frame(mwcv = mwcv_vec, invpred = NA)
	if(selMod %in% c("mod1","mod2"))
		{
		cat(paste(".",selMod,"\n"))
		out$invpred<-log(mwcv_vec/coef(mod)[1])/(-coef(mod)[2])
		}
	if(selMod == "mod3")
		{
		mwcv_vec_transf<-((mwcv_vec^lambda)-1)/lambda		
		out$invpred<-log(mwcv_vec_transf/coef(mod)[1])/(-coef(mod)[2])
		}
out
} 

# example:
# invpredSelectMod_MWCV(lst.fitMod, sampId = names(lst.fitMod)[1], mwcv_vec = seq(110,10, by=-10), selMod = "mod3")