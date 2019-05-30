# =======================
# Function to fit exponential models to CV
# =======================

	# Nuno Prista, SLU Aqua, Sweden @ WKBIOPTIM 2018-2019 (based on previous work done 2011-2015 @ IPMA, Portugal, with Ernesto Jardim)

	# use with caution requires "boxcox.nls.R" [extracted from deprecated package "nlrwr"9
	
	# takes in dataset of simulations, a target continuous variable (e.g., lenCls, age), a group of sampIds, and a sequence of lambda (for boxcox transf)
	# outputs a list with the results and data used in 3 models
		# mod1: regular nls fit of exponential model assuming normality of errors
		# mod2: update of nls after boxcox.nls (both x and y transformed?)
		# mod3: nls fit of exponential model to the y-boxcox transformed 

# requires "boxcox.nls.R" [extracted from deprecated package "nlrwr"] - use at own risk ;)
		#source("000_Auxiliary_Funs/boxcox.nls2.R")

		# 2019-05-26: created from fitMod_MWCV
		# 2019-05-26: set x to as.data.frame(x) in boxcox.nls [used if x is data.table]	
	
fitMod_CV<-function(dataset, target_variable, sampIds, lambda = seq(-3,-1,by=1/10)){ 
	out<-sapply(sampIds, function(x) NULL)
	for (id in sampIds)
		{
			cat(id,"\n")
			x<-dataset[[target_variable]]
			x<-x[x$sampId == id,]
			if (target_variable == "age" & sum(x$cv==0)) {print(paste(sum(x$cv==0),"replicates removed because cv==0")); x<-x[x$cv!=0,]}
			cat(".Mod1 Exp\n")
			es <- unname(coef(lm(log(cv)~sim, data=x)))
			try(mod1<-nls(cv~k*exp(-b*sim), data = x, start=list(k=exp(es[1]), b=-es[2])))
			cat(".Mod2 Exp (boxcox.nls model)\n")
			mod2<-boxcox.nls(mod1, x = as.data.frame(x), lambda = lambda, plotit =FALSE)  # tot = 0.4
			#print(mod2$lambda$lambda)
			cat(paste(".Mod3 Exp (y-lambda (",mod2$lambda$lambda,") model)\n", sep=""))
			x$transf<-((x$cv^mod2$lambda$lambda)-1)/mod2$lambda$lambda
			es <- unname(coef(lm(log(transf)~sim, data=x)))
			try(mod3<-nls(transf~k*exp(-b*sim), data = x, start=list(k=exp(es[1]), b=-es[2]), control = nls.control(maxiter = 100, tol = 1e-05, minFactor = 1/1024,printEval = FALSE, warnOnly = TRUE)))	
			out[[id]]<-list(data = as.data.frame(x), mod1 = mod1, mod2 = mod2, mod3 = mod3, lambda = mod2$lambda$lambda, target_variable = target_variable)
		}
	out
	}