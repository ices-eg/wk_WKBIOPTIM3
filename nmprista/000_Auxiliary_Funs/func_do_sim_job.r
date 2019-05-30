do_sim_job <- function(lo)	
	{
	ls_DT_compiled<-sapply(sampId, function(x) NULL)
	ls_sims1 <- faz_sim_sample (sampDes = sampling_design, sampOpt = sampling_options, df1o = df1)
	# creates storage object	
	ls_sims_stats<-lapply(sapply(as.character(sampling_options$samp_sizes), function(x) NULL), function(x) sapply(c(variable_table$variable, sampling_options$models), function(x) NULL)) 
	vars_numerical<-variable_table$variable[variable_table$type=="numerical"]
	vars_categorical<-variable_table$variable[variable_table$type=="categorical"]
	ls_original_modes_sample<-ls_original_modes[[sampId]]	
	
	for (j in 1:length(sampling_options$samp_sizes))
			{
			if(!sampling_options$stratified & (sampling_options$stages =="one" | sampling_options$stages =="two")) # not stratified, one or two stages
				{
				#print(paste("Processing sample size", sampling_options$samp_sizes[j]))	
				if(sampling_options$stages == "one") w <- "1st_Stage" else w <- "2nd_Stage"
				 for (variable in vars_numerical)
					{	
					#ptc1<-Sys.time()
					#print(paste(".",variable, sep=""))
					#source("sample_level_funs1.R")
					#print(Sys.time()-ptc1)
					ls_sims_stats[[j]][[variable]]<-do.call("rbind",lapply(ls_sims1[[j]], function(x, y = ls_original_modes_sample){make_summary_numeric(x[[w]], variable, a= coefs_weight_length[["a"]], b=coefs_weight_length[["b"]], ls_original_modes_sample = y, repl = lo)}))
					}
				 for (variable in vars_categorical)
					{	
					#ptc1<-Sys.time()
					#print(paste(".",variable, sep=""))
					#source("sample_level_funs1.R")
					ls_sims_stats[[j]][[variable]]<-do.call("rbind",lapply(ls_sims1[[j]], function(x){make_summary_categorical(x[[w]], variable, repl = lo)}))
					#print(Sys.time()-ptc1)
					}	

				# adds weight estimate from lenCls to all variables
					if ("lenCls" %in% vars_numerical)
						{
						for (variable in c(vars_numerical, vars_categorical)[c(vars_numerical, vars_categorical) != "lenCls"] )
							{
						ls_sims_stats[[j]][[variable]]$estim_weight<-ls_sims_stats[[j]][["lenCls"]]$estim_weight
							}
						}

				# models: 
					#print(".models")
					if("weight-length" %in% sampling_options$models) ls_sims_stats[[j]][["weight-length"]]<-do.call("rbind",lapply(ls_sims1[[j]], function(x, model){make_models_random(x[[w]], model="weight-length", repl = lo)}))
					if("sex-ratio" %in% sampling_options$models) ls_sims_stats[[j]][["sex-ratio"]]<-do.call("rbind",lapply(ls_sims1[[j]], function(x, model){make_models_random(x[[w]], model="sex-ratio", repl = lo)}))
					if("L50" %in% sampling_options$models) ls_sims_stats[[j]][["L50"]]<-do.call("rbind",lapply(ls_sims1[[j]], function(x, model){make_models_random(x[[w]], model="L50", repl = lo)}))
					if("VBGF" %in% sampling_options$models) ls_sims_stats[[j]][["VBGF"]]<-do.call("rbind",lapply(ls_sims1[[j]], function(x, model){make_models_random(x[[w]], model="VBGF", repl = lo)}))
				 }
			}

			

			
#browser()			
#ls_DT_compiled[[sampId]]<-sapply(c(vars_categorical, vars_numerical), function(x) NULL)

DT<-sapply(c(vars_categorical, vars_numerical), function(x) NULL)

for (variable in c(names(DT),sampling_options$models)[c(names(DT),sampling_options$models)!=""])
{
# compilation of results
DT[[variable]]<-data.frame()
for (i in names(ls_sims_stats))
	{
	DT_sim<-data.frame(sampId = df1$sampId[1], sim = as.numeric(as.character(i)), ls_sims_stats[[i]][[variable]])
	DT[[variable]]<-rbind(DT[[variable]], DT_sim)
	}
#ls_DT_compiled[[sampId]][[variable]]<-DT[[variable]]
}
rm(ls_sims1, ls_auto_modes_sample, vars_numerical, vars_categorical, ls_sims_stats)
gc()

DT
}
