# ====================	
# faz_sim_sample
# ====================	
	
	# Nuno Prista, 2017-2018
	
	
	# 2018-09-10: "extracted from sample_level_funs1.r"


faz_sim_sample<-function(sampDes, sampOpt, df1o){ #browser()
# Nuno Prista 2017

# 2018-05-22: single stage now returns indivId
# 2018-05-22: added functionality: stop option for when replacement==FALSE
# 2018-05-23: added functionality - one stage, non-stratified - now allows "sampling all available" when WR

require(reshape)

# creates storage object	
ls_out_sims<-sapply(as.character(sampOpt$samp_sizes), function(x) NULL)

strata_var<-sampOpt$strata_var

# check compatibility between sampling_design and sampling_options
	if (sampDes$stratified == TRUE & (sampOpt$stratified==FALSE | (sampOpt$stratified==TRUE & sampOpt$strata_var!=sampDes$strata_var)))
		{
		stop (cat("\n ATT: sampling_design not compatible with sampling_options \n\n "))
		}

# check on columns
	if(sampOpt$stratified==TRUE)
		{
		if(!strata_var %in% colnames(df1o)) {stop (cat("\n ATT: check strata_var - not found in colnames(df1o) \n\n "))}
		}	

# 
	if(sampOpt$stages=="one" & !is.na(sampOpt$stage1_samp_size))
		{ 
		cat("\n WARNING: stage1_samp_size defined with stage == one?  \n\n")
		}
		
# check on sample sizes
	if(sampOpt$stages=="one" & sampOpt$replacement==FALSE)
		{
		if(any(sampOpt$samp_sizes>nrow(df1o)) & sampOpt$sample_all_available==FALSE) {stop (cat("\n ATT: samp_sizes >  population size with replace = FALSE AND sample_all_available = FALSE\n\n"))}
		if(sampOpt$stratified==TRUE){if (any(sampOpt$samp_sizes>min(table(df1o[,strata_var]))) & sampOpt$sample_all_available==FALSE) {stop (cat("\n ATT: samp_sizes >  strata size with replace = FALSE AND sample_all_available = FALSE\n\n"))}}
		}

	if(sampOpt$stages=="two" & sampOpt$replacement==FALSE)
		{ 
		if(sampOpt$stage1_samp_size>nrow(df1o)) {stop (cat("\n ATT: stage1_samp_size > population size with replace = FALSE \n\n"))}
		if(sum(sampOpt$samp_sizes>sampOpt$stage1_samp_size)>0  & sampOpt$stratified == FALSE & sampOpt$sample_all_available==FALSE) {stop (cat("\n ATT: at least 1 samp_sizes > stage1_samp_size \n\n"))}
		if(sampOpt$stratified == TRUE)
			{
			if(any(sampOpt$samp_sizes>min(table(df1o[,strata_var]))) & sampOpt$sample_all_available==FALSE) {stop (cat("\n ATT: at least 1 samp_sizes > 2nd stage class size with sample_all_available == FALSE \n\n"))}
			}
		}

	
# simulation algorithm
	# runs on each sample size j
#browser()	
	for (j in 1:length(sampOpt$samp_sizes))
	{

				#print(paste("Simulating sample size", sampOpt$samp_sizes[j]))		
						
						# creates a list to hold results of simulations (each leaf is a simulation of size j)	
												
							out<-sapply(as.character(1:sampOpt$n_sims), function(x) NULL)
							if(sampOpt$stratified)
							{
							sample_size_by_strata<-rep(sampOpt$samp_sizes[j], length(unique(df1o[,strata_var])))
							names(sample_size_by_strata)<-unique(factor(df1o[,strata_var]))
							}
					
						# runs each simulation	
							for(i in 1:sampOpt$n_sims)
								{
								#browser()
								if (sampOpt$stages=="one") {df1<-df1o}
								if (sampOpt$stages=="two") {
										df1<-df1o[sample(nrow(df1o), size=sampOpt$stage1_samp_size, replace=FALSE),]
										 if(sampOpt$stratified){
											 sample_size_by_strata<-rep(sampOpt$samp_sizes[j], length(unique(df1[,strata_var])))
											 names(sample_size_by_strata)<-unique(factor(df1[,strata_var]))
											 }
										}
				
								
								# sampling of individuals [note: what is sampled are the rows]
									# simple random sampling [SRS non-stratified]
									if(!sampOpt$stratified)
										{
											if(sampOpt$stages =="one")
												{
												if (sampOpt$replacement == FALSE & sampOpt$sample_all_available==TRUE & sampOpt$samp_sizes[j]>nrow(df1))
													{
													if(sampOpt$sample_all_available_warning==TRUE){print("sampling all available")}
													out[[i]]$'1st_Stage'<-df1$indivId[as.vector(sample(1:nrow(df1), size = nrow(df1), replace = sampOpt$replacement))]
													} else {
															out[[i]]$'1st_Stage'<-df1$indivId[as.vector(sample(1:nrow(df1), size = sampOpt$samp_sizes[j], replace = sampOpt$replacement))]
															}
													# if(sampOpt$replacement == TRUE)
														# {
														# out[[i]]$'1st_Stage'<-df1$indivId[as.vector(sample(1:nrow(df1), size = sampOpt$samp_sizes[j], replace = sampOpt$replacement))]
														# } else
															# {
															# if (sampOpt$sample_all_available==TRUE)
																# {
																# if(sampOpt$sample_all_available_warning==TRUE){print("sampling all available")}
																# out[[i]]$'1st_Stage'<-df1$indivId[as.vector(sample(1:nrow(df1), size = nrow(df1), replace = sampOpt$replacement))]
																# } else {
																	# out[[i]]$'1st_Stage'<-df1$indivId[as.vector(sample(1:nrow(df1), size = sampOpt$samp_sizes[j], replace = sampOpt$replacement))]
																	# }
																
															# }
												out[[i]]$'2nd_Stage'<-"Not Applicable"
												}	
											
											if(sampOpt$stages =="two")
												{
												if (sampOpt$replacement == FALSE & sampOpt$sample_all_available==TRUE & sampOpt$samp_sizes[j]>nrow(df1))
													{
													if(sampOpt$sample_all_available_warning==TRUE){print("sampling all available")}
													out[[i]]$'1st_Stage'<-df1$indivId
													out[[i]]$'2nd_Stage'<-df1$indivId[as.vector(sample(1:nrow(df1), size = nrow(df1), replace = sampOpt$replacement))]
													} else {
															out[[i]]$'1st_Stage'<-df1$indivId
															out[[i]]$'2nd_Stage'<-df1$indivId[as.vector(sample(1:nrow(df1), size = sampOpt$samp_sizes[j], replace = sampOpt$replacement))]
															}
												}
										}				
									# stratified simple random sampling [SRS stratified]
										if(sampOpt$stratified)
										{
											if(sampOpt$stages =="one")
												{
												#browser()
												# samples first stage (df1o)
												#browser()
													out[[i]]$'1st_Stage'<-sampleFromAGroup(x=df1$indivId, y=df1[,strata_var], nsize=sample_size_by_strata, samp_options = sampOpt)
													out[[i]]$'2nd_Stage'<-"Not Applicable"
													#tmp<-sampleFromAGroup(x=1:nrow(df1o), y=df1o[,strata_var], nsize=sample_size_by_strata, samp_options = sampOpt)
												# picks the characteristics of the individuals from the rows selected in each simulation of sampOpt$samp_sizes[j] into list of sampOpt$n_sims elements, each with sampOpt$samp_sizes[j]
													#out[[i]]<-df1o[as.vector(tmp),unlist(sampOpt$vars_to_keep, use.names = FALSE)] # Note: if dim(out[[i]] < stage1_samp_size it is because not all strata were sampled with sample_size_by_strata intensity (individuals in strata missing)
													#out[[i]]["strata_var"]<-out[[i]][[strata_var]]
												# picks strata_sizes
													#tmp2<-melt(table(df1o[df1o[,strata_var] %in% unique(out[[i]][[strata_var]]), strata_var]))
													#colnames(tmp2)<-c(strata_var,"freq")
												# associates previous two	
													#out[[i]]["strata_size"]<-tmp2$freq[match(out[[i]][[strata_var]],tmp2[[strata_var]])]												
												}
											if(sampOpt$stages =="two")
												{
												 #browser()
												# samples second stage (df1)
													out[[i]]$'1st_Stage'<-df1$indivId
													out[[i]]$'2nd_Stage'<-sampleFromAGroup(x=df1$indivId, y=df1[,strata_var], nsize=sample_size_by_strata, samp_options = sampOpt)
												# picks the characteristics of the individuals from the rows selected in each simulation of sampOpt$samp_sizes[j] into list of sampOpt$n_sims elements, each with sampOpt$samp_sizes[j]	
													#out[[i]]<-df1[as.vector(tmp),unlist(sampOpt$vars_to_keep, use.names = FALSE)] # Note: if < stage1_samp_size it is because not all strata were sampled with sample_size_by_strata intensity (individuals in strata missing)
													#out[[i]]["strata_var"]<-out[[i]][[strata_var]]
												# picks strata_sizes
													#tmp2<-melt(table(df1[df1[,strata_var] %in% unique(out[[i]][[strata_var]]), strata_var]))
													#colnames(tmp2)<-c(strata_var,"freq")
												# associates previous two
													#out[[i]]["strata_size"]<-tmp2$freq[match(out[[i]][[strata_var]],tmp2[[strata_var]])]
												}	
										}
								}


			ls_out_sims[[as.character(sampOpt$samp_sizes[j])]]<-out					
	}
	
ls_out_sims	
}
