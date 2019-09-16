# 2017-08-22: added expl.analysis.smooth.and.modes
# 2018-05-22: tweaks to expl.analysis.smooth.and.modes
# 2018-05-22: added "func_detect_modes_in_samples" 
# 2018-05-26: added "in_interval" 

expl.analysis.smooth.and.modes <- function( df0 = df0, samples_to_analyze = samples_to_analyze, smooth_class_span = 2, min_proportion_to_accept_mode = 0.01, save_plot = FALSE, dir_save = "001_Exploratory_analysis\\", file_root = paste("001_original_and_smoothed_data_",variable,"_", sep=""))
{
				# added save_plot option
				#browser()
					for (sample_id in samples_to_analyze)
					{
					df1<-df0[df0$sampId == sample_id,]
					sample_threshold_for_modes <- min_proportion_to_accept_mode * nrow(df1)
					# Exploratory checks
						windows(10,7)
						par(mfrow=c(1,2))
							# lengths freq (original and smooth)
								original_freq<-table(factor(df1[[variable]], levels=seq(min(df1[[variable]]), max(df1[[variable]]), by=original_class_span))); original_freq
								tmp.lt<-df1[[variable]]-df1[[variable]]%%smooth_class_span
								smoothed_freq<-table(factor(tmp.lt, levels=seq(min(tmp.lt, na.rm=T), max(tmp.lt, na.rm=T), by=smooth_class_span))); smoothed_freq
							# modes analyses
								original_modes = localMaxima2(as.numeric(table(factor(df1[[variable]], levels=seq(min(df1[[variable]]), max(df1[[variable]]), by=original_class_span)))))
								original_modes_after_threshold<-original_modes[original_modes %in% which(original_freq>sample_threshold_for_modes)]			
								smoothed_modes = localMaxima2(as.numeric(table(factor(tmp.lt, levels=seq(min(tmp.lt, na.rm=T), max(tmp.lt, na.rm=T), by=smooth_class_span)))))
								smoothed_modes_after_threshold<-smoothed_modes[smoothed_modes %in% which(smoothed_freq>sample_threshold_for_modes)]
							# barplots 
								a<-barplot(original_freq, las=2, cex.names=1.1, main="original freq", las=2, cex.axis=1.1, cex.main=1.5)
								abline(h=min_proportion_to_accept_mode*nrow(df1), col="red", lty=2)
								text(x = a[original_modes_after_threshold], y = -0.5, labels = "|", cex=1.5)
								a<-barplot(smoothed_freq, las=2, cex.names=1.1, main=paste("smooth: class span", smooth_class_span), las=2, cex.axis=1.1, cex.main=1.5)
								abline(h=min_proportion_to_accept_mode*nrow(df1), col="red", lty=2)
								text(x = a[smoothed_modes_after_threshold], y = -0.5, labels = "|", cex=1.5)
								title(df1$sampId[1], outer=T, line=-1)
								if (save_plot==TRUE)
									{
									savePlot(filename = paste(dir_save, file_root, df1$sampId[1],".png", sep=""), type = "png")				
									dev.off()
									} else {
										keyPressed = readkeygraph("[press enter to continue]")
										dev.off()
										}
					}

				}


readkeygraph <- function(prompt)
{
    getGraphicsEvent(prompt = prompt, 
                 onMouseDown = NULL, onMouseMove = NULL,
                 onMouseUp = NULL, onKeybd = onKeybd,
                 consolePrompt = "[click on graph then follow top prompt to continue]")
    Sys.sleep(0.01)
    return(keyPressed)
}

onKeybd <- function(key)
{
    keyPressed <<- key
}				
				
func_detect_modes_in_samples<-function(x = droplevels(df0[df0$sampId %in% samples_to_analyze,]), variable = "lenCls", original_class_span = original_class_span, smooth_class_span = smooth_class_span, min_proportion_to_accept_mode = min_proportion_to_accept_mode)
{
	# identifies modes and stores results of modal analyses
		# note: numerical and categorical variables are processed differently
	
  
  
  # x = droplevels(df0[df0$sampId %in% samples_to_analyze,])
	
	

	# x <- df1[,c(sampling_design$strata_var, var1)]
				# t1<-table(df1[sampling_options$strata_var])
				# colnames(x)<-c("strata_id", variable)
					# t2<-table(x$strata_id)
					# x$total_strata <- t1[match(x$strata_id, names(t1))]
				# x$samp_weight <- x$total_strata/(t2[match(x$strata_id, names(t2))])					
				# freq_dist_pop_estimate_no_NAs<-tapply(x$samp_weight, x[,variable], sum)
	
	if (is.character(x[[variable]])) {x[[variable]]<-factor(is.character(x[[variable]]))}
	
	out<-sapply(unique(x$sampId), function(x) NULL)
	
	ls1<-split(x, x$sampId)
		
	ls2<-lapply(ls1, function(x){ print(paste("processing sample", as.character(x$sampId[1]))); 
	print(variable)
		# if stratified and variable is not the stratification variable, then performs the raising 
			if(sampling_design$stratified==TRUE & variable!=sampling_design$strata_var)
				{
					
				# computes totals for strata_var
					if(sampling_design$strata_var %in% variable_table$variable[variable_table$variable=="numerical"])
						{
						t1<-table(factor(x[[sampling_design$strata_var]], levels=seq(min(x[[sampling_design$strata_var]], na.rm=T), max(x[[sampling_design$strata_var]], na.rm=T), by=variable_table[variable_table$variable == sampling_design$strata_var, "original_class_span"])))
						} else
							{
							t1<-table(x[[sampling_design$strata_var]], useNA="al")
							}
				# extracts strata_var and target_var
					#browser()
					x <- x[,c(sampling_design$strata_var, variable)]
				colnames(x)<-c("strata_id", variable)
				# computes raised freq dist [NAs are considered]
					t2<-table(x$strata_id, useNA="al")
					x$total_strata <- t1[match(x$strata_id, names(t1))]
					x$samp_weight <- x$total_strata/(t2[match(x$strata_id, names(t2))])
					print(sum(x$samp_weight)==nrow(x))
					freq_dist_pop_estimate<-tapply(x$samp_weight, factor(x[,variable], exclude=NULL), sum)
					freq_dist_pop_estimate_no_NAs<-freq_dist_pop_estimate[!is.na(names(freq_dist_pop_estimate))]
				# creates dummy dataset to enter analysis
				x<-data.frame(1,rep(names(freq_dist_pop_estimate),freq_dist_pop_estimate))	
				names(x)<-c("dummy",variable)
				if(variable_table[variable_table$variable==variable,"type"]=="numerical"){x[[variable]]<-as.numeric(x[[variable]])}
				}
	
		if(!is.factor(x[[variable]]))
			{
		# frequency tables (original and smooth) [NAs are not considered]
			
			original_freq<-table(factor(x[[variable]], levels=seq(min(x[[variable]], na.rm=T), max(x[[variable]], na.rm=T), by=original_class_span)), useNA="al")
			tmp.lt<-x[[variable]]-x[[variable]]%%smooth_class_span
			smoothed_freq<-table(factor(tmp.lt, levels=seq(min(tmp.lt, na.rm=T), max(tmp.lt, na.rm=T), by=smooth_class_span))); smoothed_freq
			} else {
					original_class_span<-NA
					smooth_class_span<-NA
					original_freq<-table(x[[variable]], useNA="al")
					smoothed_freq<-NA
					}
			
			
		# modes determination
			sample_threshold_for_modes <- min_proportion_to_accept_mode * nrow(x)
			
			if(length(unique(table(x[,variable])))>1) # condition for mode existence
				{
				if(!is.factor(x[[variable]])) # different processing of numerical and categorical variable
				{
					original_modes = localMaxima2(as.numeric(table(factor(x[[variable]], levels=seq(min(x[[variable]], na.rm=T), max(x[[variable]], na.rm=T), by=original_class_span)))))
					original_modes_after_threshold<-original_modes[original_modes %in% which(original_freq[!is.na(names(original_freq))]>sample_threshold_for_modes)]			
					if (length(unique(x[[variable]]))>1){
							smoothed_modes = localMaxima2(as.numeric(table(factor(tmp.lt, levels=seq(min(tmp.lt, na.rm=T), max(tmp.lt, na.rm=T), by=smooth_class_span)))))
							smoothed_modes_after_threshold<-smoothed_modes[smoothed_modes %in% which(smoothed_freq>sample_threshold_for_modes)]
							} else { smoothed_modes = original_modes; original_modes_after_threshold = original_modes_after_threshold}
					} else {
							original_modes = localMaxima2(as.numeric(table(x[[variable]])))
							original_modes_after_threshold<-original_modes[original_modes %in% which(original_freq>sample_threshold_for_modes)]			
							smoothed_modes <- NA
							smoothed_modes_after_threshold <- NA
						}
				} else {
						original_modes <- NA
						original_modes_after_threshold <- NA		
						smoothed_modes <- NA
						smoothed_modes_after_threshold <- NA						
						}
				
			ls_auto_modes<-list()
		#browser()
			ls_auto_modes [[variable]]<- list (
												total_n=nrow(x),
												NAs=sum(is.na(x[[variable]])),
												original_class_span = original_class_span,
												original_breaks = names(original_freq),
												original_freq = original_freq, 
												original_modes = original_modes_after_threshold, #NAs are excluded
												smooth_class_span = smooth_class_span,
												smooth_breaks = names(smoothed_freq),
												smoothed_freq = smoothed_freq,
												smooth_modes = smoothed_modes_after_threshold,
												threshold_for_modes = sample_threshold_for_modes,
												min_proportion_to_accept_mode = min_proportion_to_accept_mode
											)
			ls_auto_modes								
		})									
		ls2
	}


	
in_interval <- function(x, interval){
# adapted from http://r.789695.n4.nabble.com/screen-if-a-value-is-within-range-td4651562.html
									   stopifnot(length(interval) == 2L)
									   interval[1] <= x & x <= interval[2]
									}	
	
	
	
# function that corrects "sample" execution in the case of only one element in the population (more info: ?sample and ?sample.int)
sample2 <- function(x, ...) x[sample.int(length(x), ...)]

sampleFromAGroup<-function(x, y, nsize, samp_options=list(replacement=FALSE, sample_all_available=FALSE, sample_all_available_warning = TRUE)){
# 2015-2016 WP2 FishPI
	# Nuno Prista (adapted from great original work of Liz Clarke, Marine Scotland)

	# 2016-10-17 Nuno Prista: added option sample all when sampling without replacement [see comments]
	# 2016-10-17 Nuno Prista: added sample2 [correction of behaviour of "sample" when only 1 element is being sampled]
	# 2016-10-17 Nuno Prista: improved identification of samples in result
	# 2016-10-18 Nuno Prista: improved code at samp_options level [made independent from position in list - > now easier to add options]
	# 2016-10-18 Nuno Prista: added suppress.warnings to samp_options 
	# 2018-05-29 Nuno Prista: adapted so that one of the group names can be NA (useful in, e.g., stratifying by maturity) 

nGroup<-length(nsize)
xSamp<-NULL
for (i in 1:nGroup) {
print(i)

	if(!is.na(names(nsize)[i]))
		{
		indx<-unique(x[which(y==names(nsize)[i])]) 
		} else {indx <- x [which(is.na(y))] } # nuno 20180529 [handles NAs in stratification]
    # nuno 20161017: condicao para amostrar todos os disponiveis without replacement
    if(samp_options$replacement == FALSE & samp_options$sample_all_available == TRUE & nsize[i]>length(indx)){
            nsize[i] <- length(indx)
            if(samp_options$sample_all_available_warning==TRUE) {print(paste("sampling all available in group",names(nsize)[i]))}
            }
    # nuno 20161017: condicao para existencia de 1 unico elemento a amostrar (corrige comportamento de funcao sample)
    if(length(indx)>1){
        samp<-sample(indx, size=nsize[i], replace = samp_options$replacement)
        } else {samp<-sample2(indx, size=nsize[i], replace = samp_options$replacement)}
    # nuno 20161017: atribui nomes
	names(samp)<-rep(names(nsize)[i], length(samp))
    if(i == 1) {
    xSamp <-samp
    } else {
        xSamp <- c(xSamp, samp)
    }
    }
    return(xSamp)
}


faz_sim_sample<-function(sampDes, sampOpt, df1o){ #browser()
# Nuno Prista 2017

# 2018-05-22: single stage now returns indivId
# 2018-05-22: added functionality: stop option for when replacement==FALSE
# 2018-05-23: added functionality - one stage, non-stratified - now allows "sampling all available" when WR

# df1o = df1_no_NAs
  
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
		if(any(sampOpt$samp_sizes>min(table(df1o[,strata_var])))  & sampOpt$stratified == TRUE & sampOpt$sample_all_available==FALSE) {stop (cat("\n ATT: at least 1 samp_sizes > 2nd stage class size with sample_all_available == FALSE \n\n"))}
		}

	
# simulation algorithm
	# runs on each sample size j
#browser()	
	for (j in 1:length(sampOpt$samp_sizes))
	{

				print(paste("Simulating sample size", sampOpt$samp_sizes[j]))		
						
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

faz_sim_sample_old<-function(sampOpt, df1o) 
  { #browser()
# Nuno Prista 2017


# creates storage object	
ls_out_sims<-sapply(as.character(sampOpt$samp_sizes), function(x) NULL)

strata_var<-sampOpt$strata_var

# simulation algorithm
	# runs on each sample size j
	
	for (j in 1:length(sampOpt$samp_sizes))
	{

				print(paste("Simulating sample size", sampOpt$samp_sizes[j]))		
						
						# creates a list to hold results of simulations (each leaf is a simulation of size j)	
												
							out<-sapply(as.character(1:sampOpt$n_sims), function(x) NULL)
							#browser()
							if(sampOpt$stratified)
							{
							sample_size_by_strata<-rep(sampOpt$samp_sizes[j], length(unique(df1o[,strata_var])))
							names(sample_size_by_strata)<-unique(factor(df1o[,strata_var]))
							}
					
						# runs each simulation	
							for(i in 1:sampOpt$n_sims) # e.g., i=1
								{
								if (sampOpt$stages=="one") {df1<-df1o}
								if (sampOpt$stages=="two") {df1<-df1o[sample(nrow(df1o), size=sampOpt$stage1_samp_size, replace=FALSE),]}
				
								
								# sampling of individuals [note: what is sampled are the rows]
									# simple random sampling [SRS non-stratified]
									if(!sampOpt$stratified)
										{
											out[[i]]<-df1o[as.vector(sample(1:nrow(df1o), size = sampOpt$samp_sizes[j], replace = sampOpt$replacement)),unlist(sampOpt$vars_to_keep, use.names = FALSE)]
										}
									# stratified simple random sampling [SRS stratified]
										if(sampOpt$stratified)
										{
											if(sampOpt$stages =="one")
												{
												# samples first stage (df1o)
													tmp<-sampleFromAGroup(x=1:nrow(df1o), y=df1o[,strata_var], nsize=sample_size_by_strata, samp_options = sampOpt)
												# picks the characteristics of the individuals from the rows selected in each simulation of sampOpt$samp_sizes[j] into list of sampOpt$n_sims elements, each with sampOpt$samp_sizes[j]
													out[[i]]<-df1o[as.vector(tmp),unlist(sampOpt$vars_to_keep, use.names = FALSE)] # Note: if dim(out[[i]] < stage1_samp_size it is because not all strata were sampled with sample_size_by_strata intensity (individuals in strata missing)
													out[[i]]["strata_var"]<-out[[i]][[strata_var]]
												# picks strata_sizes
													tmp2<-melt(table(df1o[df1o[,strata_var] %in% unique(out[[i]][[strata_var]]), strata_var]))
													colnames(tmp2)<-c(strata_var,"freq")
												# associates previous two	
													out[[i]]["strata_size"]<-tmp2$freq[match(out[[i]][[strata_var]],tmp2[[strata_var]])]												
												}
											if(sampOpt$stages =="two")
												{
												# samples second stage (df1)
													tmp<-sampleFromAGroup(x=1:nrow(df1), y=df1[,strata_var], nsize=sample_size_by_strata, samp_options = sampOpt)
												# picks the characteristics of the individuals from the rows selected in each simulation of sampOpt$samp_sizes[j] into list of sampOpt$n_sims elements, each with sampOpt$samp_sizes[j]	
													out[[i]]<-df1[as.vector(tmp),unlist(sampOpt$vars_to_keep, use.names = FALSE)] # Note: if < stage1_samp_size it is because not all strata were sampled with sample_size_by_strata intensity (individuals in strata missing)
													out[[i]]["strata_var"]<-out[[i]][[strata_var]]
												# picks strata_sizes
													tmp2<-melt(table(df1[df1[,strata_var] %in% unique(out[[i]][[strata_var]]), strata_var]))
													colnames(tmp2)<-c(strata_var,"freq")
												# associates previous two
													out[[i]]["strata_size"]<-tmp2$freq[match(out[[i]][[strata_var]],tmp2[[strata_var]])]
												}	
										}
								}


			ls_out_sims[[as.character(sampOpt$samp_sizes[j])]]<-out					
	}


	
	
ls_out_sims	
}

localMaxima <- function(x) {
 # from https://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima  
 # Use -Inf instead if x is numeric (non-integer)
   y <- diff(c(-.Machine$integer.max, x)) > 0L
  rle(y)$lengths
  y <- cumsum(rle(y)$lengths)
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}	

localMaxima2<-function(x){
# Nuno Prista 2017-06-08
# Adaptation of localMaxima to avoid false detection of plateau in upward sequences AND detect all values in plateau if maxima
# looks like a small thing but took over 3 hours of work to get to result...
# based on https://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima and https://stackoverflow.com/questions/7509381/identifying-sequences-of-repeated-numbers-in-r

	b<-cumsum(rle(x)$lengths)[localMaxima(rle(x)$values)]; b
	elements_in_sequence_of_equal_values<-which(rep(rle(x)$lengths >= 2,times = rle(x)$lengths)==TRUE)
	for (i in 1:length(x))
		{
		if(
			x[i] %in% x[b] # i.e., if x[i] is max 
			& i%in% elements_in_sequence_of_equal_values # and i is part of a sequence 
				& !i%in%b) # and i in not yet in the set
				{
				b<-c(b,i)	# adds i to the set
				}
		}
	sort(b)
}

lrPerc <-function(cf,p) (log(p/(1-p))-cf[1])/cf[2] 


make_summary_numeric<-function(y, variable, a ,b){ #browser()
# Nuno Prista 2017
	# wishlist - add DI and other indexes from Chih, 2010
	
	# 20180526 - adapted to indivId as input
	# 20180526 - naming of objects improved (less portuguese now)
	# 20180526 - moved to arguments the parameters of the weight - length relationship
	# 20180526 - n_class_sampled_x now excludes NAs 
	# - CHECK - 20180526 - added original number of modes 
	#browser()
  
  #y= ls_sims1[[j]][[1]][[w]]
  #b=coefs_weight_length[["b"]]
  #a= coefs_weight_length[["a"]]
  

													x <- df1[variable][match(y, df1$indivId),variable]
													
													# adjustments for processing
														#if(is.vector(x)) {x<-data.frame(dummy=1,x); colnames(x)[2]<-variable}
														if(is.vector(x) | is.factor(x[variable])) {x<-data.frame(dummy=1,x); colnames(x)[2]<-variable}
														# if(is.factor(x[,variable])) x[,variable]<-as.numeric(as.character(x[,variable]))
														# if(is.factor(df1[,variable])) df1[,variable]<-as.numeric(as.character(df1[,variable]))
													# summary	
														n_indiv = nrow(x)
														NAs_x<-sum(is.na(x[,variable]))
														mean_x<-mean(x[,variable], na.rm=TRUE)
														stand_err_mean_x<-sd(x[,variable], na.rm=TRUE)/sqrt(length(x[,variable]))
														median_x<-median(x[,variable], na.rm=TRUE)
														min_x<- min(x[,variable], na.rm=TRUE)
														max_x<-max(x[,variable], na.rm=TRUE)
														n_class_sampled_x<-length(unique(x[!is.na(x[,variable]),variable])) 
												#ATTT!!!!		
														if(variable == "lenCls") estim_weight_sample <- sum(exp(a)*x$lenCls^b) else {estim_weight_sample<-NA}
													# mean weighed CV	
														sigma_i<-sqrt(nrow(x)*as.matrix(prop.table(table(x[,variable]))*(1-prop.table(table(x[,variable])))))
														cv_i <- sigma_i / (nrow(x)*as.matrix(prop.table(table(x[,variable]))))
														MWCV<-round(sum(sigma_i)/nrow(x)*100,1)
														
														
													# analysis of frequency distributions (original)
														freq_dist_sampled_indivs <- table( factor(x[,variable]-x[,variable]%%ls_auto_modes_sample[[variable]][["original_class_span"]], levels=ls_auto_modes_sample[[variable]][["original_breaks"]]))
														modes_sampled_individuals <- localMaxima2(as.numeric(freq_dist_sampled_indivs))
														modes_sampled_individuals_after_threshold<-modes_sampled_individuals[modes_sampled_individuals %in% which(freq_dist_sampled_indivs> (ls_auto_modes_sample[[variable]]$min_proportion_to_accept_mode * sum(freq_dist_sampled_indivs)))]	
														n_modes<-length(modes_sampled_individuals_after_threshold)
														# compares modes of individuals sampled with the original modes
															modes_correct<-identical(modes_sampled_individuals_after_threshold,ls_auto_modes_sample[[variable]][["original_modes"]])
															n_modes_correct<-sum(modes_sampled_individuals_after_threshold %in% ls_auto_modes_sample[[variable]][["original_modes"]])
													# analysis of frequency distributions (smooth)															
														freq_dist_sampled_indivs_smooth <- table( factor(x[,variable]-x[,variable]%%ls_auto_modes_sample[[variable]][["smooth_class_span"]], levels=ls_auto_modes_sample[[variable]][["smooth_breaks"]]))
														modes_sampled_individuals_smooth <- try(localMaxima2(as.numeric(freq_dist_sampled_indivs_smooth)), silent=TRUE)
														if (class(modes_sampled_individuals_smooth) == "try-error") {
														  modes_sampled_individuals_smooth <- 1
														}
														modes_sampled_individuals_smooth_after_threshold<-modes_sampled_individuals_smooth[modes_sampled_individuals_smooth %in% which(freq_dist_sampled_indivs_smooth> (ls_auto_modes_sample[[variable]]$min_proportion_to_accept_mode * sum(freq_dist_sampled_indivs_smooth)))]	
														n_modes_smooth = length(modes_sampled_individuals_smooth_after_threshold)
														# compares smoothed modes of individuals sampled with the original smoothed modes														
															modes_correct_smooth<-identical(modes_sampled_individuals_smooth_after_threshold,ls_auto_modes_sample[[variable]][["smooth_modes"]])
															n_modes_correct_smooth<-sum(modes_sampled_individuals_smooth_after_threshold %in% ls_auto_modes_sample[[variable]][["smooth_modes"]])														
													# tests on distributions
														ttest_prob <- t.test(x[,variable], df1[,variable])$p.value
														ttest_logic <- ttest_prob >= 0.05
														ks_prob<-ks.test(x[,variable], df1[,variable])$p.value
														kstest_logic <- ks_prob >= 0.05
													
													# output
														data.frame(var=variable, stages = sampling_options$stages, n_stage1 = sampling_options$stage1_samp_size, 
														           strata_var = sampling_options$strata_var, n = n_indiv, estim_weight = estim_weight_sample, 
														           NAs_x = NAs_x, mean=mean_x, se = stand_err_mean_x, cv=round(stand_err_mean_x/mean_x*100,1), 
														           min = min_x, median = median_x, max = max_x, n_class_sampled=n_class_sampled_x,
            														n_modes = n_modes, n_modes_smooth = n_modes_smooth, modes_correct = modes_correct, 
            														n_modes_correct = n_modes_correct, modes_correct_smooth = modes_correct_smooth, 
            														n_modes_correct_smooth = n_modes_correct_smooth, 
            														ttest_prob = ttest_prob, ttest_logic = ttest_logic, ks_prob = ks_prob, 
														           kstest_logic = kstest_logic, MWCV = MWCV)																
}														

														
make_summary_categorical <- function(y, variable){ #browser()
# Nuno Prista 2017
#browser()
	# 20180526 - adapted to indivId as input
	# 20180526 - naming of objects improved (less portuguese now)
	# 20180526 - n_class_sampled_x now excludes NAs 

													x <- df1[variable][match(y, df1$indivId),variable]
													
													# adjustments for processing
														x<-data.frame(dummy=1,x); colnames(x)[2]<-variable
														#x[,variable]<-factor(x[,variable], levels=sort(unique(df1[,variable])))
														#df1[,variable]<-factor(df1[,variable], levels=sort(unique(df1[,variable])))
													# summary	
														n_indiv = nrow(x)
														NAs_x<-sum(is.na(x[,variable]))
														n_classes_sampled_x<-length(unique(x[,variable]))
														sampled_classes_x <- paste(sort(unique(x[!is.na(x[,variable]),variable])), collapse=",")
													# analysis of frequency distributions (original) and modes
														freq_dist_sampled_indivs <- table( x[,variable])
														if(length(unique(freq_dist_sampled_indivs))>1)
															{
															modes_sampled_individuals <- localMaxima2(as.numeric(freq_dist_sampled_indivs))
															modes_sampled_individuals_after_threshold<-modes_sampled_individuals[modes_sampled_individuals %in% which(freq_dist_sampled_indivs> (ls_auto_modes_sample[[variable]]$min_proportion_to_accept_mode * sum(freq_dist_sampled_indivs)))]	
															n_modes<-length(modes_sampled_individuals_after_threshold)
															modes_correct<-identical(modes_sampled_individuals_after_threshold,ls_auto_modes_sample[[variable]][["original_modes"]])
															n_modes_correct<-sum(modes_sampled_individuals_after_threshold %in% ls_auto_modes_sample[[variable]][["original_modes"]])
															} else {
																	n_modes = 0
																	modes_correct <- NA
																	n_modes_correct <- NA
													}
													#browser()				
													# chisq test
														if(variable == sampling_options$strata_var)
															{
															expected_probs<-prop.table(table(df1[,variable], useNA="al"))
															realized_sample<-table(x[,variable], useNA="al")
															} else {
																	expected_probs<-prop.table(table(df1[,variable]))
																	realized_sample<-table(x[,variable])
																	}
														if(variable == "matStage") {expected_probs<-expected_probs[expected_probs>0]; realized_sample<-realized_sample[names(expected_probs)]}
														# implements simulated p.value for matStage where approximations seem worse
														if(variable != "matStage") chisq_test_prob <- chisq.test(x=as.vector(realized_sample),p = expected_probs)$p.value else {chisq_test_prob <- chisq.test(x=as.vector(realized_sample),p = expected_probs,simulate.p.value=T, B=1000)$p.value}
														chisq_test_logic<- chisq_test_prob >= 0.05
														
													# Julia's	
														if(variable == sampling_options$strata_var)
															{
															expected_probs<-prop.table(table(df1[,variable], useNA="al"))
															realized_probs<-prop.table(table(x[,variable], useNA="al"))
															} else {
																	expected_probs<-prop.table(table(df1[,variable]))
																	realized_probs<-prop.table(table(x[,variable]))
																	}
														
														L1 <- sum(abs(expected_probs-realized_probs))
														L2 <- sqrt(sum((expected_probs-realized_probs)^2))
														L2a <- sum((expected_probs-realized_probs)^2)
														L3 <- max(expected_probs-realized_probs)
														
													# output
														data.frame(var=variable, stages = sampling_options$stages, n_stage1 = sampling_options$stage1_samp_size, strata_var = sampling_options$strata_var, n = n_indiv, estim_weight=NA, NAs_x = NAs_x, n_class_sampled=n_classes_sampled_x, sampled_classes = sampled_classes_x,
																	n_modes = n_modes, modes_correct = modes_correct, n_modes_correct = n_modes_correct, 
																		chisq_test_prob = chisq_test_prob, chisq_test_logic = chisq_test_logic,																
																		L1 = L1, L2 = L2, L3 = L3, L2a = L2a)																
														}	
														
make_summary_numeric_stratified <- function(y, variable, a ,b){#browser()
# Nuno Prista 2017
	# wishlist - add DI and other indexes from Chih, 2010
	
	# 20180526 - adapted to indivId as input
	# 20180526 - naming of objects improved (less portuguese now)
	# 20180526 - moved to arguments the parameters of the weight - length relationship
	# 20180526 - n_class_sampled_x now excludes NAs 
	# - CHECK - 20180526 - added original number of modes 
	
	# check samp weights with replacement and without
	
	
	# Note: n_class_sampled_x excludes NAs
	
	# assumes NAs randomly distributed
	
	### 
	#browser()
 #  y=ls_sims1[[j]][[1]]

														if(sampling_options$stages == "one")
															{
															x <- data.frame(names(y$"1st_Stage"), df1[variable][match(y$"1st_Stage", df1$indivId),variable])
															#x[,1]<-factor(x[,1], levels=levels(df1[[sampling_options$strata_var]]))
															
															# measured individuals by lenght
															t1<-table(df1[sampling_options$strata_var], useNA="al")
															colnames(x)<-c("strata_id", variable)
															} else {
																	x <- data.frame(names(y$"2nd_Stage"), df1[variable][match(y$"2nd_Stage", df1$indivId),variable])
																	t1<-table(df1[df1$indivId %in% y$"1st_Stage", sampling_options$strata_var], useNA="al")														
																	colnames(x)<-c("strata_id", variable)
															}
  
  
															 # x$age[x$age == -1] <- NA

  
															# adds rasing factor
															  
															 # number of individuals with age by length 
															t2<-table(x$strata_id, useNA="al")
														
															x$total_strata <- t1[match(x$strata_id, names(t1))] 	 # number of individuals with age by length 
															
															if(sampling_options$stages == "one")
																	{
																	x$samp_weight <- x$total_strata/(t2[match(x$strata_id, names(t2))])
																	} else {
																			x$samp_weight <- x$total_strata/(t2[match(x$strata_id, names(t2))])*nrow(df1)/sampling_options$stage1_samp_size
																			}
													
														print(sum(x$samp_weight))
														
														df2 <- df0
														df2$age[df2$age == -1] <- NA
														
														class_span_ <- as.numeric(variable_table$original_class_span[variable_table$variable == "age"])
														breaks_ <- seq(min(df2$age, na.rm=T), max(df2$age, na.rm=T), by=class_span_)
														
														# sum of samp_weights by age
													freq_dist_pop_estimate_no_NAs <- tapply(x$samp_weight, factor(x[,variable], levels=breaks_), sum)
													freq_dist_pop_estimate_no_NAs[is.na(freq_dist_pop_estimate_no_NAs)]<-0
													
													freq_dist_pop_estimate<-c(freq_dist_pop_estimate_no_NAs, sum(is.na(x[,variable])*x[,"samp_weight"]))
													freq_dist_pop_estimate[is.na(freq_dist_pop_estimate)]<-0
													names(freq_dist_pop_estimate)[length(freq_dist_pop_estimate)]<-"NA"
																		
													# adjustments for processing
														#if(is.vector(x)) {x<-data.frame(dummy=1,x); colnames(x)[2]<-variable}
														# if(is.vector(x) | is.factor(x[variable])) {x<-data.frame(dummy=1,x); colnames(x)[2]<-variable}
														# if(is.factor(x[,variable])) x[,variable]<-as.numeric(as.character(x[,variable]))
														# if(is.factor(df1[,variable])) df1[,variable]<-as.numeric(as.character())
													
													# summary	
														n_indiv = nrow(x)
														NAs_x<-sum(is.na(x[,variable]))
														NAs_pop_estimate<-sum(is.na(x[,variable])*x[,"samp_weight"])
														#mean_x1<-sum(x[,variable]*x$samp_weight, na.rm=TRUE)/sum(x[!is.na(x[,variable]),]$samp_weight) # stratified mean
														
															# calculations of each strata [discarding NAs]
															mean_strata <- tapply(x[!is.na(x[,variable]),variable], x[!is.na(x[,variable]),"strata_id"], mean, na.rm=T)
															
															# added 11092018 
															 # mean_strata <- mean_strata[!is.na(mean_strata)]
															sd_strata <- tapply(x[!is.na(x[,variable]),variable], x[!is.na(x[,variable]),"strata_id"], sd, na.rm=T)
															

															n_strata <- tapply(x[!is.na(x[,variable]),variable], x[!is.na(x[,variable]),"strata_id"], length)
															n_strata[is.na(n_strata)]<-0
															
															if(sampling_options$stages == "one")
																{
																N_strata <- tapply(df1[!is.na(df1[,variable]),variable], df1[!is.na(df1[,variable]),sampling_options$strata_var], length)
																N_strata[is.na(N_strata)]<-0
																} else  {
																		aux<-unique(x[c("strata_id","total_strata")])
																		N_strata <- tapply(aux$total_strata, aux$strata_id, sum)*nrow(df1)/sum(aux$total_strata)
																		}
															# corrects when census (note: !is.na(mean_strata) ensure that if one NA exists then variance is not calculated]
																sd_strata[is.na(sd_strata) & !is.na(mean_strata)]<-0
														
																# mean weighted age		
														mean_pop_estimate <- sum(N_strata[N_strata>0]*mean_strata)/sum(N_strata)
														
														
														var_mean_pop_estimate <- sum(((N_strata[N_strata>0]/sum(N_strata))^2)*((N_strata[N_strata>0]-n_strata)/n_strata)*(sd_strata^2)/n_strata)
														stand_err_mean_pop_estimate = sqrt(var_mean_pop_estimate)
														median_pop_estimate <- median(as.numeric(rep(names(freq_dist_pop_estimate_no_NAs),freq_dist_pop_estimate_no_NAs)))
														min_pop_estimate <- min(x[,variable], na.rm=TRUE) # ok 
														max_pop_estimate  <- max(x[,variable], na.rm=TRUE) # ok
														n_class_sampled_pop_estimate <- length(unique(x[!is.na(x[,variable]),variable])) #ok.

													# weight of sample [obtained from weight-length relationship)
														if(variable == "lenCls") estim_weight_sample <- sum(exp(a)*x$lenCls^b) else {estim_weight_sample<-NA}
													
													# analysis of frequency distributions (original)
														
														#modes_pop_estimate <- localMaxima2(as.numeric(freq_dist_pop_estimate_no_NAs))
													
													# mean weighed CV	
														sigma_i<-sqrt(n_class_sampled_pop_estimate*as.matrix(prop.table(freq_dist_pop_estimate_no_NAs)*(1-prop.table(freq_dist_pop_estimate_no_NAs))))
														cv_i <- sigma_i / (n_class_sampled_pop_estimate*as.matrix(prop.table(freq_dist_pop_estimate_no_NAs)))
														MWCV<-round(sum(sigma_i)/nrow(x)*100,1)
														

														data.frame(var=variable, stages = sampling_options$stages, n_stage1 = sampling_options$stage1_samp_size, 
														           strata_var = sampling_options$strata_var, n = n_indiv, 
														          
														           NAs_x = NAs_x, mean=mean_pop_estimate, se = stand_err_mean_pop_estimate, cv=round(stand_err_mean_pop_estimate/mean_pop_estimate*100,1),
														           min = min_pop_estimate, median = median_pop_estimate, max = max_pop_estimate
														           )	
													
															
}														

make_summary_categorical_stratified <- function(y, variable){ #browser()
# Nuno Prista 2017
	#browser()
	# 20180526 - adapted to indivId as input
	# 20180526 - naming of objects improved (less portuguese now)
	# 20180526 - n_class_sampled_x now excludes NAs 

											
												#browser()
													if(sampling_options$stages == "one")
															{
															x <- data.frame(names(y$"1st_Stage"), df1[variable][match(y$"1st_Stage", df1$indivId),variable])
															t1<-table(df1[sampling_options$strata_var], useNA="al")
															colnames(x)<-c("strata_id", variable)
															} else {
																	x <- data.frame(names(y$"2nd_Stage"), df1[variable][match(y$"2nd_Stage", df1$indivId),variable])
																	t1<-table(df1[df1$indivId %in% y$"1st_Stage", sampling_options$strata_var], useNA="al")														
																	colnames(x)<-c("strata_id", variable)
																	}
															
															# adds rasing factor
															#browser()
															t2<-table(x$strata_id, useNA="al")
															x$total_strata <- t1[match(x$strata_id, names(t1))] 
															if(sampling_options$stages == "one")
																	{
																	x$samp_weight <- x$total_strata/(t2[match(x$strata_id, names(t2))])
																	} else {
																			x$samp_weight <- x$total_strata/(t2[match(x$strata_id, names(t2))])*nrow(df1)/sampling_options$stage1_samp_size
																			}
													print(sum(x$samp_weight))							
													freq_dist_pop_estimate_no_NAs<-tapply(x$samp_weight, factor(x[,variable], levels=ls_auto_modes_sample[[variable]]$original_breaks), sum)
													freq_dist_pop_estimate_no_NAs[is.na(freq_dist_pop_estimate_no_NAs)]<-0
													freq_dist_pop_estimate<-c(freq_dist_pop_estimate_no_NAs, sum(is.na(x[,variable])*x[,"samp_weight"]))
													
													freq_dist_pop_estimate[is.na(freq_dist_pop_estimate)]<-0
													names(freq_dist_pop_estimate)[length(freq_dist_pop_estimate)]<-"NA"

													
													# adjustments for processing
														#x<-data.frame(dummy=1,x); colnames(x)[2]<-variable
														#x[,variable]<-factor(x[,variable], levels=sort(unique(df1[,variable])))
														#df1[,variable]<-factor(df1[,variable], levels=sort(unique(df1[,variable])))
													# summary	
														n_indiv = nrow(x)
														NAs_x<-sum(is.na(x[,variable]))
														NAs_pop_estimate<-sum(is.na(x[,variable])*x[,"samp_weight"])
														n_class_sampled_pop_estimate <- length(unique(x[!is.na(x[,variable]),variable])) #ok.
														sampled_classes_x <- paste(sort(unique(x[!is.na(x[,variable]),variable])), collapse=",")
													
												
													# analysis of frequency distributions (original) and modes
														freq_dist_sampled_indivs <- table( factor(x[,variable],levels=ls_auto_modes_sample[[variable]]$original_breaks),useNA="al")
														#freq_dist_sampled_indivs<-freq_dist_sampled_indivs[freq_dist_sampled_indivs!=0]
														if(!all.equal(names(freq_dist_sampled_indivs), ls_auto_modes_sample[[variable]]$original_breaks)){stop ("levels differen cannot compute modes")}
														if(length(unique(freq_dist_pop_estimate_no_NAs))>1)
															{
															modes_sampled_individuals <- localMaxima2(as.numeric(freq_dist_pop_estimate_no_NAs))
															modes_sampled_individuals_after_threshold<-modes_sampled_individuals[modes_sampled_individuals %in% which(freq_dist_pop_estimate_no_NAs> (ls_auto_modes_sample[[variable]]$min_proportion_to_accept_mode * sum(freq_dist_pop_estimate_no_NAs)))]	
															n_modes<-length(modes_sampled_individuals_after_threshold)
															modes_correct<-identical(modes_sampled_individuals_after_threshold,ls_auto_modes_sample[[variable]][["original_modes"]])
															n_modes_correct<-sum(modes_sampled_individuals_after_threshold %in% ls_auto_modes_sample[[variable]][["original_modes"]])
															} else {
																	n_modes = 0
																	modes_correct <- NA
																	n_modes_correct <- NA
																	}						
													# chisq test
														#browser()
														#expected_probs<-prop.table(table(df1[,variable]))
														#expected_probs <- expected_probs[expected_probs!=0]
														#freq_dist_pop_estimate_no_NAs<-freq_dist_pop_estimate_no_NAs[names(freq_dist_pop_estimate_no_NAs) %in% names(expected_probs)]
														# implements simulated p.value for matStage where approximations seem worse
														if(variable != "matStage") teste_chisq <- chisq.test(x=as.vector(freq_dist_pop_estimate_no_NAs),p = expected_probs)$p.value else teste_chisq <- chisq.test(x=as.vector(freq_dist_pop_estimate_no_NAs),p = expected_probs,simulate.p.value=T, B=1000)$p.value
														chisqtest_result<- teste_chisq >= 0.05

														# L1, L2, L3, L2a
														
														#realized_probs <- prop.table(freq_dist_pop_estimate)
														#expected_probs <- prop.table(ls_auto_modes_sample[[variable]][["original_freq"]])
														#L1 <- sum(abs(expected_probs-realized_probs))
														#L2 <- sqrt(sum((expected_probs-realized_probs)^2))
														#L2a <- sum((expected_probs-realized_probs)^2)
														#L3 <- max(expected_probs-realized_probs)

														# output
														data.frame(var=variable, stages = sampling_options$stages, n_stage1 = sampling_options$stage1_samp_size, strata_var = sampling_options$strata_var, n = n_indiv, estim_weight=NA, NAs_x = NAs_x, n_class_sampled=n_class_sampled_pop_estimate, sampled_classes = sampled_classes_x,
																	n_modes = n_modes, modes_correct = modes_correct, n_modes_correct = n_modes_correct, 
																	chisq_test = teste_chisq, chisq_test_logic = chisqtest_result,
																	L1 = L1, L2 = L2, L3 = L3, L2a = L2a)																		
														}		

make_models_random<-function(y, model) {				
					
					out_list<-list("weight-length"=NULL, "sex-ratio"=NULL, "L50"=NULL)
						
					# data prep	

					x <- df1[match(y, df1$indivId),c("lenCls","indWt","sex","mature")]
					x$samp_weight<-1
														
								
					# models: weight-length 
						if (model=="weight-length")
							{
							if(nrow(na.omit(x[c("indWt","lenCls")]))>2)
									{
									model_weight_length<-lm(log(x$indWt)~log(x$lenCls+variable_table[variable_table$variable=="lenCls","original_class_span"]/2), weights= x$samp_weight); 
									out<-data.frame(n = nrow(na.omit(x[c("indWt","lenCls")])), log_a=coef(model_weight_length)[1], b=coef(model_weight_length)[2], r.squared=summary(model_weight_length)$r.squared)
									} else {
											out<-data.frame(n = nrow(na.omit(x[c("indWt","lenCls")])), log_a=NA, b=NA, r.squared=NA)
											}
							}				

					# models: sex-ratio						
						if (model=="sex-ratio")
							{		
							if(sum(!is.na(x$sex))>5)
												{
												a<-table(x$sex, useNA="al")
												out<-data.frame(n_females = a["F"], n_males=a["M"], n_indeterm = sum(a)-a["F"]-a["M"], sex_ratio=a["F"]/a["M"]); 
												rownames(out)<-NULL
												} else {
														out<-data.frame(n_females = sum(x$sex=="F", na.rm=T), n_males=sum(x$sex=="M", na.rm=T), n_indeterm = sum(x$sex=="I", na.rm=T), sex_ratio=NA)
														}
							}
					
					# models: L50 (maturation)	
						if (model=="L50")
							{
							if(nrow(na.omit(x[c("lenCls","mature")]))>10 & length(unique(x$lenCls))>5 & sum(c("0","1") %in% unique(x$mature))==2)
												{
												#browser()
												#x$mature<-as.numeric(as.character(x$mature))
												model_maturation<-glm(mature~lenCls,data=x[c("mature","lenCls")],family=binomial,weights= x$samp_weight); 
												out<-data.frame(n = nrow(na.omit(x[c("lenCls","mature")])), n_immature = sum(x$mature==0, na.rm=T), n_mature = sum(x$mature==1, na.rm=T), L50 = lrPerc(coef(model_maturation),0.5))
												} else {
														out<-data.frame(n = nrow(na.omit(x[c("mature","lenCls")])), n_immature = sum(x$mature==0, na.rm=T), n_mature = sum(x$mature==1, na.rm=T), L50=NA)
														}
							}					
						out				
						}															

make_models_stratified<-function(y, model){				
					
				
					# data prep	
						if(sampling_options$stages == "one")
						{
						x <- data.frame(names(y$"1st_Stage"), df1[match(y$"1st_Stage", df1$indivId),c("lenCls","indWt","sex","mature")])
						t1<-table(df1[sampling_options$strata_var])
						colnames(x)<-c("strata_id", c("lenCls","indWt","sex","mature"))
						} else {
								x <- data.frame(names(y$"2nd_Stage"),  df1[match(y$"2nd_Stage", df1$indivId),c("lenCls","indWt","sex","mature")])
								t1<-table(df1[df1$indivId %in% y$"2nd_Stage", sampling_options$strata_var])														
								colnames(x)<-c("strata_id", c("lenCls","indWt","sex","mature"))
								}
						# adds rasing factor
						t2<-table(x$strata_id)
						x$total_strata <- t1[match(x$strata_id, names(t1))] 
						if(sampling_options$stages == "one")
								{
								x$samp_weight <- x$total_strata/(t2[match(x$strata_id, names(t2))])
								} else {
										x$samp_weight <- x$total_strata/(t2[match(x$strata_id, names(t2))])*nrow(df1)/sampling_options$stage1_samp_size
										}		
								
					# models: weight-length 
						if (model=="weight-length")
							{
							if(nrow(na.omit(x[c("indWt","lenCls")]))>2)
									{
									model_weight_length<-lm(log(x$indWt)~log(x$lenCls+variable_table[variable_table$variable=="lenCls","original_class_span"]/2), weights= x$samp_weight); 
									out<-data.frame(n = nrow(na.omit(x[c("indWt","lenCls")])), log_a=coef(model_weight_length)[1], b=coef(model_weight_length)[2], r.squared=summary(model_weight_length)$r.squared)
									} else {
											out<-data.frame(n = nrow(na.omit(x[c("indWt","lenCls")])), log_a=NA, b=NA, r.squared=NA)
											}
							}				

					# models: sex-ratio						
						if (model=="sex-ratio")
							{					
								if(sum(!is.na(x$sex))>5)
												{
												a<-table(x$sex, useNA="al")
												out<-data.frame(n_females = a["F"], n_males=a["M"], n_indeterm = sum(a)-a["F"]-a["M"], sex_ratio=a["F"]/a["M"]); 
												rownames(out)<-NULL
												out
												} else {
														data.frame(n_females = sum(x$sex=="F", na.rm=T), n_males=sum(x$sex=="M", na.rm=T), n_indeterm = sum(x$sex=="I", na.rm=T), sex_ratio=NA)
														}
							}
					
					# models: L50 (maturation)	
						if (model=="L50")
							{
							if(nrow(na.omit(x[c("lenCls","mature")]))>10 & length(unique(x$lenCls))>5 & sum(c("0","1") %in% unique(x$mature))==2)
												{
												#browser()
												x$mature<-as.numeric(as.character(x$mature))
												model_maturation<-glm(mature~lenCls,data=x[c("mature","lenCls")],family=binomial,weights= x$samp_weight); 
												out<-data.frame(n = nrow(na.omit(x[c("lenCls","mature")])), n_immature = sum(x$mature==0, na.rm=T), n_mature = sum(x$mature==1, na.rm=T), L50 = lrPerc(coef(model_maturation),0.5))
												} else {
														out<-data.frame(n = nrow(na.omit(x[c("mature","lenCls")])), n_immature = sum(x$mature==0, na.rm=T), n_mature = sum(x$mature==1, na.rm=T), L50=NA)
														}
							}					
						out				
						}	







# ***************************************************************


# ***************************************************************

# ***************************************************************
# ***************************************************************

make_summary_proportion_by_length<-function(y, variable){ #browser()
 
  if (FALSE) {
    x = ls_sims1[[j]][[1]]
    y = x[[w]]
    variable = "age"
  }
  
   # Nuno Prista 2017
  # wishlist - add DI and other indexes from Chih, 2010
  
  # 20180526 - adapted to indivId as input
  # 20180526 - naming of objects improved (less portuguese now)
  # 20180526 - moved to arguments the parameters of the weight - length relationship
  # 20180526 - n_class_sampled_x now excludes NAs 
  # - CHECK - 20180526 - added original number of modes 
  #browser()
  
   df1[c("lenCls", variable,"sampId")][match(y, df1$indivId),c("lenCls", variable,"sampId")]
  
  
}														




# Isabella Bitetto (COISPA)
#INPUT:
# dataframe con lunghezze sulle righe ed età sulle colonne; ultima colonna è il totale dei misurati per ogni classe di lunghezza
# DF = read.table("DF_example.csv",sep=";",header=T)

CV_ALK=function (DF) {
  
  # DF= alk_this
  DF <- DF[, colnames(DF) != "total_ages"]
  
  DFrame=data.frame(ages=colnames(DF)[-c(1,ncol(DF))],CV=rep(999,(ncol(DF)-2)), Nipi=rep(999,(ncol(DF)-2)), VarNipi=rep(999,(ncol(DF)-2)))
  ages=colnames(DF)[-c(1,ncol(DF))]
  for (ag in 1:length(ages)) {
    age_temp=DF[,c(1,(1+ag))]
    age_temp[which(is.na(age_temp[,2])),2] <- 0
    age_temp$Ni=DF$total_lengths
    if (ncol(DF)>3){
      age_temp$ni=rowSums(DF[,-c(1,ncol(DF))],na.rm=T)
    } else {
      age_temp$ni= DF[,2]
    }
    age_temp$nipi=age_temp[,2]
    age_temp$pi=age_temp$nipi/age_temp$ni
    age_temp$Nipi=age_temp$Ni*age_temp$pi
    age_temp$Varpi = (age_temp$pi*(1-age_temp$pi)) / age_temp$ni
    age_temp$VarNipi=(age_temp$Ni^2) * age_temp$Varpi
    DFrame$Nipi[ag]=sum(age_temp$Nipi) 
    DFrame$VarNipi[ag]=sum(age_temp$VarNipi)
    DFrame$CV[ag]=sum(age_temp$VarNipi)^0.5/sum(age_temp$Nipi) *100
    
  }
  
  DFrame$total_CV = ifelse(sum( DFrame$Nipi) == 0, 0, (sum( DFrame$VarNipi)^0.5)/sum( DFrame$Nipi) ) * 100
  
  return(DFrame)
}




														