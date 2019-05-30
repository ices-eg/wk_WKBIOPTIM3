make_models_random<-function(y, model, repl = 1){				
					
					out_list<-list("weight-length"=NULL, "sex-ratio"=NULL, "L50"=NULL, "VBGF"=NULL)
						
					# data prep	

					x <- df1[match(y, df1$indivId),c("lenCls","indWt","age","sex","mature")]
					x$samp_weight<-1
														
								
					# models: weight-length 
						if (model=="weight-length")
							{
							if(nrow(na.omit(x[c("indWt","lenCls")]))>2)
									{
									model_weight_length<-lm(log(x$indWt)~log(x$lenCls+variable_table[variable_table$variable=="lenCls","original_class_span"]/2), weights= x$samp_weight); 
									out<-data.frame(repl = repl, n = nrow(na.omit(x[c("indWt","lenCls")])), log_a=coef(model_weight_length)[1], b=coef(model_weight_length)[2], r.squared=summary(model_weight_length)$r.squared)
									} else {
											out<-data.frame(repl = repl, n = nrow(na.omit(x[c("indWt","lenCls")])), log_a=NA, b=NA, r.squared=NA)
											}
							}				

					# models: sex-ratio						
						if (model=="sex-ratio")
							{		
							if(sum(!is.na(x$sex))>5)
												{
												a<-table(x$sex, useNA="al")
												out<-data.frame(repl = repl, n_females = a["F"], n_males=a["M"], n_indeterm = sum(a)-a["F"]-a["M"], sex_ratio=a["F"]/a["M"]); 
												rownames(out)<-NULL
												} else {
														out<-data.frame(repl = repl, n_females = sum(x$sex=="F", na.rm=T), n_males=sum(x$sex=="M", na.rm=T), n_indeterm = sum(x$sex=="I", na.rm=T), sex_ratio=NA)
														}
							}
					
					# models: L50 (maturation)	
						if (model=="L50")
							{
							if(nrow(na.omit(x[c("lenCls","mature")]))>5 & length(unique(x$lenCls))>5 & sum(c("0","1") %in% unique(x$mature))==2)
												{
												#browser()
												#x$mature<-as.numeric(as.character(x$mature))
												model_maturation<-glm(mature~lenCls,data=x[c("mature","lenCls")],family=binomial,weights= x$samp_weight); 
												out<-data.frame(repl = repl, n = nrow(na.omit(x[c("lenCls","mature")])), n_immature = sum(x$mature==0, na.rm=T), n_mature = sum(x$mature==1, na.rm=T), L50 = lrPerc(coef(model_maturation),0.5))
												} else {
														out<-data.frame(repl = repl, n = nrow(na.omit(x[c("mature","lenCls")])), n_immature = sum(x$mature==0, na.rm=T), n_mature = sum(x$mature==1, na.rm=T), L50=NA)
														}
							}
					## models: von Bertalanffy growth model
						if(model=="VBGF")
							{
							  # df_a<-df0[complete.cases(df0$age),]
							  #if(nrow(na.omit(x[c("age","lenCls")]))>10 & length(unique(x$age))>2)
							  if(nrow(na.omit(x[c("age","lenCls")]))>10)
							  {
							  #browser()
								#print((unique(x$age)))
								svTypical <- list(Linf = start_values_vbgf[["Linf_teo"]],K = start_values_vbgf[["K_teo"]],t0 = start_values_vbgf[["t0_teo"]]) ##Initial parameters to run the VBGM
								vbTypical <- lenCls~Linf*(1-exp(-K*(age-t0))) ##von Bertallanfy growth model
								control<- nls.control(maxiter=100)
						
								fitTypical<-NULL
								try(fitTypical<-nls(vbTypical,data=x,start=svTypical,control), silent=T); # does not stop in the case of error
								if(!is.null(fitTypical))
									{
									fitTypical <- nls(vbTypical,data=x,start=svTypical,control) ##data(subset only length and age)
									Linf<- summary(fitTypical)$coefficients[[1]]
									K<- summary(fitTypical)$coefficients[[2]]
									t0<- summary(fitTypical)$coefficients[[3]]
									out<- data.frame(repl = repl, Linf, K, t0)
									} else {out<- data.frame(repl = repl, Linf=NA, K=NA, t0=NA); 
										#print("Did not converge")
										}
								#print(paste("VBGM parameters estimated: ",out))
								
							  } else {
								out<- data.frame(repl = repl, Linf=NA, K=NA, t0=NA)  ##message of not enough age groups
								test <- nrow(na.omit(x[c("age","lenCls")]))
								#print((unique(x$age)))
								#print(paste("Not enough data to obtain the VBGF parameters: only",test,"indiv (min = 10 indiv)"))
								   }
							}
							
						out				
						}															
