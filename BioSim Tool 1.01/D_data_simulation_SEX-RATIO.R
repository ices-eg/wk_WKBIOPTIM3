# BioSim Tool 1.01 developed within STREAM (STrengthening REgional cooperation in the Area of 
#fisheries biological data collection in the Mediterranean and Black Sea (STREAM)) project

# SD Tool v.3 and WKBIOPTIM SimTool v.1  were taken into account for the developement of the present tool.
# SD tool has been developed in MARE/2014/19 project and updated in MARE/2016/22 project (STREAM)

# Authors of the first version of SD Tool: F. Gontrand, T. Rouyer, N. Billet, 2016
# IFREMER, UMR MARBEC, Avenue Jean Monnet, BP 171, 34203 S` ete, France 

# Authors of the first version of WKBIOPTIM scripts: Nuno Prista (SLU, Sweden) from a subgroup work carried out during a local Workshop on Sampling Design and Optimization (Lysekil, 31 October to 4 November 2016, unpublished)
# SLU Aqua, Institute of Marine Research, Department of Acquatic Resources - Swedish University of Agricultural Sciences

# Authors of this new tool based on SD tool and WKBIOPTIM SimTool: M.T. Facchini, I. Bitetto, 2017
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# The tool is documented in Deliverable D3.3 "Upgrade the methodological framework and tools for sampling optimization, implement and report case studies" (January 2019)

# In case of use of the tool, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail addresses: facchini@coispa.it, bitetto@coispa.it
# BioSimTool is believed to be reliable. However, we disclaim any implied warranty or representation 
# about its accuracy, completeness or appropriateness for any particular purpose.


rm(list=ls())	

# read packages
library(rlist) # list.merge
library(dplyr)
library(parallel)	
library(ggplot2)
library(emdist)

# set the working directory
 myWD <- "C://BioSim Tool 1.01"

setwd(myWD)

caseStudy_path <-"C://BioSim Tool 1.01"

species_name <- "Parapenaeus longirostris"  # "Aristeus antennatus"
short_name <-  "DPS" # "ARA"

GSA <- "GSA99" # "ALL"

# set sampling design of sample data
sampling_design <- list (stratified = FALSE, strata_var = "")	

# expliciting the number of iterations and the sample sizes
n_sims <- 20
samp_sizes<-c(seq(100,1000, by=100))

# stratification of sampleId definition

by_cat="N"  # "Y" or "N"
by_EUlev6="N"  # "Y" or "N"
by_EUlev4="N"
by_quart="N"  # "Y" or "N"

#  Sampling of different number of individuals without replacement 
#???(sample size dependent of size classes in the sample)

sampling_options <- list (n_sims = n_sims, 
                          stages="one", 				# no of stages
                          stratified = FALSE, strata_var ="", 	# stratification details
                          stage1_samp_size=NA, samp_sizes = c(samp_sizes), 	# samp sizes
                          replacement=FALSE, 	sample_all_available = TRUE, 
                          sample_all_available_warning = TRUE, 	# replacement options
                          vars_to_keep = c(""))		

# load input data
load(paste(caseStudy_path, "\\A_data_preparation\\input_data.rdata", sep=""))


# read functions
source("funs-26102018.R") 

str_cat <- ifelse(by_cat == "Y", "catY", "catN")
str_gear <- ifelse(by_EUlev6 == "Y", "lev6", ifelse(by_EUlev4 == "Y", "lev4", "gearN"))
str_quart <- ifelse(by_quart == "Y", "quartY", "quartN")

if (	GSA != "ALL") {
  res_dir <- paste(caseStudy_path, "\\D_sexratio_", short_name,"_", GSA, "_",str_cat, "_",str_gear, "_",str_quart, sep="")
} else {
  res_dir <- paste(caseStudy_path, "\\D_sexratio_", short_name,"_",str_cat, "_",str_gear, "_",str_quart, sep="")
}
#sim_dir <- paste(res_dir, "\\simulation_by_trip", sep="")

dir.create(res_dir)
#dir.create(sim_dir)


	df0 <- df0[df0$spp == species_name, ]
	
	if (GSA != "ALL") {
	  df0 <- df0[df0$area == GSA, ]
	}
	

# read variable table
	
	# Isabella: to be commented??
#	variable_table <- read.csv2("000_Inputs\\variable_table_biotic.csv", as.is=TRUE)

	df0=df0[!is.na(df0$sex),]
	df0=df0[as.character(df0$sex) =="M"| as.character(df0$sex) =="F",]
	
	df0_temp_ <- df0
	
	if (by_cat=="N"){
	  df0_temp_$commCat="-1"
	}
	
if (by_EUlev6=="N" & by_EUlev4=="N") {
	  df0_temp_$Metier_Gear="-1"
	} else if (by_EUlev6=="Y") {
	  df0_temp_$Metier_Gear=df0_temp_$foCatEu6_sc
	} else if (by_EUlev4=="Y") {
	  df0_temp_$Metier_Gear=df0_temp_$foCatEu4
	}
	
	if (by_quart=="N"){
	  df0_temp_$quarter="-1"
	}
	
	df0_by_year <-  group_by(df0_temp_ , Metier_Gear,quarter,commCat, year )  
	df0_by_year <- data.frame(summarise(df0_by_year,  sex_measures=length( mature)))
	
	df0_avg <-  group_by(df0_by_year , Metier_Gear,quarter,commCat )  
	df0_avg <- data.frame(summarise(df0_avg,  sex_meas=round(mean( sex_measures), 0)))
	
	df0_avg$year <- paste(min(df0_by_year$year), max(df0_by_year$year), sep="-")
	df0_avg <- df0_avg[ , c(1:3, 5, 4)]
	colnames(df0_avg)[5] <- 	colnames(df0_by_year)[5]
	past_situation <- data.frame(rbind(df0_by_year, df0_avg))
	write.table(past_situation,paste(res_dir,"/Past situation.csv", sep=""),sep=";",row.names=F)
	
	
	df0$sex=as.character(df0$sex)
	
	# Definition of a dycotomic variable 0/1 (1 the individual is female, 0 is male)
	df0$fem=ifelse(df0$sex=="F",1,0)
	
  #check compatibility
  if (by_EUlev6=="Y" & by_EUlev4=="Y")   {
  print("Incompatibility of by_EUlev6==Y & by_EUlev4==Y")
  }
   
	
	# Isabella 28082018: summary of sample size by trip Id (to be eventually adapted using sampId_Cat) 


	if (by_cat=="Y") {
	df0$sampId=df0$sampId_cat
	df0$indivId=df0$indivId_cat
	} 

df0=df0[df0$sex!="",]	

tab=aggregate(df0$sex,by=list(df0$sampId),FUN="length")	

colnames(tab) =c("sampId","Num_ind_with_sex") 
summary(tab[,2])

write.table(tab, paste(res_dir,"/Numbers with sex by sampId.csv", sep=""),sep=";",row.names=F)

quants <- summary(tab[,2])
QUANTS <- data.frame(Min = as.numeric(quants[1]), qu_1st= as.numeric(quants[2]),  Median = as.numeric(quants[3]),  
                     Mean = as.numeric(quants[4]), qu_3rd = as.numeric(quants[5]), Max = as.numeric(quants[6]) )
write.table(QUANTS, paste(res_dir,"/Numbers with sex by sampId_quantiles.csv", sep=""),sep=";",row.names=F)



# setting of the minimum number of individuals considered representative
min_n <- 1000

		table_select_samples<-table(df0$sampId)[table(df0$sampId)>=min_n]; 
		samples_to_analyze<-names(table_select_samples)		


# ======================
# Weight - Length Relationship		
# ======================

	#coefs_weight_length<-coef(lm(log(df0$indWt)~log(df0$lenCls)))
	#names(coefs_weight_length)<-c("a","b")
	 coefs_weight_length<- c(a= NA, b=NA)
	
# =======================
# Simulations	
# =======================

		# if your data is a simple random sample of individuals from which all biological variables were sampled you will be able to test all sampling strategies
		# if your data was stratified with regards to one of the variables you have to take that into account
			# you can mantain the stratification and look into the consequences of a reduction or increase in the number of individuals samples per strata
			# you can attemp to de-stratify, creating a pseudo random sample and then test scenarios in it
														# check			

	# creates a storage object
	ls_DT_compiled<-sapply(samples_to_analyze, function(x) NULL)

	seed<-1
	set.seed(seed)
	ptc1<-Sys.time()	
  # 
  

	for (sampId in samples_to_analyze)  {
  # sampId = samples_to_analyze[1] 
  #"2001_999"


	print("==========================")
	print(sampId)      
	print("==========================")
	
	# selects sample
		df1<-df0[df0$sampId == sampId,]
		# use this instead if you want to look at a specific sample
			#df1<-df0[df0$sampId == "2001_999",]

# ===============			
# Simulations of samples
# ===============
								
				ls_sims1<-faz_sim_sample(sampDes = sampling_design, sampOpt = sampling_options, df1o = df1)	
				
# 				$ 50 :List of 10
# 				..$ 1 :List of 2
# 				.. ..$ 1st_Stage: chr [1:50] "01_18_2015_999_1_148" "01_18_2015_999_1_207" "01_18_2015_999_1_317" "01_18_2015_999_1_502" ...
# 				.. ..$ 2nd_Stage: chr "Not Applicable"
		
# ====================
# Building of sample statistics
# ====================


	# creates storage object
	ls_sims_stats<-lapply(sapply(as.character(sampling_options$samp_sizes), function(x) NULL), function(x) sapply("females_proportion", function(x) NULL)) 

 	vars_numerical<- c()     #variable_table$variable[variable_table$type=="numerical"]
	vars_categorical<- c("sex")  #variable_table$variable[variable_table$type=="categorical"]
	
	detectCores(all.tests = FALSE, logical = TRUE)
	
	# set the no of cores 
	# detectCores()
	cl <- makeCluster(5)
	#clusterExport(cl, varlist = c("make_summary_numeric","variable","df1","ls_auto_modes","coefs_weight_length","localMaxima2","localMaxima","vars_numerical","vars_categorical"))		
	clusterExport(cl, varlist = c("make_summary_numeric","df1","coefs_weight_length","localMaxima2","localMaxima","vars_numerical","vars_categorical"))		
	
	#ls_auto_modes_sample<-ls_auto_modes[[sampId]]

	ls_DT_compiled_ss <-vector(mode="list", length=length(sampling_options$samp_sizes))
	
	for (j in 1:length(sampling_options$samp_sizes))	{
	  
	  variable = "fem"
	  
			if(!sampling_options$stratified & (sampling_options$stages =="one" | sampling_options$stages =="two")) # not stratified, one or two stages
				{
				print(paste("Processing sample size", sampling_options$samp_sizes[j]))	
				if (sampling_options$stages == "one") w <- "1st_Stage" else w <- "2nd_Stage"
				
				

				   #ls_sims_stats[[j]][[variable]]
				
				res_ <- do.call("rbind",lapply(ls_sims1[[j]], function(x){ make_summary_proportion_by_length(x[[w]], variable)}))
				res_$n_it <- do.call("rbind",strsplit(as.character(rownames(res_)), "[.]"))[,1]
				res_$samp_size <- sampling_options$samp_sizes[j] 
				# ==================
				# models: 
				# ==================			
				} # if stratification option ****************************
	  
		ls_DT_compiled_ss[[j]] <- res_	
	  }
			
			#browser()	
	
				ls_DT_compiled[[sampId]]<-ls_DT_compiled_ss
				
         stopCluster(cl)
 }
	
	
	
	tab=aggregate(df0$sex,by=list(df0$sampId),FUN="length")	
	
	colnames(tab) =c("sampId","Num_ind_with_sex") 
	
	if (by_cat=="Y"){
	  df0_ok=unique(df0[,c("sampId","commCat")])
	  tab=merge(tab,df0_ok,by=c("sampId")) 
	}
	
	if (by_EUlev6=="Y" | by_EUlev4=="Y" | by_quart=="Y")
	{
	  df0_ok=unique(df0[,c("sampId","foCatEu6_sc","foCatEu4","quarter")])
	  tab=merge(tab,df0_ok,by=c("sampId"))  
	  
	}
	
	
	if (by_cat=="N"){
	  tab$commCat="-1"
	}
	
	if (by_EUlev6=="N" & by_EUlev4=="N") {
	  tab$Metier_Gear="-1"
	} else if (by_EUlev6=="Y") {
	  tab$Metier_Gear=tab$foCatEu6_sc
	} else if (by_EUlev4=="Y") {
	  tab$Metier_Gear=tab$foCatEu4
	}
	
	if (by_quart=="N"){
	  tab$quarter="-1"
	}
	
	tab <- tab[, !(colnames(tab) %in% c("foCatEu6_sc", "foCatEu4")) ]
	
	
	tab$is_BASELINE = FALSE 
	tab$is_BASELINE[tab$sampId %in% samples_to_analyze ] = TRUE 
	
	BASELINE_VALUE_tab <- group_by(tab, commCat, Metier_Gear,quarter)
	BASELINE_VALUE_tab <- data.frame(summarise(BASELINE_VALUE_tab, BASELINE_VALUE= mean(Num_ind_with_sex) ))
	BASELINE_VALUE_tab$BASELINE_VALUE <- round(BASELINE_VALUE_tab$BASELINE_VALUE, 0)
	
	BASELINE_VALUE_tab$BASELINE_VALUE[which(	BASELINE_VALUE_tab$BASELINE_VALUE %in% samp_sizes)] <- BASELINE_VALUE_tab$BASELINE_VALUE[which(	BASELINE_VALUE_tab$BASELINE_VALUE %in% samp_sizes)] +1
	
	BASELINE_VALUE_tab$BASELINE_NAME <-  paste("Baseline [", BASELINE_VALUE_tab$BASELINE_VALUE , "]", sep="")	


	for (sampId in samples_to_analyze)  {
	  
	  if (sampId == samples_to_analyze[1]) {
	    total_fem_res <-	do.call("rbind", ls_DT_compiled[[sampId]])
   
	  } else {
	    total_fem_res <- rbind(total_fem_res, do.call("rbind", ls_DT_compiled[[sampId]]))
	    
	  }
	  
	}
 


if (by_cat=="Y"){
  df0_ok=unique(df0[,c("sampId","commCat")])
  total_fem_res=merge(total_fem_res,df0_ok,by=c("sampId")) 

}

if (by_EUlev6=="Y" | by_EUlev4=="Y" | by_quart=="Y")
{
  df0_ok=unique(df0[,c("sampId","foCatEu6_sc","foCatEu4","quarter")])
  total_fem_res=merge(total_fem_res,df0_ok,by=c("sampId"))  

}
	total_fem_res$fem <- as.numeric(as.character(total_fem_res$fem))
	
	
	if (by_cat=="N"){
	  total_fem_res$commCat="-1"
	}
	
	if (by_EUlev6=="N" & by_EUlev4=="N") {
	  total_fem_res$Metier_Gear="-1"
	} else if (by_EUlev6=="Y") {
	  total_fem_res$Metier_Gear=total_fem_res$foCatEu6_sc
	} else if (by_EUlev4=="Y") {
	  total_fem_res$Metier_Gear=total_fem_res$foCatEu4
	}
	
	if (by_quart=="N"){
	  total_fem_res$quarter="-1"
	}
	
	total_fem_res <- total_fem_res[, !(colnames(total_fem_res) %in% c("foCatEu6_sc", "foCatEu4")) ]

	total_fem_res_prop <- group_by(total_fem_res , Metier_Gear, quarter, samp_size,	n_it, lenCls,commCat)  

 total_fem_res_prop$fem <- as.numeric(as.character(total_fem_res_prop$fem))

total_fem_res_prop_ <- data.frame(summarise(total_fem_res_prop,  females=sum(fem), 
                                                  total= length(fem) ))
write.table(total_fem_res_prop_,paste(res_dir,"/Summary_table_by_length_sex.csv",sep=""),sep=";",row.names=F)

total_fem_res_prop_$males <- with(total_fem_res_prop_, total-females)

total_fem_res_prop_$pi =   with(total_fem_res_prop_, females/total) # fem
total_fem_res_prop_$qi =  with(total_fem_res_prop_, males/total) # mal
  
total_fem_res_prop_$prod_pi_qi =  with(total_fem_res_prop_, pi*qi) # mal


total_fem_res_prop_ = total_fem_res_prop_[!is.na(total_fem_res_prop_$n_it),]

total_fem_res_prop2 <- group_by(total_fem_res_prop_ , Metier_Gear, quarter, samp_size,	n_it, commCat)  
  
total_fem_res_prop3 <- data.frame(summarise(total_fem_res_prop2,  CV=sqrt(sum(prod_pi_qi)/(sum(total)-1))/ (sum(females)/sum(total))*100     ))

  write.table(total_fem_res_prop3,paste(res_dir,"/Extended_output_CV_sex.csv",sep=""),sep=";", row.names=F)
  
  
  dataset<-total_fem_res_prop3
  
  # Saving plots

    categ=unique(dataset$commCat)
    met=as.character(unique(dataset$Metier_Gear))
    quart=as.character(unique(dataset$quarter))
   
# Creation of a similar dataset for the sample data
    
samp= df0[,c("sampId","foCatEu6_sc","foCatEu4","quarter","commCat","lenCls","fem")]
samp$fem=as.numeric(as.character(samp$fem))



 if (by_cat=="N"){
   samp$commCat="-1"
 }

if (by_EUlev6=="N" & by_EUlev4=="N") {
  samp$Metier_Gear="-1"
} else if (by_EUlev6=="Y") {
  samp$Metier_Gear=samp$foCatEu6_sc
} else if (by_EUlev4=="Y") {
  samp$Metier_Gear=samp$foCatEu4
}

if (by_quart=="N"){
  samp$quarter="-1"
}

samp <- samp[, !(colnames(samp) %in% c("foCatEu6_sc", "foCatEu4")) ]

samp_ <- group_by(samp , Metier_Gear, quarter,	 lenCls,commCat)  



samp2 <- data.frame(summarise(samp_,  females=sum(fem), total= length(fem) ))

samp2$males <- with(samp2, total-females)

samp2$pi =   with(samp2, females/total) # mat
samp2$qi =  with(samp2, males/total) #imma

samp2$prod_pi_qi =  with(samp2, pi*qi) #imma

samp3 <- group_by(samp2 , Metier_Gear, quarter, commCat) 


samp4 <- data.frame(summarise(samp3,  CV=sqrt(sum(prod_pi_qi)/(sum(total)-1))/ (sum(females)/sum(total))*100))
# colnames(samp4)[which(colnames(samp4)=="foCatEu6" | colnames(samp4)=="foCatEu4")]="Metier_Gear"
samp3=data.frame(samp3)


samp3$n_it=-1

ss=samp3
ss$samp_size=samp_sizes[1]



for (ssz in 2:length(samp_sizes)){
  ss_t=samp3
  ss_t$samp_size=samp_sizes[ssz]
  ss_t=rbind(ss,ss_t)  
  ss=ss_t
}

samp3=data.frame(ss_t)


dataset2<-data.frame(total_fem_res_prop_)

samp3=data.frame(samp3[,colnames(dataset2)])
samp3$Type="Sampling"

dataset2$Type="Simulations"

overall=rbind(samp3,dataset2)

# samp3 is by length


# GROUP OF PLOTS NUMBER 1
dataset<-data.frame(dataset)
dataset<-dataset[!is.na(dataset$CV),]

for (mett in 1:length(met)){
  
  for (quartt in quart){
    
    for (catt in categ){
      
      this <- dataset[dataset$commCat==catt & dataset$Metier_Gear==met[mett] & dataset$quarter==quartt, ] 
      
      BASELINE_VALUE <- BASELINE_VALUE_tab$BASELINE_VALUE[ BASELINE_VALUE_tab$commCat==catt & 
                                                             BASELINE_VALUE_tab$ Metier_Gear==met[mett] & 
                                                             BASELINE_VALUE_tab$quarter==quartt]
     
      BASELINE_CV <-  samp4$CV[ samp4$commCat==catt & 
                                  samp4$ Metier_Gear==met[mett] & 
                                  samp4$quarter==quartt ] 
      
      this <- this[ this$samp_size  != BASELINE_VALUE, ] 
      
      if (nrow(this)>0){
        STEP = unique(this$samp_size)[2] - unique(this$samp_size )[1]
        this <- this[order(as.numeric(as.character(this$CV)),decreasing =TRUE) , ]
        b <- ggplot(this, aes(x = samp_size,y=CV))
        b + geom_point(stat = "identity") +  xlab("sample size") + ylab("CV %")   + ylim(c(0,max(this$CV))) + 
          theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
          ggtitle(paste(species_name, "CV - cat=",catt, "met=", met[mett], "quarter=", quartt))  + 
          geom_hline(aes(yintercept = quantile(this$CV, probs =c(0.05) )),col=as.character("blue"),linetype="dashed") + 
          geom_hline(aes(yintercept = quantile(this$CV, probs =c(0.25)) ),col=as.character("blue"),linetype="dashed") + 
          geom_hline(aes(yintercept = quantile(this$CV, probs =c(0.5) )),col=as.character("blue"),linetype="dashed") +
          geom_hline(aes(yintercept = quantile(this$CV, probs =c(0.75) )),col=as.character("blue"),linetype="dashed") + 
          geom_hline(aes(yintercept = quantile(this$CV, probs =c(0.95) )),col=as.character("blue"),linetype="dashed") +
          geom_hline(aes(yintercept = BASELINE_CV),col=as.character("red")) + 
          geom_text(aes(label = paste("Baseline [", BASELINE_VALUE, "]", sep="") , y = BASELINE_CV + 0.05, x=max(this$samp_size)-STEP*0.5 ),
                    position = position_dodge(0.9),	 vjust = 0 ) + 
          scale_x_continuous(breaks=unique(this$samp_size), labels= as.character(unique(this$samp_size)) )
        ggsave(paste(res_dir,"/Cat_",as.character(catt),"_met",mett,"_Q",quartt,"_CV_sex.jpg", sep=""), last_plot(), 
               height=8, width=8) 
      }
    }
    
  }
}


#GROUP OF PLOTS NUMBER 2  
  
  
dataset2<-overall  
  
    for (mett in met){
        for (quartt in quart){
            for (catt in categ){
              
              this <- dataset2[dataset2$commCat==catt & dataset2$Metier_Gear ==mett & dataset2$quarter==quartt, ]
             if (nrow(this) >0 ) { 
              b <- ggplot(this, aes(x = lenCls, y=pi,col=Type,type=Type))
              b + geom_point(stat = "identity") +  xlab("Length class") + ylab("Sex ratio") + facet_grid(.~samp_size)   +
                ylim(c(0,max(dataset2$pi))) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
                ggtitle(paste(species_name, "- Sex ratio - cat=",catt, "met=", mett, "quarter=", quartt))  
              ggsave(paste(res_dir,"/Cat ",catt,"_met",mett,"_Q",quartt,"_SR_at_length",".jpg", sep=""), last_plot(), 
                     height=8, width=16) 
            
            }
              }
            
          }
        }


# SUMMARY TABLE
# samp44 contains the sample CV
# dataset contains the simulations


# mean proprotion by length on the n iteration
overall_3 <- group_by( dataset2, Type, commCat , Metier_Gear, quarter, samp_size, lenCls)  
overall_3 <- data.frame(summarise(overall_3, mean_prop = mean(pi)  ))

for (mett in met){
  
  for (quartt in quart){
    
    for (catt in categ){
      
      this <- overall_3[overall_3$commCat==catt & overall_3$ Metier_Gear==mett & overall_3$quarter==quartt, ] 
      if (nrow(this) >0 ) { 
        
      b <- ggplot(this, aes(x = lenCls, y=mean_prop,col=Type,type=Type))
      b + geom_point(stat = "identity") + xlab("Length class") + ylab("Sex ratio") + facet_grid(.~samp_size)   + 
        ylim(c(0,max(dataset2$pi))) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        ggtitle(paste(species_name, "- SR - cat=",catt, "met=", mett, "quarter=", quartt))  
      ggsave(paste(res_dir,"/Cat ",catt,"_met",mett,"_Q",quartt,"_SR_at_length.jpg", sep=""), last_plot(),  height=8, width=16) 
    }  
    }
    
  }
}

summ_tab=aggregate(dataset$CV,by=list(Metier_Gear=dataset$Metier_Gear,quarter=dataset$quarter,samp_size=dataset$samp_size,commCat=dataset$commCat),FUN="mean")
colnames(summ_tab)[ncol(summ_tab)]="CV"  

samp4$samp_size="Baseline"
samp4=samp4[,colnames(summ_tab)]

tab=rbind(summ_tab,samp4)
  
tab=tab[order(tab[,1]),]
tab=tab[order(tab[,2]),]

# tab$Variation= "-"
tab$EMD= 0

for (i in c(1:nrow(tab)))
  if (as.character(tab$samp_size[i])!="Baseline"){
{
      ref=as.numeric(tab[as.character(tab$samp_size) == "Baseline" & tab$commCat==tab[i,]$commCat & tab$Metier_Gear==tab[i,]$Metier_Gear& tab$quarter==tab[i,]$quarter,]$CV)
      #tab[i,]$Variation=  paste(round((tab[i,]$CV-ref)/ref*100,1),"%",sep="")
      
      ref_SR=overall_3[overall_3$samp_size == samp_sizes[1] & overall_3$Type == "Sampling" & 
                               overall_3$commCat==tab[i,]$commCat & overall_3$Metier_Gear==tab[i,]$Metier_Gear & 
                               overall_3$quarter==tab[i,]$quarter,c("lenCls", "mean_prop") ] 
      
      this_SR=overall_3[overall_3$samp_size == as.numeric(tab$samp_size[i]) & overall_3$Type == "Simulations" &
                                overall_3$commCat==tab[i,]$commCat & overall_3$Metier_Gear==tab[i,]$Metier_Gear & 
                                overall_3$quarter==tab[i,]$quarter,c("lenCls", "mean_prop") ] 
      
      the_two_struct <- merge(ref_SR, this_SR, by =c("lenCls"), all=T )
      the_two_struct[is.na(the_two_struct[,])] <- 0
      
      tab[i,]$EMD <- round(emd2d(matrix(the_two_struct$mean_prop.x), matrix(the_two_struct$mean_prop.y) ), 2)
      
      
} 
    
  }

temp__ <- merge(tab, BASELINE_VALUE_tab)

temp__$samp_size[temp__$samp_size == "Baseline"] <- temp__$BASELINE_NAME[temp__$samp_size == "Baseline"] 
temp__ <- temp__[,1:6]

temp__$EMD <- round(temp__$EMD, 3)
colnames(temp__)[colnames(temp__) == "MWCV" ] <- "MWCV %"

write.table(temp__,paste(res_dir,"/Summary_table_SR.csv", sep=""),sep=";",row.names=F)

save.image(file=paste(res_dir, "\\D_sexratio_output.RData", sep=""))
