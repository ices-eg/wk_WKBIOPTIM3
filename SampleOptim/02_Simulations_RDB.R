#######################################################################################################
#######################################################################################################
#######################################################################################################
##
##
##   Biological sampling optimization (SampleOptim)
##   Developed by: Patricia Goncalves 
##   Version for: Regional DataBase (RDB) exchange format
##
##   Reference: 
##   Goncalves, Patricia 2019. SampleOptim a data analysis R-tool to optimize fish sampling for 
##   biological parameters as input on fish stock assessment.
##
##
##   github link: https://github.com/gonpatricia/SampleOptimRDBformat/edit/master/1_Simulations_RDB.R
## 
##
#######################################################################################################
#######################################################################################################
#######################################################################################################
##Packages: 
library(FSA)
library(FSAdata)
library(nlstools)
library(reshape)
library(ggplot2)
library(ggthemes)
library(cvTools)
library(dplyr)
library("robustbase")
library(MASS)
library(psyphy)
library(boot)
library(RCurl)

########################################################################################################
#### Files path and function source:
this_dir<-"C:/~/..." ##Set directory

setwd(paste(this_dir,"data",sep="/")) ##Set directory for data and settings


### Source file
source(paste(this_dir,"sample_selection_function_RDB.r",sep="/"))##Function for randomly select the samples by length class

### Biological sample data (Applied to a period of years)
### load input data
data_samplebio<- load(file="input_data.rdata")


### Prepare  directory for output files
out_dir<-paste(this_dir,"output",sep="/")
dir.create(out_dir)
setwd(out_dir)


###### Input settings file
sett<- read.table("input_params.csv",sep=";", header=F,row.names = c(), stringsAsFactors=FALSE)
stt<- data.frame(t(sett), stringsAsFactors=FALSE)
rownames(stt)<- c()
colnames(stt) <- as.character(unlist(stt[1,]))
stt = stt[-1, ]

### use this for parameters that can take FALSE/ TRUE as values
stt<-stt%>% dplyr::mutate_at(c("SEX_RATIO","distUniPorto","PORT"),function(x) type.convert(x[1]))

## convert to numeric if needed
stt[,1:ncol(stt)]=lapply(1:ncol(stt),function(x) {
  tryCatch({
    as.numeric(stt[[x]])
  },warning = function(w) {
    stt[[x]]}
  )} )

#########################################################################################################
#########################################################################################################
#########################################################################################################
#########################################################################################################
#### 2.1 Data preparation for run the simulations:
### REMOVE NAs in age data (Note: only use the individuals where age has atributted) 
### Age classes 
data_samplebio<-data_samplebio[!is.na(data_samplebio$Age),]

length_class<- seq(stt$MIN_LC,stt$MAX_LC,stt$interval_LC) ##set length class range and the intervals 

set.seed(2019)

########################################################################################################
########################################################################################################
########################################################################################################
####2.2  Set function for von Bertalanffy growth model (VBGM) parameters (Linf, K and t0)
########################################################################################################
vonberPoryear<-function(year){
  system.time(
    vvv <- lapply(1:n, function(i,years) {
    data<- amostraTemporal(data_samplebio[data_samplebio$year==years,], j, length_class,stt$SEX_RATIO,stt$TIME_STRATA, porto=stt$PORT, distUniPorto=stt$distUniPorto)
    ## Subsampling simulations
      fitTypical <-try( nls(vbTypical,data=data,start=svTypical,control), silent = TRUE)
      if (!inherits(fitTypical, "try-error")){
        coef<- summary(fitTypical)$coefficients
        Linf<- summary(fitTypical)$coefficients[[1]]
        K<- summary(fitTypical)$coefficients[[2]]
        t0<- summary(fitTypical)$coefficients[[3]]
        predict_values<-  predict(fitTypical,newdata=newdata)
       return(list(year=year,data=data,coef=coef,Linf=Linf,K=K,t0=t0,predict_values=predict_values,n=j))
      }      
      else{ ## For cases where is not possible to adjust the VBGM to data, but we want to proceed with the subsampling simulations
        Linf=NA
        K=NA
        t0=NA
      return(list(Linf=Linf,K=K,t0=t0,year=year,data=data,n=j))
      }
    }, year)
  )[[3]] #
  return(vvv)
}

#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
#####################   **  START the SIMULATION PROCESS  **  ###########################################################
#########################################################################################################################
#########################################################################################################################
#########################################################################################################################
### Define the temporal stratification (T= Quarter; S= semester; A= year)
## SR=1:1  #define the sexratio stratification (SR=1 only females; SR=0 only males; SR=0.5 the same proportion of males and females)
## Setup the conditions for Ports stratification (example:porto=FALSE, distUniPorto=FALSE)
## j - is the number of otoliths/individuals selected by length class
#########################################################################################################################
#########################################################################################################################
##### Initial values set and fixed by species
###Initial values for parameters to all the simulations
svTypical <- list(Linf=stt$Linf,K=stt$K,t0=-stt$to) ##Initial parameters values for the growth curve 
vbTypical <- Length_class~Linf*(1-exp(-K*(Age-t0))) ##von Bertallanfy growth model
control<- nls.control(maxiter=10000)
n <- stt$n  ##Number of iterations for simulation process 
tm<- stt$TIME_STRATA ##Time interval (T="quarter", A="year", S="semester"), for the otoliths selection by length class

## Define the age distribution (according to the original dataset) for predictions
newdata<-seq(stt$MIN_age,stt$MAX_age,0.1) ###set age distribution vector for predictions
newdata<-data.frame(newdata)
colnames(newdata)<- "Age"
year_init<- stt$year_start
year_last<- stt$year_end
years<- seq(year_init,year_last,by=1) ##set de dados definido com base no ajuste the VB j=1

years<-years[years %in% unique(data_samplebio$year)]
otol.per.lc=seq(stt$MIN_OTOL.Read,stt$MAX_OTOL.Read,stt$interval_OTOL.Read) # Number of otoliths by length class as a setup for simulations

##########################################################################################################
### Running the function for the different options on the number of otoliths/individuals by length class 
   
 for (j in otol.per.lc ){ ###  is the number of otoliths/individuals selected by length class
  
  vonber<-sapply(years, vonberPoryear) 

######################################
######################################
#extracting the variables from each of the 100 samples (subsamples) 
vb_year<- sapply(vonber, function(x) x$data$year)
vb_Age<- sapply(vonber, function(x) x$data$Age)
vb_ct<- sapply(vonber, function(x) x$data$Length_class)
vb_linf<- as.list(sapply(vonber, function(x) x$Linf))
vb_k<- as.list(sapply(vonber, function(x) x$K))
vb_t0<- as.list(sapply(vonber, function(x) x$t0))
vb_n<- as.list(sapply(vonber, function(x) x$n))
vb_sex<- sapply(vonber, function(x) x$data$Sex)
vb_matur<- sapply(vonber, function(x) x$data$Maturity_stage)
vb_weight<- sapply(vonber, function(x) x$data$Weight)
vb_month<- sapply(vonber, function(x) x$data$month)
  
##Data of length, age, sex, matutity stage, weight, month and year
vb_year_melt<- melt(vb_year)
vb_Age_melt <- melt(vb_Age) #age
vb_ct_melt<- melt(vb_ct) #length
vb_sex_melt<-melt(vb_sex) #sex
vb_mat_melt<-melt(vb_matur) ##maturuty stage
vb_wt_melt<-melt(vb_weight) #fish total weight
vb_month_melt<-melt(vb_month) #month
dados_bio<-cbind(vb_ct_melt,vb_Age_melt,vb_year_melt,vb_sex_melt,vb_mat_melt,vb_wt_melt,vb_month_melt)
dados_bio<- dados_bio[,c(grep("value",names(dados_bio)),grep("L1",names(dados_bio))[1])]
  
colnames(dados_bio)<-c("Lt","age","year","sex","mat_stg","wt","month","ID_sim")
dados_bio$mat_stg<-as.numeric(dados_bio$mat_stg)
dados_bio$maturity<-ifelse(dados_bio$mat_stg==stt$stage_mature[1], 0, ifelse(dados_bio$mat_stg>stt$stage_mature[1],1,NA))
dados_bio$quarter<-ifelse(dados_bio$mont<4, 1, ifelse(dados_bio$mat_stg>1,1,NA))
dados_bio$quarter<-factor(NA,levels=c("1","2","3","4"))
dados_bio[dados_bio$month<=3,"quarter"]<-"1"
dados_bio[dados_bio$month>3 & dados_bio$month<=6,"quarter"]<-"2"
dados_bio[dados_bio$month>6 & dados_bio$month<=9,"quarter"]<-"3"
dados_bio[dados_bio$month>9, "quarter"]<-"4"
dados_bio$type<-j
write.table(dados_bio,paste("dados_bio_",j,".csv",sep=""),sep=",")
  
### Predict values
vb_predict<- sapply(vonber, function(x) x$predict_values)
vb_predict_melt <- try(melt(vb_predict),silent=T)
   
 if(!inherits(vb_predict_melt, "try-error")){
    vb_predict_melt$type<-j
    vb_predict_melt$year<-rep(years,each=n)###
    colnames(vb_predict_melt)<-c("ID_ind","ID_sim","pred_lt","type","year")
    write.table(vb_predict_melt,paste("vb_predict_melt_",j,".csv",sep=""),sep=",")
  }
  
  
###Data of length, age
vb_year_melt<- melt(vb_year)
vb_Age_melt <- melt(vb_Age)
vb_ct_melt<- melt(vb_ct)
dados_lt_age<-cbind(vb_ct_melt,vb_Age_melt,vb_year_melt)
  
dados_lt_age<-dados_lt_age[,c(grep("value",names(dados_lt_age)),grep("L1",names(dados_lt_age))[1])]
colnames(dados_lt_age)<-c("Lt","age","year","ID_sim")
dados_lt_age$type<-j
write.table(dados_lt_age,paste("dados_lt_age_",j,".csv",sep=""),sep=",")

###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###      MATURITY OGIVE
###  DETERMINE: L25, L50, L75
## Confindence intervals by year
##
##
## NOTE: COULD USE A DATA SUBSET FOR THE SPAWNING SEASON PERIOD
###############################################################################################
###############################################################################################
years<-unique(dados_bio$year)
  
if(stt$VARIABLE!="age")  {
  # 
  table_mature<- function (data=dados_bio){
  sim<-unique(data$ID_sim)
  results<-matrix(nrow=length(sim),ncol=6)
for(nb in 1: length(sim))
  {
  glm1 <- glm(factor(maturity)~Lt,family=binomial,data=data[data$ID_sim==nb,])
  Lmat <- signif(dose.p (glm1, p = c(0.25, 0.50, 0.75)), digits = 3)
  results[nb,1]<-unique(data$year[data$ID_sim==nb]) #year
  results[nb,2]<-as.numeric(Lmat[[1]]) #L25
  results[nb,3]<-as.numeric(Lmat[[2]]) #L50
  results[nb,4]<-as.numeric(Lmat[[3]]) #L75
  results[nb,5]<-nb ##ID_simulacao
  results[nb,6]<-unique(data$type) ##j
  nb<-nb+1
    }
  colnames(results)<-c("year","L25","L50","L75","ID_sim","type")
  results
}

### DATA SUBSET FOR THE SPAWNING SEASON PERIOD
# Option A:
table_mo<-table_mature(data=dados_bio[dados_bio$quarter=="1",]) ##Option of using only the 1st quarter data (from the spawning period)

### DATA FROM THE WHOLE YEAR
# Option b:
table_mo<-table_mature(data=dados_bio) ##Data from the all year
write.table(table_mo,paste("table_res_mo_",j,".csv",sep=""),sep=",")
  }
  
#### Figure 3 - compare length and age distributions by year (by simulations)
## Note: the length distribution did not change by simulations 
## (because the selection is based in the number of otoliths by length class)
################ Data from simulations - figures (length and age distribution)

Fig3_length_path <- file.path(paste("Fig3_length_", j,tm, ".jpg", sep = ""))
Fig3_length<- ggplot(dados_lt_age[dados_lt_age$age!=33,], aes(x=Lt, colour=factor(ID_sim))) +
              geom_density(show.legend = FALSE)+facet_wrap(~ year, ncol=2)+theme_classic()
              ggsave(Fig3_length_path)
              try(dev.off(),silent=T)
  
Fig3_age_path <- file.path(paste("Fig3_age_", j, tm, ".jpg", sep = ""))
Fig3_age<- ggplot(dados_lt_age[dados_lt_age$age!=33,], aes(x=age, colour=factor(ID_sim))) +
           geom_density(show.legend = FALSE)+facet_wrap(~ year, ncol=2)+theme_classic()
           ggsave(filename=Fig3_age_path)
           try(dev.off(),silent=T)
  
#### Determine mean length at age - original data and by simulation (for each year)
table_original<-group_by(data_samplebio, Age, year) %>% summarize(m_lt = mean(Length_class))
table_original$data<-"original" 
table_original$ID_sim<-0
table_original$type<-j
colnames(table_original)<- c("age","year","m_lt","data","ID_sim","type")
  
table_simul<-group_by(dados_lt_age, age, year, ID_sim) %>% summarize(m_lt = mean(Lt))
table_simul$data<-"simulations"
table_simul$type<-j
table_simul<-table_simul[,c(1,2,4,5,3,6)]### organize the columns 
  
######
## Combine data original and simulations in one table (data frame)
table_original_simul<- merge(table_original,table_simul,all=TRUE) 
year_simul<-years
table_original_simulsub<-table_original_simul[table_original_simul$year %in% year_simul,]
write.table(table_original_simulsub,paste("table_original_simulsub_mla_",j,".csv",sep=""),sep=",")
  
#############
### Compare mean length at age from distributions of original data with the data from simulations
Fig4_length_path <- file.path(paste("Fig4_length_", j, tm,".jpg", sep = ""))
Fig4_length<- ggplot(table_original_simulsub, aes(x=factor(age), y=m_lt, fill=factor(type))) +
              geom_bar(stat="identity", position=position_dodge())+facet_wrap(~ year, ncol=2)+xlab("Age")+
              ylab("Mean length (cm)")+ theme_classic()
              ggsave(filename=Fig4_length_path)
              try(dev.off(),silent=T)
  
Fig4_age_path <- file.path(paste("Fig4_age_", j, tm,".jpg", sep = ""))
Fig4_age<- ggplot(table_original_simulsub, aes(x=factor(age),y=m_lt,colour=factor(type))) +
           geom_boxplot()+facet_wrap(~ year, ncol=2)+xlab("Age")+
           ylab("Mean length (cm)")+theme_classic()
           ggsave(Fig4_length_path)
           try(dev.off(),silent=T)
  
#### Determine standard deviation (length) at age - original data and by simulation (for each year)
table_originalsd<-group_by(data_samplebio, Age, year) %>% summarize(sd_lt = sd(Length_class))
table_originalsd$data<-"original" 
table_originalsd$ID_sim<-0
table_originalsd$type<-j
colnames(table_originalsd)<- c("age","year","sd_lt","data","ID_sim","type")
  
table_simulsd<-group_by(dados_lt_age, age, year, ID_sim) %>% summarize(sd_lt = sd(Lt))
table_simulsd$data<-"simulations"
table_simulsd$type<-j
table_simulsd<-table_simulsd[,c(1,2,4,5,3,6)]###organize columns 

######
## Combine data original and simulations in one table (data frame)
table_original_simulsd<- merge(table_originalsd,table_simulsd,all=TRUE) 
year_simul<-years
table_original_simulsubsd<-table_original_simulsd[table_original_simulsd$year %in% year_simul,]
write.table(table_original_simulsubsd,paste("table_original_simulsub_sd_",j,".csv",sep=""),sep=",")
  
#############
### Compare standard deviation of length at age from distributions of original data with the data from simulations
Fig5_length_path <- file.path(paste("Fig5_length_", j, tm, ".jpg", sep = ""))
Fig5_length<- ggplot(table_original_simulsubsd, aes(x=factor(age), y=sd_lt, fill=factor(type))) +
              geom_bar(stat="identity", position=position_dodge())+facet_wrap(~ year, ncol=2)+xlab("Age")+
              ylab("Standard deviation of length (cm)")+theme_classic()
              ggsave(Fig5_length_path)
              try(dev.off(),silent=T)
  
Fig5_age_path <- file.path(paste("Fig5_age_", j, tm, ".jpg", sep = ""))
Fig5_age<- ggplot(table_original_simulsubsd, aes(x=factor(age),y=sd_lt,colour=factor(type))) +
           geom_boxplot()+
           facet_wrap(~ year, ncol=2)+xlab("Age")+
           ylab("Standard deviation of length (cm)")+theme_classic()
           ggsave(Fig5_age_path)
           try(dev.off(),silent=T)
  
  
#####################################################################################################################
#####################################################################################################################
### Growth parameters from the von Bertallanfy model by year
vb_yearunique_melt<- unique(vb_year_melt)
colnames(vb_yearunique_melt)<- c("year","ID_sim")
vb_linf_melt <- melt(vb_linf)
colnames(vb_linf_melt)<- c("Linf","ID_sim")
vb_k_melt<- melt(vb_k)
colnames(vb_k_melt)<- c("k","ID_sim")
vb_t0_melt<- melt(vb_t0)
colnames(vb_t0_melt)<- c("t0","ID_sim")
trimestre_simulvb<- cbind(vb_linf_melt, vb_k_melt, vb_t0_melt,vb_yearunique_melt)
trimestre_simulvb$type<-j
colnames(trimestre_simulvb)<- c("Linf","ID_sim","K","ID_sim","t0","ID_sim","year","ID_sim","type")
trimestre_simulvb<- trimestre_simulvb[,-c(grep("ID_sim",names(trimestre_simulvb)))[-1]]
write.table(trimestre_simulvb,paste("results_simulvbgm_",j,".csv",sep=""),sep=",")
  
  
#### For all the years, to compare the VGBGM parameters between years
#### Figure 6 - Summary of parameters by year for the full set of simulations (n), by j (number of selected otoliths)
Fig6_K_VBGM_path <- file.path(paste("Fig6_K_VBGM_", j, tm, ".jpg", sep = ""))
fig6_K<-ggplot(trimestre_simulvb, aes(x=factor(year), y=K)) + 
        geom_boxplot()+xlab("year")+theme_classic()+
        theme(axis.title.y = element_text(size = 14),axis.title.x=element_text(size=14),
        axis.line = element_line(size = 0.5),axis.text = element_text(size = 10))
        ggsave(Fig6_K_VBGM_path)
        try(dev.off(),silent=T)
  
Fig6_t0_VBGM_path <- file.path(paste("Fig6_t0_VBGM_", j, tm, ".jpg", sep = ""))
fig6_t0<-ggplot(trimestre_simulvb, aes(x=factor(year), y=t0)) + 
        geom_boxplot()+xlab("year")+theme_classic()+
        theme(axis.title.y = element_text(size = 14),axis.title.x=element_text(size=14),
        axis.line = element_line(size = 0.5),axis.text = element_text(size = 10))
        ggsave(Fig6_t0_VBGM_path)
        try(dev.off(),silent=T)
  
Fig6_Linf_VBGM <- file.path(paste("Fig6_Linf_VBGM_", j, tm, ".jpg", sep = ""))
fig6_Linf<-ggplot(trimestre_simulvb, aes(x=factor(year), y=Linf)) + 
          geom_boxplot()+xlab("year")+theme_classic()+
          theme(axis.title.y = element_text(size = 14),axis.title.x=element_text(size=14),
          axis.line = element_line(size = 0.5),axis.text = element_text(size = 10))
          ggsave(Fig6_Linf_VBGM)
          try(dev.off(),silent=T)
  
#### Von bertallanfy growth model 
svTypical <- list(Linf=stt$Linf,K=stt$K,t0=stt$to) ##Initial growth parameters 
vbTypical <- Lt~Linf*(1-exp(-K*(age-t0))) ##von Bertallanfy growth model
control<- nls.control(maxiter=10000)
  
  
############################################################################################################################
############################################################################################################################
### Determining mean square error between sim and obs (cost=mspe, mape, rtmspe)
### mspe(sim, obs, na.rm=TRUE)
############################################################################################################################
############################################################################################################################
table_results<- function (data=dados_lt_age){
results<-matrix(nrow=length(years),ncol=8)
years<-unique(dados_lt_age$year)
    for(nb in 1: length(years))
    {
    fitTypical <- try(nls(vbTypical,data=data[data$year==years[nb],],start=svTypical,control),silent=T)
    mspe_fit<- try(cvFit(fitTypical,Lt ~ Linf * (1 - exp(-K * (age - t0))),data=data[data$year==years[nb],],y=data$age[data$year==years[nb]], cost=mspe, k=10),silent=T)
    mape_fit<-try(cvFit(fitTypical,Lt~Linf*(1-exp(-K*(age-t0))),data=data[data$year==years[nb],],y=data$age[data$year==years[nb]], cost=mape, k=10),silent=T)
    rtmspe_fit<-try(cvFit(fitTypical,Lt~Linf*(1-exp(-K*(age-t0))),data=data[data$year==years[nb],],y=data$age[data$year==years[nb]], cost=rtmspe, k=10),silent=T)
    results[nb,1]<-as.numeric(years[nb])
    if(!inherits(fitTypical, "try-error")){
        results[nb,2]<-as.numeric(coef(fitTypical)[1])
        results[nb,3]<-as.numeric(coef(fitTypical)[2])
        results[nb,4]<-as.numeric(coef(fitTypical)[3])
        results[nb,5]<-as.numeric(mspe_fit$cv)
        results[nb,6]<-as.numeric(mape_fit$cv)
        results[nb,7]<-as.numeric(rtmspe_fit$cv)
        results[nb,8]<-unique(data$type) ##j
      } else{
        results[nb,2:8]<-NA
      }
      nb<-nb+1
    }
    colnames(results)<-c("year","Linf","k","t0","mspe","mape","rtmspe","type")
    results
  }
  table_res<-table_results(data=dados_lt_age)
  write.table(table_res,paste("table_res_stat_",j,".csv",sep=""),sep=",")
 }

##################################### ## END ## ###########################################################
###########################################################################################################
###########################################################################################################
