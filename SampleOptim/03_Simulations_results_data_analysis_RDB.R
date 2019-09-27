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
##   github link: https://github.com/gonpatricia/SampleOptimRDBformat/edit/master/3_Simulations_results_data_analysis_RDB.R
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


###########################################################################################################
###########################################################################################################
###########################################################################################################
###########################################################################################################
############# 3. Simulation results (Plots preparation)
#############   Agregate all the data available from the different simulation runs
#############  
############################################################################################################
this_dir<-"C:/~/..." ##Set directory
out_dir<-paste(this_dir,"output",sep="/") ##outputs from 02_Simulations_RDB.R
sett<- read.table(paste0(this_dir,"data/","input_params.csv"),sep=";", header=F,row.names = c(), stringsAsFactors=FALSE)

stt<-data.frame(t(sett), stringsAsFactors=FALSE)
rownames(stt)<- c()
colnames(stt) <- as.character(unlist(stt[1,]))
stt = stt[-1, ]
stt[,1:ncol(stt)]=lapply(1:ncol(stt),function(x) {
  tryCatch({
    as.numeric(stt[[x]])
  },warning = function(w) {
    stt[[x]]}
  )} )


setwd(out_dir)

#############   For all the matrixes generated from the cod
#### Data of age, length by year (from the individuals selected in each simulation run)

tm<- stt$TIME_STRATA
data_selected_lt_age<- data.frame()
            for (i in c(stt$MIN_OTOL.Read:stt$MAX_OTOL.Read)){
                 get<-read.table(paste0("dados_lt_age_",i,".csv"),sep=",", header=T)
                 assign(paste0("dados_lt_age_", i),get)
                 data_selected_lt_age<- rbind(data_selected_lt_age,get)
 }



############################################################################
####   von Bertalanffy growth model parameters by year and simulation run
############################################################################
#############################################################################

simulvbgm_param_year=data.frame()
      for (i in c(stt$MIN_OTOL.Read:stt$MAX_OTOL.Read)){
          get<-read.table(paste0("results_simulvbgm_",i,".csv"),sep=",", header=T)
          assign(paste0("results_simulvbgm_", i),get)
          simulvbgm_param_year<- rbind(simulvbgm_param_year,get)
}


####For all the years, to compare the VGBGM parameters between years
####Figure 7 - Summary of parameters by year for the full set of simulations (n=100), for j's (number of selected otoliths)
Fig7_K_VBGM <- file.path(paste("Fig7_K_VBGM_", tm, ".jpg", sep = ""))
fig7_K<-ggplot(simulvbgm_param_year, aes(x=factor(type), y=K, fill=factor(type))) + 
        geom_boxplot(outlier.shape=NA)+xlab("number of otoliths selected by length class (cm)")+theme_classic()+
        facet_wrap(~year)+
        theme(axis.title.y = element_text(size = 14),axis.title.x=element_text(size=14),
        axis.line = element_line(size = 0.5),axis.text = element_text(size = 10))
ggsave(Fig7_K_VBGM)
try(dev.off(),silent=T)


Fig7_t0_VBGM <- file.path(paste("Fig7_t0_VBGM_", tm, ".jpg", sep = ""))
fig7_t0<-ggplot(simulvbgm_param_year, aes(x=factor(type), y=t0,  fill=factor(type))) +
         geom_boxplot(outlier.shape=NA)+xlab("number of otoliths selected by length class (cm)")+theme_classic()+
         facet_wrap(~year)+
        theme(axis.title.y = element_text(size = 14),axis.title.x=element_text(size=14),
        axis.line = element_line(size = 0.5),axis.text = element_text(size = 10))
ggsave(Fig7_t0_VBGM)
try(dev.off(),silent=T)


Fig7_Linf_VBGM <- file.path(paste("Fig7_Linf_VBGM_", tm, ".jpg", sep = ""))
fig7_Linf<-ggplot(simulvbgm_param_year, aes(x=factor(type), y=Linf, fill=factor(type))) +
           geom_boxplot(outlier.shape=NA)+xlab("number of otoliths selected by length class (cm)")+theme_classic()+
           facet_wrap(~year)+
           theme(axis.title.y = element_text(size = 14),axis.title.x=element_text(size=14),
           axis.line = element_line(size = 0.5),axis.text = element_text(size = 10))
ggsave(Fig7_Linf_VBGM)
try(dev.off(),silent=T)


####################################################################################
#####  Mean length at age data original and by simulation run
###################################################################################
####################################################################################
table_mla_sims<- data.frame()
         for (i in c(stt$MIN_OTOL.Read:stt$MAX_OTOL.Read)){
             get<-read.table(paste0("table_original_simulsub_mla_",i,".csv"),sep=",", header=T)
             assign(paste0("table_original_simulsub_mla_", i),get)
             table_mla_sims<- rbind(table_mla_sims,get)
}

###############################################################################################################
###### Figure 8 - compare mean length at age (oringinal versus simulation) by year and simulation run
#######
tm<- "T"
years<-unique(table_mla_sims$year)

for(i in years){
  fig8_mla<- ggplot(data=subset(table_mla_sims, year==i),aes(x=factor(age), y=m_lt,fill=factor(type))) +
    geom_boxplot(outlier.shape = NA)+xlab("age")+ylab("mean length at age (cm)")+scale_color_brewer(palette = "Paired")+ theme_classic()+
    facet_wrap(~year)+ggtitle(i)+
    theme(axis.title.y = element_text(size = 14),axis.title.x=element_text(size=14),
          axis.line = element_line(size = 0.5),axis.text = element_text(size = 10))
  ggsave(fig8_mla, file=paste0("Fig8_mla_",i,tm,".png"),width=14, height = 10, units="cm")
}


#############################################################################################
#############################################################################################
######      Standard deviation from length at age by simualtion run
#############################################################################################
#############################################################################################

table_sdla_sims=data.frame()
for (i in c(stt$MIN_OTOL.Read:stt$MAX_OTOL.Read)){
    get<-read.table(paste0("table_original_simulsub_sd_",i,".csv"),sep=",", header=T)
    assign(paste0("table_original_simulsub_sd_", i),get)
    table_sdla_sims<- rbind(table_sdla_sims,get)
}

###############################################################################################################
###### Figure 9 - compare the sd length at age (oringinal versus simulation) by year and simulation run
#######
#table_sdla_sims<-table_sdla_sims[!is.na(table_sdla_sims$sd_lt),]##remove NAs on data[table_sdla_sims$year==years[[nb]],]

years<-unique(table_sdla_sims$year)
for(i in years){
  fig9_sdla<- ggplot(data=subset(table_sdla_sims, year==i),aes(x=factor(age), y=sd_lt,fill=factor(type))) +
    geom_boxplot(outlier.shape =NA)+xlab("age")+ylab("sd length at age (cm)")+scale_color_brewer(palette = "Paired")+ theme_classic()+
    facet_wrap(~year)+ggtitle(i)+
    theme(axis.title.y = element_text(size = 14),axis.title.x=element_text(size=14),
          axis.line = element_line(size = 0.5),axis.text = element_text(size = 10))
  ggsave(fig9_sdla, file=paste0("Fig9_sdla_",i,tm,".png"),width=14, height = 10, units="cm")
}


#################################################################################################
#################################################################################################
###    Data combine (table_mla_sims, table_sdla_sims)
##################################################################################################

data_mla_sd<- cbind(table_mla_sims, table_sdla_sims)
data_mla_sd<- data_mla_sd[,c(1,2,3,9,10,11,12)]
data_mla_sd<-data_mla_sd[complete.cases(data_mla_sd$sd_lt),]
data_mla_sd$coefv<-data_mla_sd$m_lt/data_mla_sd$sd_lt


years<-unique(data_mla_sd$year)
for(i in years){
  fig9_cvla<- ggplot(data=subset(data_mla_sd, year==i),aes(x=factor(age), y=coefv,fill=factor(type))) +
    geom_boxplot(outlier.shape =NA)+xlab("age")+ylab("CV (lenght)")+scale_color_brewer(palette = "Paired")+ theme_classic()+
    facet_wrap(~year)+ggtitle(i)+
    theme(axis.title.y = element_text(size = 14),axis.title.x=element_text(size=14),
          axis.line = element_line(size = 0.5),axis.text = element_text(size = 10))
  ggsave(fig9_cvla, file=paste0("Fig9a_cvla_",i,tm,".png"),width=14, height = 10, units="cm")
}


######################################################################################################
### Stats (mape, rmspe, mspe) from each simualations run and year
######################################################################################################
######################################################################################################
stats_simul<- data.frame()
    for (i in c(stt$MIN_OTOL.Read:stt$MAX_OTOL.Read)){
         get<-read.table(paste0("table_res_stat_",i,".csv"),sep=",", header=T)
         assign(paste0("table_res_stat_", i),get)
         stats_simul<- rbind(stats_simul,get)
}

#summary(stats_simul)

###########################################################################################################################
###### Figure 10 - compare the stats (mape, mspe, rtmspe) by year and by simulation type (number of otoliths/length class)
###########################################################################################################################

Fig10_mspe <- file.path(paste("Fig10_mspe_", tm, ".jpg", sep = ""))
fig10_mspe<-ggplot(stats_simul, aes(x=factor(type), y=mspe, group=1)) + geom_step()+
            #geom_point(size=2)+
            xlab("number of otoliths selected by length class (cm)")+theme_classic()+
            facet_wrap(~year)+
            theme(axis.title.y = element_text(size = 14),axis.title.x=element_text(size=14),
            axis.line = element_line(size = 0.5),axis.text = element_text(size = 10))
ggsave(Fig10_mspe)
try(dev.off(),silent=T)

Fig10_mape <- file.path(paste("Fig10_mape_", tm, ".jpg", sep = ""))
fig10_mape<-ggplot(stats_simul, aes(x=factor(type), y=mape, group=1)) + geom_step()+
            xlab("number of otoliths selected by length class (cm)")+theme_classic()+
            facet_wrap(~year)+
            theme(axis.title.y = element_text(size = 14),axis.title.x=element_text(size=14),
            axis.line = element_line(size = 0.5),axis.text = element_text(size = 10))
ggsave(Fig10_mape)
try(dev.off(),silent=T)

Fig10_rtmspe <- file.path(paste("Fig10_rtmspe_", tm, ".jpg", sep = ""))
fig10_rtmspe<-ggplot(stats_simul, aes(x=factor(type), y=rtmspe, group=1)) + geom_step()+
              xlab("number of otoliths selected by length class (cm)")+theme_classic()+
              facet_wrap(~year)+
              theme(axis.title.y = element_text(size = 14),axis.title.x=element_text(size=14),
              axis.line = element_line(size = 0.5),axis.text = element_text(size = 10))
ggsave(Fig10_rtmspe)
try(dev.off(),silent=T)


###### Normalization of the data stats
library(dplyr)

##Mean by stats by year
mean_mspe <- stats_simul %>% group_by(year) %>% summarize(Mean = mean(mspe, na.rm=TRUE))
mean_mape <- stats_simul %>% group_by(year) %>% summarize(Mean = mean(mape, na.rm=TRUE))
mean_rtmspe <- stats_simul %>% group_by(year) %>% summarize(Mean = mean(rtmspe, na.rm=TRUE))

### Normalization of data by stats and year (max)
nor_mspe<- stats_simul %>% group_by(year) %>% mutate(norm_mspe = mspe/max(mspe))
nor_mape<- stats_simul %>% group_by(year) %>% mutate(norm_mape = mape/max(mape))
nor_rtmspe<- stats_simul %>% group_by(year) %>% mutate(norm_rtmspe = rtmspe/max(rtmspe))


Fig10a_norm_mspe <- file.path(paste("Fig10_norm_mspe_", tm, ".jpg", sep = ""))
fig10a_mspe<-ggplot(nor_mspe, aes(x=factor(type), y=norm_mspe, group=1)) + geom_step()+
             #geom_point(size=2)+
             xlab("number of otoliths selected by length class (cm)")+theme_classic()+
             facet_wrap(~year)+
             theme(axis.title.y = element_text(size = 14),axis.title.x=element_text(size=14),
            axis.line = element_line(size = 0.5),axis.text = element_text(size = 10))
ggsave(Fig10a_norm_mspe)
try(dev.off(),silent=T)

Fig10a_norm_mape <- file.path(paste("Fig10_norm_mape_", tm, ".jpg", sep = ""))
fig10a_mape<-ggplot(nor_mape, aes(x=factor(type), y=norm_mape, group=1)) + geom_step(stat="summary", fun.y = mean)+
             xlab("number of otoliths selected by length class (cm)")+theme_classic()+
             facet_wrap(~year)+
             theme(axis.title.y = element_text(size = 14),axis.title.x=element_text(size=14),
             axis.line = element_line(size = 0.5),axis.text = element_text(size = 10))
ggsave(Fig10a_norm_mape)
try(dev.off(),silent=T)

Fig10a_norm_rtmspe <- file.path(paste("Fig10_norm_rtmspe_", tm, ".jpg", sep = ""))
fig10a_rtmspe<-ggplot(nor_rtmspe, aes(x=factor(type), y=norm_rtmspe, group=1)) + geom_step(stat="summary", fun.y = mean)+
               xlab("number of otoliths selected by length class (cm)")+theme_classic()+
               facet_wrap(~year)+
               theme(axis.title.y = element_text(size = 14),axis.title.x=element_text(size=14),
              axis.line = element_line(size = 0.5),axis.text = element_text(size = 10))
ggsave(Fig10a_norm_rtmspe)
try(dev.off(),silent=T)


### Normalization of data by stats and year (mean)
nor_mn_mspe<- stats_simul %>% group_by(year) %>% mutate(norm_mn_mspe = mspe/mean(mspe))
nor_mn_mape<- stats_simul %>% group_by(year) %>% mutate(norm_mn_mape = mape/mean(mape))
nor_mn_rtmspe<- stats_simul %>% group_by(year) %>% mutate(norm_mn_rtmspe = rtmspe/mean(rtmspe))


Fig10a_norm_mn_mspe <- file.path(paste("Fig10_norm_mn_mspe_", tm, ".jpg", sep = ""))
fig10a_mn_mspe<-ggplot(nor_mn_mspe, aes(x=factor(type), y=norm_mn_mspe, group=1)) + geom_step()+
               #geom_point(size=2)+
               xlab("number of otoliths selected by length class (cm)")+theme_classic()+
               facet_wrap(~year)+
               theme(axis.title.y = element_text(size = 14),axis.title.x=element_text(size=14),
              axis.line = element_line(size = 0.5),axis.text = element_text(size = 10))
ggsave(Fig10a_norm_mn_mspe)
try(dev.off(),silent=T)

Fig10a_norm_mn_mape <- file.path(paste("Fig10_norm_mn_mape_", tm, ".jpg", sep = ""))
fig10a_mn_mape<-ggplot(nor_mn_mape, aes(x=factor(type), y=norm_mn_mape, group=1)) + geom_step(stat="summary", fun.y = mean)+
                xlab("number of otoliths selected by length class (cm)")+theme_classic()+
                facet_wrap(~year)+
                theme(axis.title.y = element_text(size = 14),axis.title.x=element_text(size=14),
                axis.line = element_line(size = 0.5),axis.text = element_text(size = 10))
ggsave(Fig10a_norm_mn_mape)
try(dev.off(),silent=T)

Fig10a_norm_mn_rtmspe <- file.path(paste("Fig10_norm_mn_rtmspe_", tm, ".jpg", sep = ""))
fig10a_mn_rtmspe<-ggplot(nor_mn_rtmspe, aes(x=factor(type), y=norm_mn_rtmspe, group=1)) + geom_step(stat="summary", fun.y = mean)+
                  xlab("number of otoliths selected by length class (cm)")+theme_classic()+
                  facet_wrap(~year)+
                  theme(axis.title.y = element_text(size = 14),axis.title.x=element_text(size=14),
                  axis.line = element_line(size = 0.5),axis.text = element_text(size = 10))
ggsave(Fig10a_norm_mn_rtmspe)
try(dev.off(),silent=T)



###################################################################################################################
####### Comparison of von Bertalanffy growth model parameters estimated by year
###### and for each j (number of otoliths by length class) the result of the 100 simulations aggregated
###################################################################################################################
Fig11_K_VBGM <- file.path(paste("Fig11_K_VBGM_", tm, ".jpg", sep = ""))
fig11_K<-ggplot(stats_simul, aes(x=factor(type), y=k)) + 
         geom_point(size=2,colour="green")+xlab("number of otoliths selected by length class (cm)")+theme_classic()+
         facet_wrap(~year)+
         theme(axis.title.y = element_text(size = 14),axis.title.x=element_text(size=14),
        axis.line = element_line(size = 0.5),axis.text = element_text(size = 10))
ggsave(Fig11_K_VBGM)
try(dev.off(),silent=T)

Fig11_t0_VBGM <- file.path(paste("Fig11_t0_VBGM_", tm, ".jpg", sep = ""))
fig11_t0<-ggplot(stats_simul, aes(x=factor(type), y=t0)) +
          geom_point(size=2,colour="green")+xlab("number of otoliths selected by length class (cm)")+theme_classic()+
          facet_wrap(~year)+
          theme(axis.title.y = element_text(size = 14),axis.title.x=element_text(size=14),
          axis.line = element_line(size = 0.5),axis.text = element_text(size = 10))
ggsave(Fig11_t0_VBGM)
try(dev.off(),silent=T)

Fig11_Linf_VBGM <- file.path(paste("Fig11_Linf_VBGM_", tm, ".jpg", sep = ""))
fig11_Linf<-ggplot(stats_simul, aes(x=factor(type), y=Linf)) +
            geom_point(size=2,colour="green")+xlab("number of otoliths selected by length class (cm)")+theme_classic()+
            facet_wrap(~year)+#ylim(c(20,100))+
            theme(axis.title.y = element_text(size = 14),axis.title.x=element_text(size=14),
            axis.line = element_line(size = 0.5),axis.text = element_text(size = 10))
ggsave(Fig11_Linf_VBGM)
try(dev.off(),silent=T)




################################################################################################################
###    Based on the VBGM by sim and year, the length for each selected fish was predicted based on age data
###########################################################################################################
#################################################################################################################
lpredict_vbsim<- data.frame()
               for (i in c(stt$MIN_OTOL.Read:stt$MAX_OTOL.Read)){
                   get<-try(read.table(paste0("vb_predict_melt_",i,".csv"),sep=",", header=T),silent=T)
               if (!inherits(get, "try-error")){
                  assign(paste0("vb_predict_melt_", i),get)
                  lpredict_vbsim<- rbind(lpredict_vbsim,get)
  }
}


###############################################################################################################
###### Figure 12 - compare the stats (mape, mspe, rtmspe) by year and by simulation type (number of otoliths/length class)
Fig12_predictLt <- file.path(paste("Fig12_predictLt_", tm, ".jpg", sep = ""))
fig12_predictLt<-ggplot(data=lpredict_vbsim, aes(x=ID_ind, y=pred_lt,colour=type)) + 
                 geom_line()+xlab("ID_ind")+ylab("Predicted length")+theme_classic()+
                 facet_wrap(~year)+
                 theme(axis.title.y = element_text(size = 14),axis.title.x=element_text(size=14),
                 axis.line = element_line(size = 0.5),axis.text = element_text(size = 10))
ggsave(Fig12_predictLt)
try(dev.off(),silent=T)


Fig12a_predictLt <- file.path(paste("Fig12a_predictLt_", tm, ".jpg", sep = ""))
fig12a_predictLt<-ggplot(data=lpredict_vbsim, aes(x=ID_ind, y=pred_lt,colour=factor(year))) + 
                  geom_point()+xlab("ID_ind")+ylab("Predicted length")+theme_classic()+
                  facet_wrap(~factor(type))+
                  theme(axis.title.y = element_text(size = 14),axis.title.x=element_text(size=14),
                  axis.line = element_line(size = 0.5),axis.text = element_text(size = 10))
ggsave(Fig12a_predictLt)
try(dev.off(),silent=T)


################################################################################################################################
###############################################################################################################################
### Data from biological samples 
###############################################################################################################################
###############################################################################################################################
dados_bio_simul<- data.frame()
       for (i in c(stt$MIN_OTOL.Read:stt$MAX_OTOL.Read)){
           get<-read.table(paste0("dados_bio_",i,".csv"),sep=",", header=T)
           assign(paste0("dados_bio_", i),get)
           dados_bio_simul<- rbind(dados_bio_simul,get)
}


####################################################################################################################
####################################################################################################################
##### Data from the maturity ogive model adjustment
###################################################################################################################
####################################################################################################################
dados_mo_simul<- data.frame()
for (i in c(stt$MIN_OTOL.Read:stt$MAX_OTOL.Read)){
     get<-try(read.table(paste0("table_res_mo_",i,".csv"),sep=",", header=T),silent=T)
if (!inherits(get, "try-error")){
    assign(paste0("dados_mo_", i),get)
    dados_mo_simul<- rbind(dados_mo_simul,get)
  }
}

library(tidyr)
dados_mo_re <- dados_mo_simul %>% gather(type_mat, L_mean, L25:L75)

if (!inherits(dados_mo_re, "try-error")){
years<- sort(unique(dados_mo_re$year)) ##list of years on the samples data

for(bb in 1:length(years)){
  Figmo <- file.path(paste("FigmoLt_", years[bb], ".png", sep = ""))
  png(file=Figmo)
  plotmo<-
    ggplot(dados_mo_re[dados_mo_re$year==years[bb],], aes(x=factor(type),y=L_mean, fill=type_mat))+xlab("Number of otoliths selected by length class (cm)")+ ylab("Length")+
    geom_boxplot()+ theme_classic() +
    theme(axis.title.y = element_text(size = 14),axis.title.x=element_text(size=14),
          axis.line = element_line(size = 0.5),axis.text = element_text(size = 10))
  print(plotmo)
  dev.off()
}

###Sem outliers
for(bb in 1:length(years)){
  Figmo_sna <- file.path(paste("Figmo_snaLt_", years[bb], ".png", sep = ""))
  png(file=Figmo_sna)
  plotmo_sna<-
    ggplot(dados_mo_re[dados_mo_re$year==years[bb],], aes(x=factor(type),y=L_mean, fill=type_mat))+xlab("Number of otoliths selected by length class (cm)")+ ylab("Length")+
    geom_boxplot(outlier.shape = NA)+ theme_classic() +
    theme(axis.title.y = element_text(size = 14),axis.title.x=element_text(size=14),
          axis.line = element_line(size = 0.5),axis.text = element_text(size = 10))
  print(plotmo_sna)
  dev.off()
 }
}

##########################################################################################################
###########################################################################################################
#############################     END CODE ;)    ###########################################################
###########################################################################################################
###########################################################################################################