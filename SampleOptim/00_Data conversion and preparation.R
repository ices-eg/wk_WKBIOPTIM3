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
##  github link: https://github.com/gonpatricia/SampleOptimRDBformat/00_Data conversion and preparation.R
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

this_dir<-"C:/Users/~"

### Set directory for data 

setwd(paste(this_dir,"data",sep="/")) 

### Input settings file

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

####### Read in CA and tranform to SampleOptim input data file:

### Dataset with biological sample data (Applied to a period of years)
ca<- read.table("CA.csv",sep=";", header=T)

### R-code on the next lines is from Eirini Mantzouni
ca$date = paste("1",ca$month,ca$year,sep = "/") 
ca$COD_FAO <- NA 
data_samplebio1 <- ca[,c("fishId","date","month","year","COD_FAO","subRect","lenCls","indWt","sex","matStage","age","spp","area")]

names(data_samplebio1) <- c("ID_BIO_FISH","date", "month" , "year", "COD_FAO","Port", "Length_class" ,"Weight" ,  "Sex","Maturity_stage" ,"Age" , "SPECIES","GSA")

data_samplebio<- data_samplebio1 %>% filter(SPECIES==stt$species & GSA==stt$AREA) ##GSA is the area in the Mediterranean

save(data_samplebio,stt, file= "input_data.rdata")

##############################################################################################################################
##############################################################################################################################
