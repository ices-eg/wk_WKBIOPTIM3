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
##  github link: https://github.com/gonpatricia/SampleOptimRDBformat/edit/master/1_Data_exploratory_analysis_RDB.R
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
library(summarytools)

########################################################################################################
#### Files path and function source:
setwd("~/...") ##Set directory


###Biological sample data (Applied to a period of years)
data_samplebio<- load(file="input_data.rdata") #File 

#########################################################################################################
#### 1. Data Preliminary analysis: (Exploratory analysis)
#### Summary
summary(data_samplebio)

view(dfSummary(data_samplebio))

out_dir<-paste(this_dir,"output",sep="/")
setwd(out_dir)

table(data_samplebio$Length_class, data_samplebio$month) ##summary of the number of individuals by length class and month

### Number of samples by Port, year and month
nsamples_year_mes<- data_samplebio %>% group_by(Port, month, year) %>% count(date)
#write.table(nsamples_year_mes, "numbersamples_summary_WHB.csv",sep=",")

#### Length classes of the samples by Port, year and month
lengthclass_samples_year_mes<- data_samplebio %>% group_by(Port, month, year) %>% count(Length_class)
#write.table(lengthclass_samples_year_mes, "numberlengthclasses_samples_summary_MAC.csv",sep=",")

########Figure a - length distribution samples by Port by year and month
#Port<-unique(date_samplebio$Port) ## list of Ports names
year<- sort(unique(lengthclass_samples_year_mes$year)) ##list of years on the samples date

for(bb in 1:length(year))
{
  plota<- ggplot(lengthclass_samples_year_mes[lengthclass_samples_year_mes$year==year[bb],], aes(x=Length_class,y=n, colour=Port))+xlab("length")+ ylab("number of individuals")+
    geom_line()+ theme_classic() + facet_wrap(~factor(month))+
    theme(axis.title.y = element_text(size = 14),axis.title.x=element_text(size=14),
          axis.line = element_line(size = 0.5),axis.text = element_text(size = 10))
  dev.copy(png, paste(year[bb],"_length_distribution_samples_Port_year",".png",sep=""))
  print(plota)
  dev.off()
}


#### Age of the samples by Port, year and month
age_samples_year_mes<- date_samplebio %>% group_by(Port, month, year) %>% count(Age)


######## Figure b - age distribution samples by Port by year and month
Port<-unique(date_samplebio$Port) ## list of Ports names
year<- sort(unique(age_samples_year_mes$year)) ##list of years on the samples date

for(bb in 1:length(year))
{
  plotb<- ggplot(age_samples_year_mes[age_samples_year_mes$year==year[bb],], aes(x=Age,y=n, colour=Port))+xlab("age")+ ylab("number of individuals")+
    geom_line()+ theme_classic() + facet_wrap(~factor(month))+
    theme(axis.title.y = element_text(size = 14),axis.title.x=element_text(size=14),
          axis.line = element_line(size = 0.5),axis.text = element_text(size = 10))
  dev.copy(png, paste(year[bb],"_age_distribution_samples_Port_year",".png",sep=""))
  print(plotb)
  dev.off()
}


## Figura 1 - Length distribution by year
years<- year

for(nb in 1: length(years))
{
  fig1<- hist(date_samplebio$Length_class[date_samplebio$year==years[nb]],xlab="length", ylab="number of individuals", main=years[nb])
  dev.copy(png, paste(years[nb],"_length_distribution",".png",sep=""))
  nb<- nb+1
  dev.off()
}


###Figure 2 - Age distribution by year
for(nb in 1: length(years))
{
  fig2<- hist(date_samplebio$Age[date_samplebio$year==years[nb]],xlab="age", ylab="number of individuals", main=years[nb])
  dev.copy(png, paste(years[nb],"_age_distribution",".png",sep=""))
  nb<- nb+1
  dev.off()
}

#########################################################################################################
#########################################################################################################
#########################################################################################################
