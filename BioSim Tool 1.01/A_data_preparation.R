
# BioSim Tool 1.01 developed within STREAM (STrengthening REgional cooperation in the Area of 
#fisheries biological data collection in the Mediterranean and Black Sea) project

# SD Tool v.3 and WKBIOPTIM SimTool v.1  were taken into account for the developement of the present tool.
# SD tool has been developed in MARE/2014/19 project and updated in MARE/2016/22 project (STREAM)

# Authors of the first version of SD Tool: F. Gontrand, T. Rouyer, N. Billet, 2016
# IFREMER, UMR MARBEC, Avenue Jean Monnet, BP 171, 34203 S` ete, France 

# Authors of the first version of WKBIOPTIM scripts: Nuno Prista (SLU, Sweden) from a subgroup work carried out during a local Workshop on Sampling Design and Optimization (Lysekil, 31 October to 4 November 2016, unpublished)
# SLU Aqua, Institute of Marine Research, Department of Acquatic Resources - Swedish University of Agricultural Sciences

# Authors of this new tool based on SD tool and WKBIOPTIM SimTool: M.T. Facchini, I. Bitetto, 2017
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 
# The tool will be documented in Deliverable D3.3 "Upgrade the methodological framework and tools for sampling optimization, implement and report case studies" (January 2019)

# In case of use of the tool, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail addresses: facchini@coispa.it, bitetto@coispa.it
# BioSimTool is believed to be reliable. However, we disclaim any implied warranty or representation 
# about its accuracy, completeness or appropriateness for any particular purpose.


library(lubridate)
library(dplyr)

caseStudy_path <-"C:\\BioSim Tool 1.01\\"
dir_res <- paste(caseStudy_path, "\\A_data_preparation", sep="")

# load file
df=read.table(paste(caseStudy_path,"\\input files\\SDEF CS-CA data.csv", sep=""),sep=";",header=T)

# Inclusion of date and metier
hh=read.table(paste(caseStudy_path,"\\input files\\SDEF CS-HH data.csv", sep=""),sep=";",header=T)

# selection of mature stages made by the user
IMMATSTAGES = c("-1", "0","1","2a")

# formatting columns names
ref_tab<-read.csv2(paste(caseStudy_path,"\\input files\\A_col_names_conversion_table.csv", sep=""),sep=";",header=T)

# association of lev4 to each trip. 
metier_shortcode=read.table(paste(caseStudy_path,"\\input files\\metier_shortcode.csv", sep=""),sep=";",header=T)




dir.create(dir_res)



# reading by the SDEF CS-CA data file
df=cbind(data.frame(rep("CA",nrow(df))),df)
df$tripId=df$trpCode
df$tripId=  paste(df$trpCode,df$staNum)

colnames(df)=c("Record_type",	"Sampling_type",	"Landing_country",	"Vessel_flag_country",	"Year",	"Project",	"Trip_number",	"Station_number",	"Quarter",
             "Month",	"Species",	"Sex",	"Catch_category",	"Landing_category",	"Comm_size_cat_scale",	"Comm_size_cat",	"Stock",	"Area",	
             "Statistical_rectangle",	"Subpolygon",	"Length_class",	"Age",	"Specimen_No",	"Length_code",	"Aging_method",	"Age_plus_group",
             "Otolith_weight",	"Otolith_side",	"Weight",	"Maturity_staging",	"Maturity_scale",	"Maturity_stage",	"Trip_id")

df0<-df 

merg=merge(df,hh, by.y = c("trpCode", "staNum"),by.x=c("Trip_number","Station_number"))
df0=merg[,which(colnames(merg) %in% c(colnames(df),"date","foCatEu6"))]

# Isabella 28082018: Inclusion of quarter
df0$Quarter= quarter(df0$date)


colnames(df0) <- ref_tab$CA_Standard[match(tolower(colnames(df0)),tolower(ref_tab$Own_names))]

# removes columns not in accepted list
	# df0[,!colnames(df0) %in% ref_tab$CA_Standard] <- NULL
df0 <- df0[,colnames(df0) %in% ref_tab$CA_Standard] 

# creates columns missing	
	for (i in ref_tab$CA_Standard) {
	  
	  if (!i %in% colnames(df0)) df0[i]<-"No info"
	  
	  } 

# Column prep [project specific]
	# tweak on Sex
		df0$sex<-as.character(df0$sex)
		df0$sex[df0$sex=="-" | is.na(df0$sex)]<-NA
		df0$sex<-factor(df0$sex, exclude=NULL)
	# Tweak on maturity
		df0$matStage<-as.character(df0$matStage)
		df0$matStage[df0$matStage==""] <- NA
		df0$matStage<-as.character(df0$matStage)
	# creates mature - definition of mature according to maturity stages
		df0$mature <- NA
		
	#	for (len in 1:length(IMMATSTAGES)) {
	#	  IMMATSTAGES_this <- IMMATSTAGES[len]
	#	  df0$mature[!is.na(df0$matStage) & as.character(df0$matStage) %in% as.character(IMMATSTAGES_this[2:length(IMMATSTAGES_this)])] <- 0
	#	  df0$mature[!is.na(df0$matStage) & as.character(df0$matScale) == as.character(IMMATSTAGES_this[1]) & !as.character(df0$matStage) %in% as.character(IMMATSTAGES_this[2:length(IMMATSTAGES_this)])] <- 1
	#	}
		
		 df0$mature[!is.na(df0$matStage) & as.character(df0$matStage) %in% as.character(IMMATSTAGES)] <- 0
	 df0$mature[!is.na(df0$matStage) & !as.character(df0$matStage) %in% as.character(IMMATSTAGES)] <- 1
		
		df0$mature<-factor(df0$mature, levels=sort(unique(df0$mature)))
		df0$mature<-factor(df0$mature)

# creates sampID [adapt to your case]		
df0$sampId<-paste(df0$trpCode, df0$staNum, sep="_")
df0$sampId_cat<-paste(df0$trpCode, df0$staNum,df0$commCat, sep="_")

ls1<-split(df0, df0$sampId)
ls2<-lapply(ls1, function(x){x$indivId<-paste(x$sampId, 1:nrow(x),sep="_"); x})
ls3<-lapply(ls1, function(x){x$indivId_cat<-paste(x$sampId, x$commCat,1:nrow(x),sep="_"); x})

df0<-do.call("rbind", ls2)
df0<-do.call("rbind", ls3)

rownames(df0)<-NULL

df0$foCatEu4 = as.character(df0$foCatEu6)
df0$foCatEu6_sc = ""

for (nr in c(1:nrow(df0))) {
  if ( length(as.character(metier_shortcode$lev4[as.character(metier_shortcode$Fishing_activity_category_level6) == df0$foCatEu6[nr]]) ) >0) {
    df0$foCatEu4[nr] =  as.character(metier_shortcode$lev4[as.character(metier_shortcode$Fishing_activity_category_level6) == df0$foCatEu6[nr]])
    df0$foCatEu6_sc[nr] =  as.character(metier_shortcode$short_code[as.character(metier_shortcode$Fishing_activity_category_level6) == df0$foCatEu6[nr]])
  } else {
    print(paste("NOT FOUND: ", df0$foCatEu6[nr]))    
  } 
}


save(df0, file=paste(dir_res, "\\input_data.rdata", sep=""))		

