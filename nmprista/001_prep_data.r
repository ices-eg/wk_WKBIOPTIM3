# =========================
# Dir structure and data prep
# =========================

# Nuno Prista, 2017-2019
# adapted to HER_SD_25 dataset

	# 2018-09-10: annotation and directory structure improved
	# 2018-09-10: renamed to 001_prep_data to match new directory structure

	# wishlist
		# add versioning system to outputs
	
	rm(list=ls())
	
	# create directories
		dir.create("000_Auxiliary_Funs")
		dir.create("000_Original_Data")
		dir.create("000_Auxiliary_Tables")
		dir.create("001_Prepared_Inputs")
		dir.create("002_Exploratory_Analyses")
		dir.create("002_Exploratory_Analyses\\002_sampId_testsims")
		dir.create("002_Exploratory_Analyses\\004_sampId_barplots")
		dir.create("003_Simdata")
		dir.create("004_Results")

	# copy data to 000_Original_Data	
	# copy col_names_conversion_table to 000_Auxiliary_Tables	
	
	# load data files
		df0<-read.table("000_Original_Data\\HER_SD_25_2014-2017_share.txt", header=TRUE, sep="\t")
		ref_tab<-read.csv2("000_Auxiliary_Tables\\col_names_conversion_table.csv")

	
	# update column names to those used in script
		colnames(df0)<-ref_tab$CA_Standard[match(tolower(colnames(df0)),tolower(ref_tab$Own_names))]

	# removes columns not in accepted list
		df0<-df0[,-which(!colnames(df0) %in% ref_tab$CA_Standard)]
		
	# creates columns missing	
		for (i in ref_tab$CA_Standard)
			{if (!i %in% colnames(df0)) df0[i]<-"No info"} 

	# Column prep [project specific]
		# tweak on indWt
		df0$indWt<-as.numeric(gsub(",",".",as.character(df0$indWt)))
		# tweak on Sex
			df0$sex<-as.character(df0$sex)
			df0$sex[df0$sex=="" | df0$sex=="-" | is.na(df0$sex)]<-NA
			df0$sex<-factor(df0$sex, exclude=NULL)
		# Tweak on maturity
			df0$matStage<-as.character(df0$matStage)
			df0$matStage[df0$matStage=="#"]<-NA
			df0$matStage<-factor(df0$matStage)
		# creates mature
			df0$mature<-NA
			df0$mature[!is.na(df0$matStage) & df0$matStage %in% c(0,1,2)]<-0
			df0$mature[!is.na(df0$matStage) & !df0$matStage %in% c(0,1,2)]<-1 # CHECK with Carina
			df0$mature<-factor(df0$mature, levels=sort(unique(df0$mature)))
			#df0$mature<-factor(df0$mature)

		# creates sampID [adapt to your case]		
			df0$sampId<-paste(df0$year, df0$trpCode, sep="_")
			ls1<-split(df0, df0$sampId)
			ls2<-lapply(ls1, function(x){x$indivId<-paste(x$sampId, 1:nrow(x),sep="_"); x})
			df0<-do.call("rbind", ls2)
			rownames(df0)<-NULL
	
	# saves prepared data	
	save(df0, file="001_Prepared_Inputs\\Input_data.Rdata")		



