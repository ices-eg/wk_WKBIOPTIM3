# Set dir of data
dataDirectory <- 
  "C:/Users/ειρηνη/Dropbox/FRI/WK_BIOPTIM3_emantzo/BioSim Tool 1.01/WKBIOTIM2_GRData"

### OR LOAD a CONSOLIDATED CS and CL, where gaps in ALK have been filled using COST methods !!!

# load data... USER set file names
# 

filepathTR <<- paste(dataDirectory,"GRC_20142016_Q1234_TR.csv",sep="/")
filepathHH <<- paste(dataDirectory,"GRC_20142016_Q1234_HH.csv",sep="/")

filepathSL <<- paste(dataDirectory,"GRC_20142016_Q1234_SL_SP.csv",sep="/")
filepathHL <<- paste(dataDirectory,"GRC_20142016_Q1234_HL_SP.csv",sep="/")

filepathCA <<- paste(dataDirectory,"GRC_20142016_Q1234_CA.csv",sep="/")

filepathCL <<- paste(dataDirectory,"GRC_20142016_Q1234_CL.csv",sep="/")

###
### user input END
### 

# data preparation --------------------------------------------------------

tr<-read.csv(filepathTR,sep=";")
hh<-read.csv(filepathHH,sep=";")
sl<-read.csv(filepathSL,sep=";")
hl<-read.csv(filepathHL,sep=";")
ca<-read.csv(filepathCA,sep=";")

cl<-read.csv(filepathCL,sep=";")
#ce<-read.csv(filepathCE,sep=";")


#  integrity  ###OPTIONAL!!
sl0<-semi_join(sl,hh)

sl0<-semi_join(sl0,hl)

hl0<-semi_join(hl,sl0)

##
cl$foCatEu6=cl$foCatEu5

hh$foCatEu6=hh$foCatEu5





CS<-csData(desc="",tr=tr[,-1],hh=hh[,-1],sl=sl0[,-1],hl=hl0[,-1],ca=ca[,-1],check=F)

CL<- clData(desc="",cl=cl[,-1])

# CS- CL :  validation & concolidation

strD <- strIni(timeStrata="quarter", techStrata="foCatEu6")  

CS_val=csDataVal(CS)

CS_csc <- csDataCons(CS_val, strD)

##

CL_val=clDataVal(CL)

CL_clc <- clDataCons(CL_val, strD)


## get technical in CA from tripCode
CS_csc@ca$technical= gsub("\\d+", "", CS_csc@ca$trpCode)
#match to technicl in HH- CL
CS_csc@ca$technical<- paste0(substr(CS_csc@ca$technical, start = 4, stop = 6),"_DEF")

setwd(dataDirectory)
save(file="emantzo_sampalc_data.rdata",CL_clc,CS_csc,CS)

