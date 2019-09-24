#### The script defines original data set.
#### Inputs: HL, HH and SL tables (data.hl, data.sl, data.hh), as well as selected species, country, metier, year, quarter etc., and also desired (if any) bin width = delta
#### Outputs: desired data set + 2 histogram plots (with bin width = 1 cm and bin width = delta > 1 cm)
#### Remarks: input tables can be in DATRAS format as well as alternative format 




setData <- function(data.hl, data.sl, data.hh,
selected.species, selected.country, selected.year, 
selected.quarter, selected.area, selected.metier, selected.sampType, delta=1)

{

library(ggplot2)
library(lattice)
library(latticeExtra)
library(gridExtra)
library(grid)
library(plyr)
library(FSA)
library(splitstackshape)
library(mondate)

### Rename your columns in HL, SL and HH tables
### If column names from DATRAS - rename and match

defined.hl <- c("Record_type","Sampling_type","Landing_country","Vessel_flag_country",
"Year","Project","Trip_number","Station_number","Species","Catch_category",      
"Landing_category","Comm_size_cat_scale","Comm_size_cat","Subsampling_category","Sex","Individual_sex",             
"Length_class","Number_at_length");

datras.col <- c("SamplingType", "FlagCountry", "LandingCountry", "Trip", "StationNo", "CatchCategory", "LandingCategory",
"LengthClass", "NoAtLengthInSample","StartDate","FishingActivityCategoryEuropeanLvl5","FishingActivityCategoryEuropeanLvl6",
"SubSampleWeight","LengthCode");

colnames(data.hl)[colnames(data.hl) %in% datras.col] <- c("Sampling_type","Landing_country","Vessel_flag_country",
"Trip_number","Station_number","Catch_category", "Landing_category", "Length_class","LengthCode","Number_at_length")

n.hl <- defined.hl[c(which(!is.na(match(defined.hl, names(data.hl)))))];

data.hl <- data.hl[,c(n.hl)];

defined.hh <- c("Record_type", "Sampling_type","Landing_country","Vessel_flag_country",      
"Year","Project","Trip_number","Station_number", 
"Fishing_validity","Aggregation_level","Catch_registration","Species_registration",   
"Date","Time","Fishing_duration","Pos_Start_Lat_Dec",       
"Pos_Start_Lon_Dec","Pos_Stop_Lat_Dec","Pos_Stop_Lon_Dec","Area",
"Statistical_rectangle","Sub_polygon","Main_fishing_depth","Main_water_depth",       
"FAC_National","FAC_EC_lvl5","FAC_EC_lvl6","Mesh_size",      
"Selection_device","Mesh_size_selection_device");

colnames(data.hh)[colnames(data.hh) %in% datras.col] <- c("Sampling_type","Landing_country","Vessel_flag_country",
"Trip_number","Station_number", "Date",
"FAC_EC_lvl5","FAC_EC_lvl6")

n.hh <- defined.hh[c(which(!is.na(match(defined.hh, names(data.hh)))))];

data.hh <- data.hh[,c(n.hh)];

defined.sl <- c("Record_type", "Sampling_type","Landing_country",    
"Vessel_flag_country","Year","Project",            
"Trip_number","Station_number","Species",             
"Catch_category","Landing_category","Comm_size_cat_scale","Comm_size_cat","Subsampling_category",
"Sex","Weight","Subsample_weight","Length_code");

colnames(data.sl)[colnames(data.sl) %in% datras.col] <- c("Sampling_type","Landing_country","Vessel_flag_country",
"Trip_number","Station_number","Catch_category", "Landing_category", "Subsample_weight","Length_code");

n.sl <- defined.sl[c(which(!is.na(match(defined.sl, names(data.sl)))))];

data.sl <- data.sl[,c(n.sl)];


### Extract only those columns from HL, that reqiured

data.hl <- data.hl[,c("Sampling_type","Year","Trip_number","Vessel_flag_country","Landing_country","Species",
"Station_number","Landing_category","Catch_category","Length_class","Number_at_length")];

##### Definition of a new column (ASFIS-Code). Needed if we have only WORMS-Code of Species. Alternatively, we would need an additional 
#### table with species names/WORMS-codes

data.hl$Species_code <- rep(NA,nrow(data.hl));

data.hl$Species <- as.character(data.hl$Species)

data.hl$Species_code <- ifelse((data.hl$Species==127143 | data.hl$Species=="Pleuronectes platessa"), "PLE", 
				ifelse((data.hl$Species==127160 | data.hl$Species=="Solea solea"), "SOL", 
				ifelse((data.hl$Species==126436 | data.hl$Species=="Gadus morhua"), "COD",
				ifelse((data.hl$Species==126437 | data.hl$Species=="Melanogrammus aeglefinus"), "HAD",
				ifelse((data.hl$Species==127139 | data.hl$Species=="Limanda limanda"), "DAB",
				data.hl$Species)))));


################ Final data set species consists of combination of HL, SL and HH, might be extended for CA #####

#################### HL #####################################

##### Choice of country, if any ############

if (!(missing(selected.country))) data.hl <- subset(data.hl, Vessel_flag_country %in% selected.country)

##### Choice of year, if any ############

if (!(missing(selected.year))) data.hl <- subset(data.hl, Year %in% selected.year)


##### Choice of quarter, if any ############

if (!(missing(selected.sampType))) data.hl <- subset(data.hl, Sampling_type %in% selected.sampType)

##### Binwidth ############

if (missing(delta)) delta <- 1;


##### Choice of species #################

species.hl <- subset(data.hl, Species_code %in% selected.species);

#species_hl$unique_key_hl <- paste(species_hl$Year,"_",species_hl$Trip_number,"_",species.hl$Vessel_flag_country,"_",species.hl$Station_number,"_", species.hl$Species, "_",
#species.hl$Catch_category,"_",species.hl$Landing_category);

#data.sl$unique_key_sl <- paste(data.sl$Year,"_",data.sl$Trip_number,"_",data.sl$Vessel_flag_country,"_",data.sl$Station_number,"_", data.sl$Species,
#"_",data.sl$Catch_category,"_",data.sl$Landing_category);


####################### SL #####################################

#data.sl <- subset(data.sl, unique_key_sl %in% unique(species.hl$unique_key_hl));

data.sl <- data.sl[, c("Year","Trip_number","Vessel_flag_country","Landing_country","Species","Weight","Subsample_weight","Station_number","Catch_category","Landing_category","Length_code")]

species.hl.sl <- merge(species.hl,data.sl, by=c("Year","Trip_number","Vessel_flag_country","Landing_country","Species","Station_number","Landing_category","Catch_category"), all.x=TRUE)

####################### HH #####################################

##### Choice of year, if any ############

if (!(missing(selected.year))) data.hh <- subset(data.hh, Year %in% selected.year)

##### Choice of area, if any ############

if (!(missing(selected.area))) data.hh <- subset(data.hh, Area %in% selected.area)

##### Choice of quarter, if any ############

data.hh$Quarter <- ifelse(!is.na(quarter(as.Date(data.hh$Date, format = "%d.%m.%Y"))), 
quarter(as.Date(data.hh$Date, format = "%d.%m.%Y")),
ifelse(!is.na(quarter(as.Date(data.hh$Date, format = "%d/%m/%Y"))),
quarter(as.Date(data.hh$Date, format = "%d/%m/%Y")),
quarter(as.Date(data.hh$Date, format = "%Y-%m-%d"))));

if (!(missing(selected.quarter))) data.hh <- subset(data.hh, Quarter %in% selected.quarter)

##### Choice of metier, if any ############

if (!(missing(selected.metier))) data.hh <- subset(data.hh, FAC_EC_lvl6 %in% selected.metier)

data.hh <- data.hh[,c("Year","Trip_number","Vessel_flag_country","Landing_country","Area","Station_number","Quarter","FAC_EC_lvl6")]

species <- merge(species.hl.sl, data.hh, by=c("Year","Trip_number","Vessel_flag_country", "Landing_country", "Station_number"), all.x=TRUE);

species <- subset(species, !is.na(Quarter))

######## Measured vs caught ################





###### Data cleaning ##################################

species$Subsample_weight <- as.numeric(as.character(species$Subsample_weight));
species$Weight <- as.numeric(as.character(species$Weight));
species$Number_at_length <- as.integer(as.character(species$Number_at_length));
species$Length_class <- as.integer(as.character(species$Length_class));

species$LengthClass_cm <- round(as.numeric(species$Length_class)/10);

species <- subset(species, Length_class!=0);

species$Subsample_weight <- ifelse(species$Subsample_weight==0, species$Weight, species$Subsample_weight);

species$NoAtLength_Raised <- species$Number_at_length*species$Weight/species$Subsample_weight;

species$NoAtLength_Raised <- ifelse(species$NoAtLength_Raised==0, 1, species$NoAtLength_Raised);

species$NoAtLength_RaisedNormed <- species$NoAtLength_Raised;


################ Extracting data: one row = one fish  ################################

species.extracted <- expandRows(species, "NoAtLength_RaisedNormed")

species.extracted$NoAtLength_RaisedNormed <- 1;

species <- species.extracted;

################ Plot ###################

M <- max(as.numeric(species$LengthClass_cm));

U <- c();

for (k in 1:length(selected.year))
{
U1 <- data.frame(selected.year[k], table(factor(subset(species, Year==selected.year[k])$LengthClass_cm, levels=seq(0, M, by=1))));
names(U1) <- c("year","LengthClass_cm", "freq");
U <- rbind(U,U1)
}

r <- max(U$freq)


dev.new();

p <- ggplot(species, aes(x=LengthClass_cm)) + 
facet_wrap(~Year) +
geom_histogram(data = species, aes(x = LengthClass_cm), binwidth = 1, boundary=0, closed="left",  colour="black",fill="turquoise") + 
xlim(0,M)+
ylim(0,r)+
labs(title="LFD", x="LENGTH", y = "COUNT") +
theme(plot.title = element_text(face="bold",size=28, hjust = 0.5), axis.title = element_text(face="bold",size = 20), axis.text = element_text(face="bold", size = 20), strip.text = element_text(size=16, face="bold.italic")) +
ggtitle(paste("LFD (raised to whole catch):", paste(unique(species$Species_code)), ", ", "EU State: ", paste(sort(unique(species$Vessel_flag_country)), collapse="/"), ", Area: ",  paste(sort(unique(species$Area)), collapse="/"), ", Quarter: ",  paste(sort(unique(species$Quarter)), collapse="/"), ", Year: ", paste(unique(species$Year),collapse="/")))

plot(p)

if (delta > 1)
{
species$LengthClass_bin <- lencat(species$LengthClass_cm,w=delta,startcat=0);

Mbin <- max(as.numeric(species$LengthClass_bin));

Ubin <- c();

for (k in 1:length(selected.year))
{
U2 <- data.frame(selected.year[k], table(factor(subset(species, Year==selected.year[k])$LengthClass_bin, levels=seq(0, M, by=delta))));
names(U2) <- c("year","LengthClass_cm", "freq");
Ubin <- rbind(Ubin,U2)
}

rbin <- max(Ubin$freq)

dev.new();

pbin <- ggplot(species, aes(x=LengthClass_cm)) + 
facet_wrap(~Year) +
geom_histogram(data = species, aes(x = LengthClass_bin), binwidth = delta, boundary=0, closed="left",  colour="black",fill="turquoise") + 
xlim(0,Mbin)+
ylim(0,rbin)+
labs(title="LFD", x="LENGTH", y = "COUNT") +
annotate(geom="text", x=6*Mbin/7, y=9*rbin/10, label=as.character(paste("bin width = ", delta)), color="blue",size=15) +
theme(plot.title = element_text(face="bold",size=28, hjust = 0.5), axis.title = element_text(face="bold",size = 20), axis.text = element_text(face="bold", size = 20), strip.text = element_text(size=20, face="bold.italic")) +
ggtitle(paste("LFD (raised to whole catch):", paste(unique(species$Species_code)), ", ", "EU State: ", paste(sort(unique(species$Vessel_flag_country)), collapse="/"), ", Area: ",  paste(sort(unique(species$Area)), collapse="/"), ", Quarter: ",  paste(sort(unique(species$Quarter)), collapse="/"), ", Year: ", paste(sort(unique(species$Year)),collapse="/")))

plot(pbin)
}

output.data <- species;

}









