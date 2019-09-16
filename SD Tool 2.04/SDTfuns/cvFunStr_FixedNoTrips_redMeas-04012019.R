# SD Tool v.3 and WKBIOPTIM SimTool v.1  were taken into account for the developement of the present tool.
# SD tool has been developed in MARE/2014/19 project and updated in MARE/2016/22 project (STREAM)

# The tool is documented in Deliverable D3.3 "Upgrade the methodological framework and tools for sampling optimization, implement and report case studies" (January 2019)

# Authors of the first version of SD Tool: F. Gontrand, T. Rouyer, N. Billet, 2016
# IFREMER, UMR MARBEC, Avenue Jean Monnet, BP 171, 34203 S` ete, France 

# Authors of the first version of WKBIOPTIM scripts: Nuno Prista (SLU, Sweden) from a subgroup work carried out during a local Workshop on Sampling Design and Optimization (Lysekil, 31 October to 4 November 2016, unpublished)
# SLU Aqua, Institute of Marine Research, Department of Acquatic Resources - Swedish University of Agricultural Sciences

# Authors of this new tool: M.T. Facchini, I. Bitetto, 2017
# COISPA Tecnologia & Ricerca, Via dei Trulli 18/20 - (Bari), Italy 

# In case of use of the tool, the Authors should be cited.
# If you have any comments or suggestions please contact the following e-mail addresses: facchini@coispa.it, bitetto@coispa.it
# SD Tool is believed to be reliable. However, we disclaim any implied warranty or representation 
# about its accuracy, completeness or appropriateness for any particular purpose.

cvFunStr_FixedNoTrips_redMeas = function(caseInd, stratification, availStrat, availSamp, CSobject, CLobjectList, sppName, nIter, loca_SH_levels) {
  
  if (FALSE) {
    caseInd= nr 
    stratification=  stratification0         
    CSobject=CS_boot_scen  
    CLobjectList=CL_boot_scen 
    loca_SH_levels = SH_levels
    availSamp = this_strata_availSamp
  }

  global_case <<- availStrat[caseInd,1:3] 

  bigSamps <-  biggestSamples[biggestSamples$Var1 == global_case$Var1 & 
                                biggestSamples$Var2 ==  global_case$Var2 & 
                                biggestSamples$Var3 ==  global_case$Var3, ] 
  
  case = global_case

  MAX_NO_TRIPS_4_BOOTSTRAP <- table_NO_TRIPS_4_BOOTSTRAP$NO_TRIPS_4_BOOTSTRAP[as.character(table_NO_TRIPS_4_BOOTSTRAP$space) == as.character(case$Var1) &
                                                                                as.character(table_NO_TRIPS_4_BOOTSTRAP$time) == as.character(case$Var2) &
                                                                                as.character(table_NO_TRIPS_4_BOOTSTRAP$technical) == as.character(case$Var3) ]
  
  print(paste("MAX_NO_TRIPS to be used in the BOOTSTRAP for sharing in strata", case$Var1, "-",case$Var2, "-",case$Var3 , ":", MAX_NO_TRIPS_4_BOOTSTRAP), quote=F)
  
  # subset by area 
  switch(col_ele_var1,

         GSA={ CLobjectList =  subset(CLobjectList, area == as.character(availStrat$Var1[caseInd]), link = TRUE) },
         Country={CLobjectList =  subset(CLobjectList, vslFlgCtry == as.character(availStrat$Var1[caseInd]), link = TRUE)},
		  ALL={CLobjectList =  CLobjectList},
         stop("Enter something that switches me!")
  )
  
  switch(col_ele_var2,
         Q = { CLobjectList = subset(CLobjectList, quarter == as.character(availStrat$Var2[caseInd])) },
         S = {  
           if (availStrat$Var2[caseInd] == 1) { CLobjectList = subset(CLobjectList, quarter == 1 | quarter == 2)


           } else { CLobjectList = subset(CLobjectList, quarter == 3 | quarter == 4) } },


         Y = { CLobjectList = CLobjectList  },

         stop("Enter something that switches me!")
  )
  
  switch(col_ele_var3,    

         lev6 = { CLobjectList = subset(CLobjectList, foCatEu6 == as.character(availStrat$Var3[caseInd]))},
         lev4 = { CLobjectList = subset(CLobjectList, foCatEu5 == as.character(availStrat$Var3[caseInd]))},
         NONE = { CLobjectList = CLobjectList},
         stop("Enter something that switches me!")
  )

  availSamp_all_species <- merge(cs_all@hh, cs_all@hl)
  
  availSamp_all_species$SampQuarter <-  quarter(availSamp_all_species$date)
  availSamp_all_species$SampSemester <- ifelse(quarter(availSamp_all_species$date) == 1 | quarter(availSamp_all_species$date) == 2, 1, 2)
  
  availSamp_all_species <- setVar123(availSamp_all_species)
  
  availSamp_all_species <- availSamp_all_species[availSamp_all_species$Var1 == global_case$Var1 & availSamp_all_species$Var2 == global_case$Var2 & availSamp_all_species$Var3 == global_case$Var3 ,]
  

  # subset by area 
  switch(col_ele_var1,

         
		 GSA={ sampPoolStr1 = data.frame(sampPoolStr = 
                 unique(availSamp_all_species$trpCode[which(availSamp_all_species$area == 
                  case$Var1)] ) , group1 = rep(1, length(unique(availSamp_all_species$trpCode[which(availSamp_all_species$area == case$Var1)] )) ))},

		 
         Country={ sampPoolStr1 = data.frame(sampPoolStr = 
                     unique(availSamp_all_species$trpCode[which(availSamp_all_species$vslFlgCtry == 
                      case$Var1)]  ) , group1 = rep(1, length(unique(availSamp_all_species$trpCode[which(availSamp_all_species$vslFlgCtry == case$Var1)]  ) ) ) )},

		
         ALL = { sampPoolStr1 = data.frame(sampPoolStr = unique(availSamp_all_species$trpCode  ), 
                                         group1 = rep(1, length(unique(availSamp_all_species$trpCode  )) ) ) },

										 
         stop("Enter something that switches me!")
  )


  
  
  switch(col_ele_var2,

  
         Q = {  sampPoolStr2 = data.frame(sampPoolStr = unique(availSamp_all_species$trpCode[which(quarter(availSamp_all_species$date) %in% case$Var2)] ), 
          group2 = rep(1, length(unique(availSamp_all_species$trpCode[which( quarter(availSamp_all_species$date) %in% case$Var2)] ))) ) },
    


	 
													   
    S = { sampPoolStr2 = data.frame(sampPoolStr = unique(availSamp_all_species$trpCode[which( ifelse(quarter(availSamp_all_species$date) == 1 | quarter(availSamp_all_species$date) == 2, 1, 2) %in% case$Var2)] ), 
	
                                    group2 = rep(1, length(unique(availSamp_all_species$trpCode[which( ifelse(quarter(availSamp_all_species$date) == 1 | quarter(availSamp_all_species$date) == 2, 1, 2) %in% case$Var2)] )))) },


				 
         Y = { sampPoolStr2 = data.frame(sampPoolStr = unique(availSamp_all_species$trpCode  ), 
                                         group2 = rep(1, length(unique(availSamp_all_species$trpCode  )) ) ) },


										 
         stop("Enter something that switches me!")



  )


  
  
  switch(col_ele_var3,    


						
         lev6 = { sampPoolStr3 = data.frame(sampPoolStr = 
                                              unique(availSamp_all_species$trpCode[which(availSamp_all_species$foCatEu6 == case$Var3)] ), 
                                            group3 = rep(1, length(unique(availSamp_all_species$trpCode[which(availSamp_all_species$foCatEu6 == case$Var3)] )) ) )

         },
         lev4 = { sampPoolStr3 = data.frame(sampPoolStr = 
                                              unique(availSamp_all_species$trpCode[which(availSamp_all_species$foCatEu5 == case$Var3)] ), 
                                            group3 = rep(1, length(unique(availSamp_all_species$trpCode[which(availSamp_all_species$foCatEu5 == case$Var3)] ) )) )
         },
         NONE = { sampPoolStr3 = data.frame(sampPoolStr = unique(availSamp_all_species$trpCode ), 
                                            group3  = rep(1, length(unique(availSamp_all_species$trpCode ) )) )
         },
         stop("Enter something that switches me!")
  )
  

  
  sampPoolStr =  merge(sampPoolStr1, merge(sampPoolStr2, sampPoolStr3, all=TRUE), all=TRUE)
  sampPoolStr$sum_groups <- rowSums(sampPoolStr[, 2:4])
  sampPoolStr <- sampPoolStr[!is.na(sampPoolStr$sum_groups), ]
  sampPoolStr <- as.character(sampPoolStr$sampPoolStr[sampPoolStr$sum_groups == 3] )
  
     print("**********************************************************************", quote=FALSE)
    print(paste("total number of trips available for the bootstrap:",length(sampPoolStr) ), quote=F)
    print("**********************************************************************", quote=FALSE)


  
  if (length(sharing_level) > 0) {
    
    switch(sharing_level,
           
           space = {SH_levels_Str <- loca_SH_levels[as.character(loca_SH_levels[, 2]) ==  as.character(global_case$Var1[1]),]},
           
           time = { SH_levels_Str <- loca_SH_levels[as.character(loca_SH_levels[, 1]) ==  as.character(global_case$Var1[1]) & 
                                               as.character(loca_SH_levels[, 4]) ==  as.character(global_case$Var3[1]),] },
           
           technical = { if (as.character(global_case$Var3[1]) != "ALL") {
             SH_levels_Str <- loca_SH_levels[as.character(loca_SH_levels[, 1]) == as.character(global_case$Var1[1]) & 
			 as.character(loca_SH_levels[, 4]) == as.character(global_case$Var3[1]),]
           } else { SH_levels_Str  <- loca_SH_levels }  },
           
           # commercial_category = { SH_levels_Str <- loca_SH_levels[as.character(loca_SH_levels[, 2]) ==  as.character(global_case$Var4[1]),]},
           stop("Enter something that switches me!")
    )
    
  } else {
       SH_levels_Str <- loca_SH_levels[as.character(loca_SH_levels$space_original) ==  as.character(global_case$Var1[1]),]
						 		 
      SH_levels_Str <- SH_levels_Str[as.character(SH_levels_Str$time_original) ==  as.character(global_case$Var2[1]),]
      SH_levels_Str <- SH_levels_Str[as.character(SH_levels_Str$technical_original) ==  as.character(global_case$Var3[1]),]
  }
  
 
  loca_bigSamps =bigSamps
  
  
  tr = CSobject@tr
  hh = CSobject@hh
  sl = CSobject@sl
  hl = CSobject@hl
  
  head(sl)
  nrow(sl)
  
  
    loca_HL = hl
  Bsamp =  loca_bigSamps 

  # any(THIS_SPE_SUBS$ commCat != "ALL")
  
  THIS_SPE_SUBS_this_stratum <-  THIS_SPE_SUBS[ as.character(THIS_SPE_SUBS$Var1) == as.character(global_case$Var1) & 
                                                  as.character(THIS_SPE_SUBS$Var2) ==  as.character(global_case$Var2) & 
                                                  as.character(THIS_SPE_SUBS$Var3) ==  as.character(global_case$Var3), ]
  
  if ( any(THIS_SPE_SUBS_this_stratum$commCat != "ALL") ) {
   
   SUBSAMPLED_SL <- merge(sl, THIS_SPE_SUBS_this_stratum, by=c("commCat"), all.x=T)
    
   SUBSAMPLED_SL$subSampWt[paste(SUBSAMPLED_SL$trpCode, SUBSAMPLED_SL$staNum, SUBSAMPLED_SL$commCat, sep="_") %in% 
                   paste(loca_bigSamps$trpCodeHaul, loca_bigSamps$commCat, sep="_") ] =  SUBSAMPLED_SL$subSampWt[paste(SUBSAMPLED_SL$trpCode,
                                   SUBSAMPLED_SL$staNum,  SUBSAMPLED_SL$commCat, sep="_") %in% 
                         paste(loca_bigSamps$trpCodeHaul, loca_bigSamps$commCat, sep="_")  ] * SUBSAMPLED_SL$factor[paste(SUBSAMPLED_SL$trpCode, 
                                     SUBSAMPLED_SL$staNum,   SUBSAMPLED_SL$commCat, sep="_") %in%  paste(loca_bigSamps$trpCodeHaul, loca_bigSamps$commCat, sep="_")  ] 

    sl <- SUBSAMPLED_SL[, c( "sampType", "landCtry", "vslFlgCtry", "year", "proj", "trpCode", "staNum", "spp",  "catchCat" , 
                             "landCat",  "commCatScl" ,"commCat",    "subSampCat" ,"sex" , "wt" , "subSampWt" , "lenCode")]
    # modified 04.01.2019 (MTF)
    asITis <- loca_HL[ as.character(loca_HL$trpCode) %in% sampPoolStr, ]
    
    asITis <- asITis[!( paste(asITis$trpCode,  asITis$staNum, asITis$commCat, sep="_") %in% paste(Bsamp$trpCodeHaul, Bsamp$commCat, sep="_")) , ]
    #

    # reduce the original HL 
    loca_cs_all <- readRDS(path_CS)
    loca_cs_all@ca$landCat = "HUC"
    loca_cs_all@ca$subSampCat = ""
    loca_cs_all@ca$catchCat <- toupper(loca_cs_all@ca$catchCat)
    
    CA_original <-  loca_cs_all@ca # read.csv(file.path(path.data, "SDEF CS-CA data.csv"), sep=";" )
    CA_original$trpCode <- paste( CA_original$trpCode, CA_original$year, sep="_")
    CA_original$year = 2999
   
    CA_original <- CA_original[paste(CA_original$trpCode, CA_original$staNum, CA_original$commCat, sep="_") %in% paste(loca_HL$trpCode, 
                                     loca_HL$staNum, loca_HL$commCat, sep="_") & 
                                 CA_original$spp ==  target_SPECIES[[current_species]], ]
    
    
    CA_reduced <-  CA_original[CA_original$recType == "", ]  # 398542 
    
    
    if (nrow(Bsamp) >0) {
      for (nr in 1:nrow(Bsamp)) {
        CA_this_sample <- CA_original[paste(CA_original$trpCode, CA_original$staNum, sep="_") == as.character(Bsamp$trpCodeHaul[nr]) & 
                                        as.character(CA_original$commCat) == as.character(Bsamp$commCat[nr]), ]
        MEAStoBEtaken =  Bsamp$noMeasSUBSAMPLED[nr] 
        CA_this_sample_reduced <- sample_n(CA_this_sample, MEAStoBEtaken)
        CA_reduced <- data.frame(rbind(CA_reduced, CA_this_sample_reduced))  # 5507
      }
      
      
      result = dplyr::group_by(CA_reduced, sampType, landCtry ,  vslFlgCtry ,year ,proj , trpCode ,   staNum, spp  ,  catchCat ,  landCat   , commCatScl,  commCat  , sex , lenCls )
      result = data.frame(dplyr::summarise(result,  lenNum = length(sampType) ) )
      
      result <- data.frame( cbind(result[,1:12], cbind(rep(NA, nrow(result)), result[,13:15]) ))
      colnames(result) <- colnames(asITis)
      
      result <- data.frame(rbind(result,asITis ))
    } else {
      result <- asITis
      
    }



  } else {
    
    loca_SUBSAMPLE_FACTOR <- THIS_SPE_SUBS_this_stratum$threshold 
      
    sl$subSampWt[paste(sl$trpCode, sl$staNum, sep="_") %in%  as.character(loca_bigSamps$trpCodeHaul) ] =  sl$subSampWt[paste(sl$trpCode, sl$staNum, sep="_") %in% 
                    as.character(loca_bigSamps$trpCodeHaul) ] * loca_SUBSAMPLE_FACTOR 

    asITis <- loca_HL[ as.character(loca_HL$trpCode) %in% sampPoolStr, ]
    asITis <- asITis[!( paste(asITis$trpCode, asITis$staNum, sep="_") %in% as.character(Bsamp$trpCodeHaul) ) , ]
    
     # reduce the original HL 
    loca_cs_all <- readRDS(path_CS)
    
    
    loca_cs_all@tr$trpCode <- paste(loca_cs_all@tr$trpCode , loca_cs_all@tr$year, sep="_")
    loca_cs_all@hh$trpCode <- paste(loca_cs_all@hh$trpCode , loca_cs_all@hh$year, sep="_")
    loca_cs_all@sl$trpCode <- paste(loca_cs_all@sl$trpCode , loca_cs_all@sl$year, sep="_")
    loca_cs_all@hl$trpCode <- paste(loca_cs_all@hl$trpCode , loca_cs_all@hl$year, sep="_")
    loca_cs_all@ca$trpCode <- paste(loca_cs_all@ca$trpCode , loca_cs_all@ca$year, sep="_")
    
    loca_cs_all@ca$landCat = "HUC"
    loca_cs_all@ca$subSampCat = ""
    loca_cs_all@ca$catchCat <- toupper(loca_cs_all@ca$catchCat)
    
    
    CA_original <-  loca_cs_all@ca # read.csv(file.path(path.data, "SDEF CS-CA data.csv"), sep=";" )
    
    # write.table(CA_original, file="CA.csv", sep=";", row.names=F)
   # write.table( loca_cs_all@hl, file="HL.csv", sep=";", row.names=F)
    
#     CA_original$trpCode <- paste( CA_original$trpCode,  CA_original$year, sep="_")
    CA_original$year = 2999
    
    CA_original <- CA_original[paste(CA_original$trpCode, CA_original$staNum) %in% paste(loca_HL$trpCode, loca_HL$staNum) & 
                                 CA_original$spp ==  target_SPECIES[[current_species]], ]
    CA_reduced <-  CA_original[CA_original$recType == "", ]  # 398542
  
 
    if (nrow(Bsamp) >0) {
      for (nr in 1:nrow(Bsamp)) {
        CA_this_sample <- CA_original[paste(CA_original$trpCode, CA_original$staNum, sep="_") == as.character(Bsamp$trpCodeHaul[nr]), ]
        MEAStoBEtaken =  Bsamp$noMeasSUBSAMPLED[nr] 
        CA_this_sample_reduced <- sample_n(CA_this_sample, MEAStoBEtaken)
        CA_reduced <- data.frame(rbind(CA_reduced, CA_this_sample_reduced))  # 5507
      }
      
      result = dplyr::group_by(CA_reduced, sampType, landCtry ,  vslFlgCtry ,year ,proj , trpCode ,   staNum, spp  ,  catchCat ,  landCat   , commCatScl,  commCat  , sex , lenCls )
      result = data.frame(dplyr::summarise(result,  lenNum = length(sampType) ) )
      
      result <- data.frame( cbind(result[,1:12], cbind(rep(NA, nrow(result)), result[,13:15]) ))
      colnames(result) <- colnames(asITis)
      
      result <- data.frame(rbind(result,asITis ))
      
    } else {
      result <- asITis
      
    }
    
    }

  # -------------------------------------------------------------
  
  # sum(result$lenNum[result $trpCode == "01_18_2015"])
  
  hl_reduced <- result 
  
  hl_reduced2 = data.frame( cbind(rep(global_case$Var1, nrow(hl_reduced)), cbind(rep(global_case$Var2, nrow(hl_reduced)) , 
                                                                                 cbind(rep(global_case$Var3, nrow(hl_reduced)), hl_reduced) )))
  colnames(hl_reduced2)[1:3] <- c( "Var1", "Var2", "Var3")
  
  bootstrapped_HL_reduced_FINAL <<- data.frame(rbind(bootstrapped_HL_reduced_FINAL, hl_reduced2 ))
  
  if (exists("res_all_iter")) rm(res_all_iter)
  
     LANDINGS_EXISTS <- TRUE
  # iterations
  for (n_it in c(1:nIter) ) {
	
	   if (LANDINGS_EXISTS) {

    loca_SH_levels_Str = SH_levels_Str 

    
    SH_levels_Str_listTrips <- vector(mode="list", length=nrow(loca_SH_levels_Str))

    
   if (length(sharing_level) >0) {
     switch(sharing_level,

            space = { 
              switch ( SD_var1, 
			  
                       GSA =  {  trips_by_level <- availSamp_all_species[, colnames(availSamp_all_species) %in% c("trpCode", "area") ]},
                       Country =  { 
                         trips_by_level <- availSamp_all_species[, colnames(availSamp_all_species) %in% c("trpCode", "vslFlgCtry") ]
                         trips_by_level <- trips_by_level[, c(2,1)]
                         },
					             ALL =  {  
                         trips_by_level <- availSamp_all_species[, colnames(availSamp_all_species) %in% c("trpCode", "area") ]
                         trips_by_level <- trips_by_level[, c(2,1)]
                         trips_by_level[,2] <- "ALL"
                         },
						 
                       stop("Enter something that switches me!")

              )
            },
            time = {
              switch ( SD_var2, 
                       
					   Q =  {  trips_by_level <- availSamp_all_species[, colnames(availSamp_all_species) %in% c("trpCode", "SampQuarter") ]},

                       
					   S =  {  trips_by_level <- availSamp_all_species[, colnames(availSamp_all_species) %in% c("trpCode", "SampSemester") ]},

                       
					   Y =  {  

                         trips_by_level <- availSamp_all_species[, colnames(availSamp_all_species) %in% c("trpCode", "year") ]
                         trips_by_level <- trips_by_level[, c(2,1)]},

						 
                       stop("Enter something that switches me!")



              )
            },
            

            technical = {  
              switch ( SD_var3,
                       
					   lev4 =  {  trips_by_level <- availSamp_all_species[, colnames(availSamp_all_species) %in% c("trpCode", "foCatEu5") ]}, 

             lev6 =  {  trips_by_level <- availSamp_all_species[, colnames(availSamp_all_species) %in% c("trpCode", "foCatEu6") ] },

                       stop("Enter something that switches me!")

              )
            },

            stop("Enter something that switches me!")


     )
     


   } else {
     trips_by_level <- availSamp_all_species[, colnames(availSamp_all_species) %in% c("trpCode", "year") ]
     trips_by_level <- trips_by_level[, c(2,1)]
     loca_SH_levels_Str$time_original <- 2999
     loca_SH_levels_Str$time <- 2999


   }

    trips_by_level_selection <- unique(trips_by_level[trips_by_level$trpCode %in% sampPoolStr,])
    
    selected_samples <- c()
 
   if ( current_species == 1 | is.null(trips_bootstrap_first_species[[paste(case$Var1,case$Var2 ,case$Var3, sep="_")]][[n_it]]) )  {
											  
	 if ( is.null(trips_bootstrap_first_species[[paste(case$Var1,case$Var2 ,case$Var3, sep="_")]][[n_it]]) ) {
         if (n_it == 1) {
           trips_bootstrap_first_species[[paste(case$Var1,case$Var2 ,case$Var3, sep="_")]] <- vector(mode="list", length=nIter)
         }   
     }				
			 

      for (ll in 1:nrow(loca_SH_levels_Str)) {
        
        if (nrow(loca_SH_levels_Str) > 1 ) {
          
        if (sharing_level == "space" ) {

          sampPoolStr_this_level <- trips_by_level_selection[as.character(trips_by_level_selection[,2]) == as.character(loca_SH_levels_Str[ll, 1]), 1 ] 
       
           } else if (sharing_level == "technical" ) {
          
          sampPoolStr_this_level <- trips_by_level_selection[as.character(trips_by_level_selection[,2]) == as.character(loca_SH_levels_Str[ll, 3]), 1 ] 
      
          } else {
           sampPoolStr_this_level <- trips_by_level_selection[as.character(trips_by_level_selection[,2]) == as.character(loca_SH_levels_Str[ll, 2]), 1 ] 
        }

        } else {
          sampPoolStr_this_level <- trips_by_level_selection[, 1 ] 
        }
        
        if (length(sampPoolStr_this_level) > 0) { 
          
          if ( nrow(loca_SH_levels_Str) > 1 & MAX_NO_TRIPS_4_BOOTSTRAP  != 0)  {
            trips_to_be_used <- sample(sampPoolStr_this_level,  as.numeric(MAX_NO_TRIPS_4_BOOTSTRAP), replace = TRUE)           
          }  else {
            trips_to_be_used <- sampPoolStr_this_level         

          }
          
          SH_levels_Str_listTrips[[ll]] <- sample( trips_to_be_used,  loca_SH_levels_Str$no_of_trips[ll], replace = TRUE)
          
          # print(paste(loca_SH_levels_Str$no_of_trips[ll], " trips of ", loca_SH_levels_Str$technical_original[ll], "selected for bootstrap iteration n°", n_it) )
          #  print(  SH_levels_Str_listTrips[[ll]]  )
          
          
          selected_samples <- c(selected_samples, SH_levels_Str_listTrips[[ll]] )
        } else {
          
          print("ATTENTION: 0 selected!")
        }
        
      }
      
        trips_bootstrap_first_species[[paste(case$Var1,case$Var2 ,case$Var3, sep="_")]][[n_it]] <<- selected_samples 
    } else {

       selected_samples <-  trips_bootstrap_first_species[[paste(case$Var1,case$Var2 ,case$Var3, sep="_")]][[n_it]]
    }

    
      print(paste("No. of samples selected for bootstrap iteration n°", n_it), quote=FALSE )

   print( length( selected_samples), quote=FALSE )

    # print(selectSamp)
    # subset CS tables accroding to selected samples

    
    sampSize = length(selected_samples)

    
    extentionList = 1 : sampSize # pas assez unque pour les trip code de spain data

       
    selectTR = do.call("rbind", lapply(1 : sampSize, select.and.rename.tripcode2, tr, extentionList, selected_samples))
    selectHH = do.call("rbind", lapply(1 : sampSize, select.and.rename.tripcode2, hh, extentionList, selected_samples))
    selectSL = do.call("rbind", lapply(1 : sampSize, select.and.rename.tripcode2, sl, extentionList, selected_samples))
    selectHL = do.call("rbind", lapply(1 : sampSize, select.and.rename.tripcode2, hl_reduced, extentionList, selected_samples))


   selectHL$subSampCat[is.na(selectHL$subSampCat)] <- ""
    
	selectSL <- selectSL[ selectSL$catchCat !=  "DIS", ]
   selectHL <- selectHL[ selectHL$catchCat !=  "DIS", ]

   no_trip_positive_to_species <- length(unique(selectSL$trpCode))
   

   # re-building the CS object

   if (no_trip_positive_to_species > 1 ) {

   # re-building the CS object
    subCS = csData(tr = selectTR,
                   hh = selectHH,
                   sl = selectSL,
                   hl = selectHL)


    head(subCS)
    
    years_for_selection <- unique(CLobjectList@cl$year) 
    # random selectection of the CLobject
    CL_ye = sample(years_for_selection, 1)
    
    if (length(years_for_selection) == 1) {
      CL_ye = years_for_selection
    }
    
    CLobject_ye = subset(CLobjectList, year == CL_ye, link = TRUE)
    
    if (nrow(CLobject_ye@cl) > 0) {
    CLobject_ye@cl$year = 2999
    
    # outputs
    nMeas = sum(subCS@hl$lenNum, na.rm = TRUE)
    
    
    CSval = csDataVal(subCS)
    CLval = clDataVal(CLobject_ye)
    
    CScons = csDataCons(CSval, stratification)
    CLcons = clDataCons(CLval, stratification) 
    
    dbeOutput = dbeObject(desc = sppName, species = sppName, catchCat = "Lan",  strataDesc = stratification, methodDesc = "analytical")
    
    raising = try(RaiseLgth(dbeOutput, CScons, CLcons, spp = sppName, taxon = sppName, strDesc = stratification), silent=TRUE)
    
    if (class(raising) == "try-error") {
      cv = NA
      ci = NA
      lenstructure = NA
      
      print("Impossible to calculate CV due a problem in COST function!", quote=FALSE)
      
    } else {   
      
      computeCv_res = list(cvLenCls = raising@lenNum$cv, cv = raising@lenNum$DCRcvIndicator, ci = raising@lenNum$ci, LenStr = raising@lenStruc[[1]])
      cv = computeCv_res$cv
      ci = computeCv_res$ci
      
      lenstructure = computeCv_res$LenStr
      
      result2 = data.frame(cbind(rep(  target_SPECIES[[current_species]] , nrow(lenstructure)), cbind(rep(n_it, nrow(lenstructure)), cbind(rep(global_case$Var1, nrow(lenstructure)), cbind(rep(global_case$Var2, nrow(lenstructure)) , cbind(rep(global_case$Var3, nrow(lenstructure)), lenstructure[,4:5]) )))))
      colnames(result2)<- c("species","iteration" , "Var1", "Var2", "Var3","length","value")
      
      LengthStr_dataframe <<- data.frame(rbind(LengthStr_dataframe,result2 ))
      
      lenCV = computeCv_res$cvLenCls
      
      result3 = data.frame(cbind(rep(  target_SPECIES[[current_species]] , nrow(lenCV)), 
                                 cbind(rep(n_it, nrow(lenCV)), 
                                       cbind(rep(global_case$Var1, nrow(lenCV)), 
                                             cbind(rep(global_case$Var2, nrow(lenCV)) , 
                                                   cbind(rep(global_case$Var3, nrow(lenCV)), 
                                                         lenCV[,4:5]) )))))
      colnames(result3) <- c("species","iteration" , "Var1", "Var2", "Var3", "length", "CV")
      
      CVbyLength_dataframe <<- data.frame(rbind(CVbyLength_dataframe, result3 ))
    
    } # end try error
    
    result = data.frame(cv, nMeas, sampSize)

}  else { # else check landing this iteration



     LANDINGS_EXISTS <- FALSE
     
     cv = NA
     ci = NA
     nMeas = NA
     result = data.frame(cv, nMeas, sampSize)
     lenstructure = NA
     lenCV = NA
   } # END check landing this iteration


  
  }   else {  # else check single trip or only discard

  
  if (no_trip_positive_to_species == 0) {
    print("Impossible to calculate CV due to only samples of discard in the stratum", quote=FALSE)
  } else {
    print("Impossible to calculate CV due to the number of trips positive to the species in the stratum (equal to 1)", quote=FALSE)


  }
  
  
  cv = NA


  ci = NA
  nMeas = NA 
  result = data.frame(cv, nMeas, sampSize)
  lenstructure = NA
  lenCV = NA

  
} # END check single trip or only discard
   

  } else {    # ELSE CHECK LANDINGS
    
    cv = NA
    ci = NA
    rr = NA
    nMeas = NA
    result = data.frame(cv, nMeas, sampSize)
    lenstructure = NA
    lenCV = NA
 
  } # END CHECK LANDINGS
  


  
   if (n_it == 1 | !exists("res_all_iter") ) {
     res_all_iter = result
   } else {
    if (LANDINGS_EXISTS) {
          res_all_iter = data.frame(rbind(res_all_iter, result))

    }





   } 
  } # end iteration
  
if (!LANDINGS_EXISTS) {
     print("Impossible to calculate CV on the raised LFD for all the iterations due to MISSING LANDINGS in the stratum!", quote=FALSE)
  } else {
    if (n_it == nIter) {
      print(paste(nIter , "iterations done!"), quote=F)
    }
  }

  return(res_all_iter)  




                
  
}