##
## Get age structure of landings for varying no. of age readings/ LC
##

ptm <- proc.time()

# load Libraries
library(dplyr)
library(tidyr)
library(COSTcore)
library(COSTdbe)
library(sampling)
library(purrr)
library(ggplot2)

library(ggforce)

library(parallel)

library(parallelsugar)

library(statip) ## for hellinger::
# https://www.rdocumentation.org/packages/statip/versions/0.2.0/topics/hellinger
#
library(provenance) # kolmogorov-smirnoff dist. ::
#https://rdrr.io/github/pvermees/provenance/man/KS.diss.html

library(seewave) # for Kullback-Leibler distance::
# http://rug.mnhn.fr/seewave/HTML/MAN/kl.dist.html

library(emdist) # for emd

set.seed(696)

## source custom functions

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("fun_emantzo2.R")

# ATTENTION!!! # BEFORE STARTING RUN THE CODE FIX THE BUG IN THE COST FUNCTION RUNNING
# THE FOLLOWING CODE:
fixInNamespace(spdAgreg, pos = "package:COSTdbe")
# WHEN THE WINDOWS APPEARS, replace the line 8 with the following code:
# val <- paste(unlist(BY), collapse = ":-:")


# USER INPUT ---------------------------------------------------------

# Select species
sppName <- "Mullus barbatus"

# select area
area = "GSA22"

years_AgStr <- c(2014, 2016) # years for  age struc

years_ca <- c(2014:2016) # years for CA data (to sample ALK)

GEAR <-
  c("OTB_DEF", "GTR_DEF", "GNS_DEF") # select gears for age struc ... for all use "ALL", as they appear in the sel.gear.col

sel.gear.col <-
  "foCatEu6" # column for GEAR subset - should be the same as in technical Strata in strD below

# proportion of new age.read to current no. (applied to all LCs)
props = seq(0.3, 0.9, by = 0.2)

# set stratification for Age structure
strD <- strIni(timeStrata = "quarter", techStrata = sel.gear.col)

## AGE sampling stritification
## ###

##
techStr_age = F ## if the age data collection is stratified  by the techStrata in strD , set this to TRUE (otherwise FALSE)

timeStr_age = F ## if the age data collection is  stratified   by the timeStrata in strD , set this to TRUE (otherwise FALSE)

age_by_sex = F # is age sampling stratified for sex? (T- F)

type = "fixedK" # If the protocol was to collect n otoliths per length class, set to "fixedK". If the protocol was to collect one otolith every n fish measured, set it to "propK"

###
###

n.sim = 10 # no. simulations for each scenario(prop)

plott = T # produce plots?

# Set dir of data
dataDirectory <-
  "C:/Users/ειρηνη/Dropbox/FRI/WK_BIOPTIM3_emantzo/BioSim Tool 1.01/WKBIOTIM2_GRData"


# Path to save results (a special folder will be created)
path_rez <- "C:/Users/ειρηνη/Dropbox/FRI/WK_BIOPTIM3_emantzo/emantzo"

###  LOAD a CONSOLIDATED CS and CL, where gaps in ALK may (optionally!)have been filled using COST methods !!!

# load data...
#
load(file = paste(dataDirectory, "emantzo_sampalc_data.rdata", sep = "/"))

####### user input end
#######
#######


# ANALYSES ----------------------------------------------------------------


# subset ca for years

CS_csc@ca <-
  CS_csc@ca %>% filter(grepl(paste(years_ca, collapse = "|"), time))

##

# subset CS - CL for years, spp, area, gear

CS_csc <-  subsetSpp(CS_csc, spp %in% sppName, link = TRUE)
CS_csc <- subset(CS_csc, area == area, table = "hh", link = TRUE)


CS_csc <-
  subset(CS_csc, grepl(paste(years_AgStr, collapse = "|"), time),  table =
           "hh", link = FALSE) # not applied to CA

#  gear
if (GEAR[1] != "ALL") {
  CS_csc <- subset(CS_csc,
                   technical %in% GEAR,
                   table = "hh",
                   link = F)
}



## AGE STRata

colsLog = c(T, techStr_age,timeStr_age, age_by_sex)
age_strat = c("lenCls", "technical", "time", "sex")[colsLog]

#path to save plots
path = paste(
  paste0(path_rez, "/"),
  sppName,
  paste(years_AgStr, collapse = "_"),
  paste(GEAR, collapse = "-"),
  paste(age_strat, collapse = "-"),
  sep = "_"
)

dir.create(path)
##

## Table summary age readings by AGE_STR
otol_by_lc.out <-
  CS_csc@ca %>% filter(!is.na(age)) %>% dplyr::group_by_at(vars(one_of(age_strat, "time"))) %>% summarize(n =  n()) %>% spread_(key = "time", value = "n", fill = 0)

write.table(
  otol_by_lc.out,
  file = paste(path, "oto.LC.bystr.csv", sep = "/"),
  row.names = FALSE,
  sep = ";"
)


# run analyses... ------------------------------------

## current AGE struc. FUN for mapply

ff1=function(sppName,  area,  CS_csc = CS_csc, CL_clc = CL_clc, year1 , 
             timeStr_age,  techStr_age,  type){
  
  age.str.curr <-
    age.str(
      sppName,
      area,
      CS_csc = CS_csc,
      CL_clc = CL_clc,
      year1 ,
      timeStr_age,
      techStr_age,
      type
    )
  
  x1 = age.str.curr@ageNum$ci
  x2 = age.str.curr@ageNum$cv
  
  # summarize results in df
  age.str.curr.df1 <-
    full_join(x1, x2, by = c("time", "space", "technical", "age")) %>% dplyr::rename(
      cv.cur = value.y,
      age_estim.cur = value.x,
      up.ci.cur = inf,
      low.ci.cur = sup
    )
  
  return(age.str.curr.df1)
}

age.str.curr.df1=mapply(ff1, year1=years_AgStr, MoreArgs =list(
                      sppName,
                       area,
                       CS_csc,
                       CL_clc ,
                       timeStr_age,
                       techStr_age,
                       type), 
                      SIMPLIFY = F)

age.str.curr.df= do.call(rbind.data.frame, age.str.curr.df1)%>%drop_na(age_estim.cur)



## Get age structure by Scenarion (new no. of age.read/ LC)
## 
## hack mcmapply to work with Windows
 source("mcmapply_hack.R")


# ff2=function(n.sim,  df_csc = CS_csc,df_clc = CL_clc,  prop, sppName = sppName,
#              area = area,    age_strat,   year1 ,  timeStr_age,  techStr_age,
#              type){
# 
#   print(year1)
#   print(prop)
#   
# 
# rez1 <- rerun(
# n.sim,
# age.str.newCA(
#   df_csc = CS_csc,
#   df_clc = CL_clc,
#   prop=prop,
#   sppName = sppName,
#   area = area,
#   age_strat,
#   year1 = year1,
#   timeStr_age= timeStr_age,
#   techStr_age=techStr_age,
#   type=type)
# 
# )
# rez2 = bind_rows(rez1, .id = "run") %>% mutate(prop.age = prop)
# 
# return(rez2)
# }
# 
# 
# system.time(
#   rez2 <-
#     mcmapply1(
#       FUN = ff2,
#       year1 = sort(rep(years_AgStr, length(props))),
#       prop = rep(props, length(years_AgStr)),
#       MoreArgs = list(
#         n.sim,
#         df_csc = CS_csc,
#         df_clc = CL_clc,
#         sppName = sppName,
#         area = area,
#         age_strat,
#         timeStr_age,
#         techStr_age,
#         type
#       ),
#       SIMPLIFY = F,
#       mc.cores = detectCores()
#     )
# )
#   
#   rez= do.call(rbind.data.frame, rez2)%>%drop_na(age_estim)
#   
  
  

  ## make all combin. for year1, prop, sim
  aa=expand.grid(props,years_AgStr,1:n.sim)
  
  system.time(
    rez2x <-
      mcmapply1(
        FUN = age.str.newCA,
  
        year1 = aa[,2],
        prop = aa[,1],
        sim=aa[,3],
        MoreArgs = list(
          df_csc = CS_csc,
          df_clc = CL_clc,
          
          sppName = sppName,
          area = area,
          age_strat,
         
          timeStr_age= timeStr_age,
          techStr_age=techStr_age,
          type=type),
       
        SIMPLIFY = F,
        mc.cores = detectCores()
      )
  )
  
  rez= do.call(rbind.data.frame, rez2x)%>%drop_na(age_estim)

#####
###### combine results by age.read/LC (rez)  with current in 1 df (rezz)
rezz <-
  inner_join(rez, age.str.curr.df, by = c("time", "space", "technical", "age"))


  
# rezz$compare_cv<- 1-rezz$cv/rezz$cv.cur
#
# rezz%>%group_by(prop.age,age)%>%summarize(mean(compare_cv,na.rm=T))

# PLOTS -------------------------------------------------------------

# plot current age struct.and copare with new for each no. of age.read/LC

if (plott) {
  rezz = rezz %>% mutate(age = as.numeric(as.character(age)))
  
  age.str.curr.df = age.str.curr.df %>% rename(age_estim = age_estim.cur) %>%
    mutate(prop.age = 1, age = as.numeric(as.character(age)))
  
  
  npage = round(dim(rezz %>% select(time, technical) %>% unique())[1] / 4, 0)
  
  pdf(file = paste0(path, "/p00", ".pdf"))
  
  for (i in 1:npage) {
    print(
      ggplot(data = rezz, aes(
        x = age,
        y = age_estim,
        group = as.factor(prop.age)
      )) + facet_wrap( ~ time + technical, scales = "free") +
        
        geom_point(data = rezz, aes(color = as.factor(prop.age))) +
        
        geom_line(data = age.str.curr.df, aes(x = age, y = age_estim)) +
        
        geom_errorbar(
          data = age.str.curr.df,
          aes(ymin = low.ci.cur, ymax = up.ci.cur),
          width = .1
        ) +
        
        theme(legend.position = "bottom") +
        facet_wrap_paginate(
          ~ time + technical,
          scales = "free",
          ncol = 2,
          nrow = 2,
          page = i
        )
      
    )
  }
  
  graphics.off()
  
  
  pdf(file = paste0(path, "/p01", ".pdf"))
  
  for (i in 1:npage) {
    print(
      ggplot(data = rezz, aes(
        x = as.factor(age),
        y = age_estim,
        fill = as.factor(prop.age)
      )) + facet_wrap( ~ time + technical, scales = "free") +  geom_boxplot(width =
                                                                              1.3, position = "dodge") +
        
        geom_line(data = age.str.curr.df, aes(x = as.factor(age), y = age_estim)) +
        
        geom_errorbar(
          data = age.str.curr.df,
          aes(ymin = low.ci.cur, ymax = up.ci.cur),
          width = .4,
          size = 1.7
        ) +
        
        theme(legend.position = "bottom") +
        facet_wrap_paginate(
          ~ time + technical,
          scales = "free",
          ncol = 2,
          nrow = 2,
          page = i
        )
      
    )
  }
  
  graphics.off()
  
}



# Distance Stats ----------------------------------------------------------

# estimate dist statistics between true and optimized (??) age str

rezz1 = rezz %>% mutate(str = paste0(time, technical))

dist_DF = data.frame()

# j = unique(rezz1$str)[1]
# i = unique(rezz1$prop.age)[3]

for (j in unique(rezz1$str)) {
  rezz22 = rezz1 %>% filter(str == j)
  
  print(j)
  for (i in unique(rezz1$prop.age)) {
    rezz2 = rezz22 %>% dplyr::filter(prop.age == i)
    
    print(i)
    tryCatch({
      # current age.str
      cur = rezz2 %>% dplyr::select(age, age_estim.cur) %>% unique() %>% mutate(run =
                                                                                  -1)
      
      # simulated age. str for reduced age.read
      sim = rezz2 %>% dplyr::select(age, age_estim, run) %>% unique()
      
      # join cur -sim
      rc = left_join(cur, sim, by = "age") %>% na.omit()
      
      
      # emd
      tryCatch({
        emd = lapply(
          1:n.sim,
          FUN = function(x) {
            cc = emd(
              data.matrix(
                rc %>% filter(run.y == x) %>% select(age, age_estim.cur)
              ),
              data.matrix(rc %>% filter(run.y == x) %>% select(age, age_estim))
            )
            return(cc)
          }
        )
        
        emd.df = do.call(rbind.data.frame, emd) %>% mutate(
          run = seq(1, n.sim),
          str = j,
          prp = i,
          stat = "EMD"
        )
        
        names(emd.df)[1] = "STTS"
        
      }, error = function(e) {
        cat("ERROR :", conditionMessage(e), "\n")
      })
      
      
      
      # kolmogorov-smirnoff
      tryCatch({
        ks = lapply(
          1:n.sim,
          FUN = function(x)
            KS.diss(rc$age_estim.cur[rc$run.y == x], rc$age_estim[rc$run.y == x])
        )
        
        ks.df = do.call(rbind.data.frame, ks) %>% mutate(
          run = seq(1, n.sim),
          str = j,
          prp = i,
          stat = "KS"
        )
        
        names(ks.df)[1] = "STTS"
      }, error = function(e) {
        cat("ERROR :", conditionMessage(e), "\n")
      })
      
      
      tryCatch({
        # Hellinger
        hel = lapply(
          1:n.sim,
          FUN = function(x)
            hellinger(
              rc$age_estim.cur[rc$run.y == x],
              rc$age_estim[rc$run.y == x],
              lower = 0,
              upper = Inf,
              method = 2
            )
        )
        
        hel.df = do.call(rbind.data.frame, hel) %>% mutate(
          run = seq(1, n.sim),
          str = j,
          prp = i,
          stat = "HEL"
        )
        
        names(hel.df)[1] = "STTS"
        
      }, error = function(e) {
        cat("ERROR :", conditionMessage(e), "\n")
      })
      
      ## Kullback-Leibler distance
      
      tryCatch({
        kull = lapply(
          1:n.sim,
          FUN = function(x)
            kl.dist (
              data.matrix(rc %>% filter(run.y == x) %>% select(age, age_estim.cur)),
              data.matrix(rc %>% filter(run.y == x) %>% select(age, age_estim))
            )
        )
        
        
        kull.df = do.call(rbind.data.frame, kull) %>% select(D) %>% mutate(
          run = seq(1, n.sim),
          str = j,
          prp = i,
          stat = "KUL"
        )
        
        names(kull.df)[1] = "STTS"
        
        dist_df = bind_rows(kull.df, emd.df, hel.df, ks.df)
      }, error = function(e) {
        cat("ERROR :", conditionMessage(e), "\n")
      })
      
      #
      # # wilcoxon
      # wil = lapply(
      #   1:n.sim,
      #   FUN = function(x)
      #     wilcox.test(rc$age_estim.cur, rc$age_estim[rc$run.y == x])
      # )
      #
      # wil.df <-
      #   data.frame(matrix(unlist(wil), nrow = length(wil), byrow = T))[, 2]
      # wil.df = data.frame(wil.df) %>% mutate(run = seq(1, n.sim),
      #                                        str = j,
      #                                        prp = i)
      #
      # names(wil.df)[1] = c("wilc.p")
      
      
      dist_DF = bind_rows(dist_df, dist_DF)
      
      
    }, error = function(e) {
      cat("ERROR :", conditionMessage(e), "\n")
    })
  }
  
}

dist_DF2 = dist_DF %>% group_by(str, prp, stat) %>% summarise(STS = mean(as.numeric(STTS), na.rm = T)) %>% group_by(str, stat) %>%
  mutate(MM = max(STS, na.rm = T), STS.norm = STS / MM)# rescale to max

## PLOT

if (plott) {
  pdf(file = paste0(path, "/Dist_by_str", ".pdf"))
  for (i in 1:npage) {
    print(
      ggplot(dist_DF2, aes(prp, STS.norm, col = stat)) +
        geom_line() +
        stat_smooth() +
        
        facet_wrap_paginate(
          ~ str,
          scales = "free_y",
          ncol = 2,
          nrow = 2,
          page = i
        )
      
    )
  }
  
  graphics.off()
  
  
  
  pdf(file = paste0(path, "/Dist_by_Stat", ".pdf"))
  print(
    ggplot(data = dist_DF2, aes(
      x = prp,
      y = STS.norm,
      group = str,
      colour = str
    )) +
      geom_line() +
      stat_smooth() +
      geom_point(
        size = 2,
        shape = 21,
        fill = "white"
      ) +
      scale_x_continuous(name = "prop") +
      facet_grid(stat ~ ., scales = "free")
  )
  graphics.off()
  
}

graphics.off()

print(proc.time() - ptm)

#######
####### DRAFT EMDomics
#
# # https://www.imsbio.co.jp/RGM/R_rdfile?f=EMDomics/man/plot_emd_density.Rd&d=R_BC
# ## TO DO!!! : close lo.. use only ks!
# library(EMDomics)
# # 100 genes, 100 samples
#
# rezz1 = rezz %>% mutate(str = paste0(time, technical))
# rezx = rezz1 %>% dplyr::select(run, prop.age, str, age, age_estim) %>% mutate(rp =
#                                                                                 paste(run, prop.age, sep = "_")) %>% dplyr::select(-one_of(c("run", "prop.age"))) %>%
#   spread(key = rp, value = age_estim, fill = 0)
#
# for (j in unique(rezx$str)) {
#   rezx1 = rezx %>% filter(str == j) %>% dplyr::select(-str)
#   rownames(rezx1) = paste("age", rezx1$age, sep = "-")
#
#   outc = substr(colnames(rezx1), 3, 5)
#   names(outc) <- colnames(rezx1)
#   outc = outc[-1]
#
#   rezx2 =  rezx1 %>% dplyr::select(-one_of(c("age")))
#
#   rezxm = as.matrix(rezx2)
#
#   results <- calculate_ks(rezxm, outc, nperm = 10, parallel = T)
#   # results <- calculate_emd(rezxm, outc, nperm=10, parallel=T) ... not working...
#
#   plot_emd_density(results, "gene5")
# }



######## the ###########
######## end ###########
