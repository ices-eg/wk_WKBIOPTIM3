
# source this file for sampalk_CS_CL_2.R

# FUNCTIONS ----------------------------------------------------------


# function to estimate new age structure for new CA (used for rerun)
# returns age_estim, ci,cv (data frame)

age.str.newCA<- function(df_csc,df_clc, prop,sppName=sppName,area=area,age_strat,year1,timeStr_age,techStr_age,type,sim=1){
  
  
  # df_csc=CS_csc;df_clc=CL_clc;prop= props[i];sppName=sppName;area=area;age_strat;year1=j;timeStr_age;techStr_age;type
  
  ca.new <- CA.resamp(dfca=df_csc@ca,age_strat,prop,year1) 
  
  df_csc@ca<-ca.new
 
  age.str.new<-age.str(sppName,area,df_csc,df_clc, year1,timeStr_age,techStr_age,type)
 
  x1=age.str.new@ageNum$ci
  x2=age.str.new@ageNum$cv
  
  
  xx<-full_join(x1,x2,by=c("time","space", "technical","age"))%>%rename(cv=value.y,age_estim=value.x)%>%mutate(run=sim,prop.age=prop)
  
  
  return(xx)
}

# age.str: LAN age- structure FUN ------------------------------

age.str<- function(sppName=sppName,area=area,CS_csc,CL_clc,year1,timeStr_age,techStr_age,type){
  

    CS_csc1<- subset(CS_csc,grepl(year1,time),  table="tr",link=T)
  
  # length str.
  
  lanEstim <- dbeObject(desc=paste(sppName,area),species=sppName,catchCat="LAN",strataDesc=strD, methodDesc="analytical")
  
  lanEstim <- RaiseLgth(lanEstim, CS_csc1, CL_clc)
  
  lanEstim <-
    dbeCalc(
      lanEstim,
      type = "CI",
      probs = c(0.025, 0.975),
      update = TRUE
    )
  

  # age str.
  if (all(techStr_age,timeStr_age)) {
    # startified by technical AND TIME
    
    print("RaiseAge_byGearTime")
    
    lanEstim <- RaiseAge_byGearTime  (dbeOutput=lanEstim, csObject=CS_csc1, type = type)
  } else{
    if (!timeStr_age) {
      #  If NOT stratified for time, use this FUN which can allow ALSO for gear start (if techStr_age=T )
      print("RaiseAge_NoTimeGearOpt")
      lanEstim <- RaiseAge_NoTimeGearOpt(dbeOutput=lanEstim, csObject=CS_csc1, type = type,gear_str=techStr_age)
       
      
    } else{ # startified by TIME only (not gear) .. use the default!
      
      print("RaiseAge")
      lanEstim <- RaiseAge(lanEstim, CS_csc1, type = "fixed")
    }
  }
  
  lanEstim <- dbeCalc(lanEstim,type="CI", vrbl="a", update=TRUE)
 # lanEstim <- dbeCalc(lanEstim,type="CV", vrbl="a", update=TRUE)

  return(lanEstim)
}


# CA.resamp ---------------------------------------------------------------

# resample CA to keep less age readings per lenCLs.. FUN ---------

#  n_vector --> vector with no. of age.red by LC .. estimated as proportion of the current (=otol_by_lc)

CA.resamp<-function(dfca=CS_csc@ca,age_strat,prop,year1){
  
  # dfca=CS_csc@ca
  # year1=2014
  # prop=0.4
  
  df11=dfca%>%filter(grepl(year1,time))%>%
    filter(!is.na(age)) %>%group_by_at(vars(one_of(age_strat)))%>%summarise(n_samp=round(ceiling(n()*prop),0)) %>%drop_na(n_samp) # calc. number of resample by age_strat
  
  #  # 
  df1=dfca%>%
    filter(!is.na(age)) %>%# keep only age data
mutate(time=paste0(year1, " - ",substr(time,8,9)))%>% # pool years, by renaming to year1
    left_join(df11)%>%drop_na(n_samp)
  
  df33=df1 %>%
    group_by_at(vars(one_of(age_strat)))%>%
    nest() %>%          # nest data
    mutate(v = map(data, ~sample_n(data.frame(id=.$fishId), unique(.$n_samp)))) %>%  # sample using id values and (unique) frq value
    unnest(v)           # unnest the sampled values
  
  
  df3=dfca%>%filter(fishId %in% df33$id)
   

    return(df3)
}



## load all funs of Costdbe
r <- unclass(lsf.str(envir = asNamespace("COSTdbe"), all = T))
# for(name in r) eval(parse(text=paste0(name, '<-COSTdbe:::', name)))
eval(parse(text=paste0("spdAgreg", '<-COSTdbe:::', "spdAgreg")))


# RaiseAge_NoTimeGearOpt ---------------------------------------------------------

### Modified RaiseAge () FUN of COSTdbe
### Raise Age using one common ALK for all time strata (eg quarters)
### and optionally by Gear (if gear_str=T)
### if gear_str=F, the function works like RaiseAge_NoTime
###  USeful if annual age data collection is not time -stratified 
### (the original function would use time-specific ALK)

RaiseAge_NoTimeGearOpt=function (csObject, dbeOutput, type = "p", sex = as.character(NA),gear_str) 
{
  sp <- dbeOutput@species
  ca <- ca(csObject)
  ca <- ca[ca$spp %in% sp, ]
  if (nrow(ca) == 0) 
    stop("no CA data for specified species in input object!!")
  Unit <- paste(ca$PSUid, ca$SSUid, sep = ":-:")
  nSAMP <- spdAgreg(list(value = Unit), BY = list(time = ca$time, 
                                                  space = ca$space), function(x) length(unique(x)))
  dbeOutput@nSamp$age <- nSAMP
  if (!(all(is.na(sex)))) {
    ca <- ca[ca$sex %in% sex, ]
    if (nrow(ca) == 0) 
      stop("no CA data for specified sex in input object!!")
  }
  CAreal <- ca[ca$fishId > 0, ]
  nMEAS <- spdAgreg(list(value = CAreal$age), BY = list(time = CAreal$time, 
                                                        space = CAreal$space), function(x) sum(!is.na(x)))
  dbeOutput@nMeas$age <- nMEAS
  ###!!
  if (gear_str){ # if stratified for gear
    Ldf <-    dbeOutput@lenStruc$estim %>% mutate_if(is.factor, as.character) %>% semi_join(ca %>% filter(!is.na(age))) #!!
  }else{ # if NOT stratified for gear
  Ldf <- dbeOutput@lenStruc$estim
  }
  N <- tapply(Ldf$value, list(length = Ldf$length, time = Ldf$time, 
                              space = Ldf$space, technical = Ldf$technical), sum, 
              na.rm = TRUE)
  ALK <- tapply(ca$age, list(length = factor(ca$lenCls, levels = dimnames(N)[[1]]), 
                             age = ca$age,
                             # time = factor(ca$time, levels = dimnames(N)[[2]]),#!!
                             space = factor(ca$space, levels = dimnames(N)[[3]])), 
                length)
  ALK[is.na(ALK)] <- 0
  ll <- dimnames(ALK)
  ll[["technical"]] <- dimnames(N)[[4]] 
  ll[["time"]] <- dimnames(N)[[2]]##!!
  ALK <-
    array(
      rep(as.vector(ALK), dim(N)[4] * dim(N)[2]),##!!
      dim = c(dim(ALK),
              dim(N)[4], dim(N)[2]),##!!
      dimnames = ll
    )
  if (all(is.na(dbeOutput@lenStruc$estim))) 
    stop("estimates for length structure are missing in 'dbeOutput' object!!")
  nj <- tapply(ca$lenCls, list(
    length = factor(ca$lenCls,
                    levels = dimnames(N)[[1]]),
    # time = factor(ca$time, levels = dimnames(N)[[2]]),#!!
    space = factor(ca$space, levels = dimnames(N)[[3]])
  ),
  function(x)
    sum(!is.na(x)))
  nj[is.na(nj)] <- 0
  ll2 <- dimnames(nj)
  ll2[["technical"]] <- dimnames(N)[[4]]
  ll2[["time"]] <- dimnames(N)[[2]]##!!
  nj <-
    array(
      rep(as.vector(nj), dim(N)[4] * dim(N)[2]),##!!
      dim = c(dim(nj),
              dim(N)[4], dim(N)[2]),##!!
      dimnames = ll2
    )
  Nl <- apply(nj, 2:4, sum)
  ns <- apply(ALK, 3:5, sum)
  nl <- apply(nj, 2:4, function(x) sum(x > 0))
  Qij <- aperm(aperm(ALK, c(1, 3, 4, 5, 2))/as.vector(apply(ALK, 
                                                            c(1, 3:5), sum)), c(1, 5, 2:4))
  if (type == "fixedK") {
    njStar <- ns/nl
  }  else {
    njStar <- nj * rep(as.vector(ns/Nl), each = dim(nj)[1])
  }
  lj <- N/rep(as.vector(apply(N, 2:4, sum, na.rm = TRUE)), 
              each = dim(N)[1])
  lj[is.na(lj)] <- 0
  Pi.hat <- apply(aperm(aperm(Qij, c(1, 3, 4, 5, 2)) * as.vector(lj), 
                        c(1, 5, 2, 3, 4)), 2:5, sum, na.rm = TRUE)
  if (!all(round(apply(Pi.hat, 2:4, sum, na.rm = TRUE), 6) == 
           1)) 
    warning("some length classes from 'dbeOutput@lenStruc' slot are not in 'ca' table")
  D.hat <- apply(N, 2:4, sum, na.rm = TRUE)
  D_i <- Pi.hat * rep(D.hat, each = dim(Pi.hat)[1])
  VarDj <- dbeOutput@lenVar
  if (type %in% c("fixedK", "propK", "agesK")) {
    if (type == "agesK") {
      a1 <- Pi.hat * (1 - Pi.hat)
      VarPi <- a1/rep(ns, each = dim(a1)[1])
    }    else {
      if (type == "fixedK") {
        b1 <- apply(aperm(aperm(Qij * (1 - Qij), c(1, 
                                                   3, 4, 5, 2)) * as.vector(lj * (1 - lj)), c(1, 
                                                                                              5, 2, 3, 4)), 2:5, sum, na.rm = TRUE)
        b2 <- apply(aperm(aperm(Qij * (1 - Qij), c(1, 
                                                   3, 4, 5, 2)) * as.vector(lj * lj), c(1, 5, 
                                                                                        2, 3, 4)), 2:5, sum, na.rm = TRUE)
        b3 <- apply(aperm(aperm(Qij * Qij, c(1, 3, 4, 
                                             5, 2)) * as.vector(lj), c(1, 5, 2, 3, 4)), 
                    2:5, sum, na.rm = TRUE) - Pi.hat^2
        VarPi <- b1/rep(Nl * njStar, each = dim(b1)[1]) + 
          b2/rep(njStar, each = dim(b2)[1]) + b3/rep(Nl, 
                                                     each = dim(b3)[1])
      }      else {
        c1 <- apply(aperm(aperm(Qij * (1 - Qij), c(1, 
                                                   3, 4, 5, 2)) * as.vector(lj), c(1, 5, 2, 3, 
                                                                                   4)), 2:5, sum, na.rm = TRUE)
        c2 <- apply(aperm(aperm(Qij * Qij, c(1, 3, 4, 
                                             5, 2)) * as.vector(lj), c(1, 5, 2, 3, 4)), 
                    2:5, sum, na.rm = TRUE) - Pi.hat^2
        VarPi <- c1/rep(ns, each = dim(c1)[1]) + c2/rep(Nl, 
                                                        each = dim(c2)[1])
      }
    }
    V1 <- VarPi * rep(D.hat * D.hat, each = dim(VarPi)[1])
    if (!all(is.na(VarDj))) {
      VarD <- tapply(VarDj$value, list(time = factor(VarDj$time, 
                                                     levels = dimnames(N)[[2]]), space = factor(VarDj$space, 
                                                                                                levels = dimnames(N)[[3]]), technical = factor(VarDj$technical, 
                                                                                                                                               levels = dimnames(N)[[4]])), sum, na.rm = TRUE)
      V2 <- Pi.hat * Pi.hat * rep(VarD, each = dim(Pi.hat)[1])
      V3 <- VarPi * rep(VarD, each = dim(VarPi)[1])
    }    else {
      V2 <- V3 <- V1
    }
    VarD_i <- V1 + V2 + V3
  }  else {
    VarQij <- aperm(Qij * (1 - Qij), c(1, 3, 4, 5, 2))/as.vector(nj)
    V1 <- aperm(VarQij * as.vector(N * N), c(1, 5, 2, 3, 
                                             4))
    if (!all(is.na(VarDj))) {
      VarNj <- tapply(VarDj$value, list(length = VarDj$length, 
                                        time = VarDj$time, space = VarDj$space, technical = VarDj$technical), 
                      sum, na.rm = TRUE)
      V2 <- aperm(aperm(Qij * Qij, c(1, 3, 4, 5, 2)) * 
                    as.vector(VarNj), c(1, 5, 2, 3, 4))
      V3 <- aperm(VarQij * as.vector(VarNj), c(1, 5, 2, 
                                               3, 4))
    }    else {
      V2 <- V3 <- V1
    }
    VarD_i <- apply(V1 + V2 + V3, 2:5, sum, na.rm = TRUE)
  }
  df.D_i <- cbind(expand.grid(dimnames(D_i)), value = as.vector(D_i))
  df.VarD_i <- cbind(expand.grid(dimnames(VarD_i)), value = as.vector(VarD_i))
  df.VarD_i <- df.VarD_i[!is.na(df.D_i$val), ]
  df.D_i <- df.D_i[!is.na(df.D_i$val), ]
  df.VarD_i <- df.VarD_i[df.D_i$val > 0, ]
  df.D_i <- df.D_i[df.D_i$val > 0, ]
  df.D_i <- df.D_i[order(df.D_i$time, df.D_i$space, df.D_i$technical, 
                         df.D_i$age), ]
  rownames(df.D_i) <- 1:nrow(df.D_i)
  dbeOutput@ageStruc$estim <- df.D_i[, names(dbeOutput@ageStruc$estim)]
  df.VarD_i <- df.VarD_i[order(df.VarD_i$time, df.VarD_i$space, 
                               df.VarD_i$technical, df.VarD_i$age), ]
  rownames(df.VarD_i) <- 1:nrow(df.VarD_i)
  if (!all(is.na(VarDj))) 
    dbeOutput@ageVar <- df.VarD_i[, names(dbeOutput@ageVar)]
  return(dbeOutput)
}

# RaiseAge_NoTime ---------------------------------------------------------

### Modified RaiseAge () FUN of COSTdbe
### Raise Age using one common ALK for all time strata (eg quarters) and all gears
###  USeful if annual age data collection is not time -stratified 
### (the original function would use time-specific ALK)

RaiseAge_NoTime=function (csObject, dbeOutput, type = "p", sex = as.character(NA)) 
{
  sp <- dbeOutput@species
  ca <- ca(csObject)
  ca <- ca[ca$spp %in% sp, ]
  if (nrow(ca) == 0) 
    stop("no CA data for specified species in input object!!")
  Unit <- paste(ca$PSUid, ca$SSUid, sep = ":-:")
  nSAMP <- spdAgreg(list(value = Unit), BY = list(time = ca$time, 
                                                  space = ca$space), function(x) length(unique(x)))
  dbeOutput@nSamp$age <- nSAMP
  if (!(all(is.na(sex)))) {
    ca <- ca[ca$sex %in% sex, ]
    if (nrow(ca) == 0) 
      stop("no CA data for specified sex in input object!!")
  }
  CAreal <- ca[ca$fishId > 0, ]
  nMEAS <- spdAgreg(list(value = CAreal$age), BY = list(time = CAreal$time, 
                                                        space = CAreal$space), function(x) sum(!is.na(x)))
  dbeOutput@nMeas$age <- nMEAS
  Ldf <- dbeOutput@lenStruc$estim
  N <- tapply(Ldf$value, list(length = Ldf$length, time = Ldf$time, 
                              space = Ldf$space, technical = Ldf$technical), sum, 
              na.rm = TRUE)
  ALK <- tapply(ca$age, list(length = factor(ca$lenCls, levels = dimnames(N)[[1]]), 
                             age = ca$age,
                           # time = factor(ca$time, levels = dimnames(N)[[2]]),#!!
                             space = factor(ca$space, levels = dimnames(N)[[3]])), 
                length)
  ALK[is.na(ALK)] <- 0
  ll <- dimnames(ALK)
  ll[["technical"]] <- dimnames(N)[[4]] 
  ll[["time"]] <- dimnames(N)[[2]]##!!
  ALK <-
    array(
      rep(as.vector(ALK), dim(N)[4] * dim(N)[2]),##!!
      dim = c(dim(ALK),
              dim(N)[4], dim(N)[2]),##!!
      dimnames = ll
    )
  if (all(is.na(dbeOutput@lenStruc$estim))) 
    stop("estimates for length structure are missing in 'dbeOutput' object!!")
  nj <- tapply(ca$lenCls, list(
    length = factor(ca$lenCls,
                    levels = dimnames(N)[[1]]),
    # time = factor(ca$time, levels = dimnames(N)[[2]]),#!!
    space = factor(ca$space, levels = dimnames(N)[[3]])
  ),
  function(x)
    sum(!is.na(x)))
  nj[is.na(nj)] <- 0
  ll2 <- dimnames(nj)
  ll2[["technical"]] <- dimnames(N)[[4]]
  ll2[["time"]] <- dimnames(N)[[2]]##!!
  nj <-
    array(
      rep(as.vector(nj), dim(N)[4] * dim(N)[2]),##!!
      dim = c(dim(nj),
              dim(N)[4], dim(N)[2]),##!!
      dimnames = ll2
    )
  Nl <- apply(nj, 2:4, sum)
  ns <- apply(ALK, 3:5, sum)
  nl <- apply(nj, 2:4, function(x) sum(x > 0))
  Qij <- aperm(aperm(ALK, c(1, 3, 4, 5, 2))/as.vector(apply(ALK, 
                                                            c(1, 3:5), sum)), c(1, 5, 2:4))
  if (type == "fixedK") {
    njStar <- ns/nl
  }  else {
    njStar <- nj * rep(as.vector(ns/Nl), each = dim(nj)[1])
  }
  lj <- N/rep(as.vector(apply(N, 2:4, sum, na.rm = TRUE)), 
              each = dim(N)[1])
  lj[is.na(lj)] <- 0
  Pi.hat <- apply(aperm(aperm(Qij, c(1, 3, 4, 5, 2)) * as.vector(lj), 
                        c(1, 5, 2, 3, 4)), 2:5, sum, na.rm = TRUE)
  if (!all(round(apply(Pi.hat, 2:4, sum, na.rm = TRUE), 6) == 
           1)) 
    warning("some length classes from 'dbeOutput@lenStruc' slot are not in 'ca' table")
  D.hat <- apply(N, 2:4, sum, na.rm = TRUE)
  D_i <- Pi.hat * rep(D.hat, each = dim(Pi.hat)[1])
  VarDj <- dbeOutput@lenVar
  if (type %in% c("fixedK", "propK", "agesK")) {
    if (type == "agesK") {
      a1 <- Pi.hat * (1 - Pi.hat)
      VarPi <- a1/rep(ns, each = dim(a1)[1])
    }    else {
      if (type == "fixedK") {
        b1 <- apply(aperm(aperm(Qij * (1 - Qij), c(1, 
                                                   3, 4, 5, 2)) * as.vector(lj * (1 - lj)), c(1, 
                                                                                              5, 2, 3, 4)), 2:5, sum, na.rm = TRUE)
        b2 <- apply(aperm(aperm(Qij * (1 - Qij), c(1, 
                                                   3, 4, 5, 2)) * as.vector(lj * lj), c(1, 5, 
                                                                                        2, 3, 4)), 2:5, sum, na.rm = TRUE)
        b3 <- apply(aperm(aperm(Qij * Qij, c(1, 3, 4, 
                                             5, 2)) * as.vector(lj), c(1, 5, 2, 3, 4)), 
                    2:5, sum, na.rm = TRUE) - Pi.hat^2
        VarPi <- b1/rep(Nl * njStar, each = dim(b1)[1]) + 
          b2/rep(njStar, each = dim(b2)[1]) + b3/rep(Nl, 
                                                     each = dim(b3)[1])
      }      else {
        c1 <- apply(aperm(aperm(Qij * (1 - Qij), c(1, 
                                                   3, 4, 5, 2)) * as.vector(lj), c(1, 5, 2, 3, 
                                                                                   4)), 2:5, sum, na.rm = TRUE)
        c2 <- apply(aperm(aperm(Qij * Qij, c(1, 3, 4, 
                                             5, 2)) * as.vector(lj), c(1, 5, 2, 3, 4)), 
                    2:5, sum, na.rm = TRUE) - Pi.hat^2
        VarPi <- c1/rep(ns, each = dim(c1)[1]) + c2/rep(Nl, 
                                                        each = dim(c2)[1])
      }
    }
    V1 <- VarPi * rep(D.hat * D.hat, each = dim(VarPi)[1])
    if (!all(is.na(VarDj))) {
      VarD <- tapply(VarDj$value, list(time = factor(VarDj$time, 
                                                     levels = dimnames(N)[[2]]), space = factor(VarDj$space, 
                                                                                                levels = dimnames(N)[[3]]), technical = factor(VarDj$technical, 
                                                                                                                                               levels = dimnames(N)[[4]])), sum, na.rm = TRUE)
      V2 <- Pi.hat * Pi.hat * rep(VarD, each = dim(Pi.hat)[1])
      V3 <- VarPi * rep(VarD, each = dim(VarPi)[1])
    }    else {
      V2 <- V3 <- V1
    }
    VarD_i <- V1 + V2 + V3
  }  else {
    VarQij <- aperm(Qij * (1 - Qij), c(1, 3, 4, 5, 2))/as.vector(nj)
    V1 <- aperm(VarQij * as.vector(N * N), c(1, 5, 2, 3, 
                                             4))
    if (!all(is.na(VarDj))) {
      VarNj <- tapply(VarDj$value, list(length = VarDj$length, 
                                        time = VarDj$time, space = VarDj$space, technical = VarDj$technical), 
                      sum, na.rm = TRUE)
      V2 <- aperm(aperm(Qij * Qij, c(1, 3, 4, 5, 2)) * 
                    as.vector(VarNj), c(1, 5, 2, 3, 4))
      V3 <- aperm(VarQij * as.vector(VarNj), c(1, 5, 2, 
                                               3, 4))
    }    else {
      V2 <- V3 <- V1
    }
    VarD_i <- apply(V1 + V2 + V3, 2:5, sum, na.rm = TRUE)
  }
  df.D_i <- cbind(expand.grid(dimnames(D_i)), value = as.vector(D_i))
  df.VarD_i <- cbind(expand.grid(dimnames(VarD_i)), value = as.vector(VarD_i))
  df.VarD_i <- df.VarD_i[!is.na(df.D_i$val), ]
  df.D_i <- df.D_i[!is.na(df.D_i$val), ]
  df.VarD_i <- df.VarD_i[df.D_i$val > 0, ]
  df.D_i <- df.D_i[df.D_i$val > 0, ]
  df.D_i <- df.D_i[order(df.D_i$time, df.D_i$space, df.D_i$technical, 
                         df.D_i$age), ]
  rownames(df.D_i) <- 1:nrow(df.D_i)
  dbeOutput@ageStruc$estim <- df.D_i[, names(dbeOutput@ageStruc$estim)]
  df.VarD_i <- df.VarD_i[order(df.VarD_i$time, df.VarD_i$space, 
                               df.VarD_i$technical, df.VarD_i$age), ]
  rownames(df.VarD_i) <- 1:nrow(df.VarD_i)
  if (!all(is.na(VarDj))) 
    dbeOutput@ageVar <- df.VarD_i[, names(dbeOutput@ageVar)]
  return(dbeOutput)
}


# RaiseAge_byGearTime ---------------------------------------------------------


### Modified RaiseAge () FUN of COSTdbe
### Raise Age using gear (technical) specific ALK 
### USe if age data collection is stratified for gears
### (the original function would repeat the same ALK for all  gears)

RaiseAge_byGearTime=function (csObject, dbeOutput, type = "p", sex = as.character(NA)) 
{
  
  sp <- dbeOutput@species
  ca <- ca(csObject)
  ca <- ca[ca$spp %in% sp, ]
  if (nrow(ca) == 0) 
    stop("no CA data for specified species in input object!!")
  Unit <- paste(ca$PSUid, ca$SSUid, sep = ":-:")
  nSAMP <- spdAgreg(list(value = Unit), BY = list(time = ca$time, 
                                                  space = ca$space), function(x) length(unique(x)))
  dbeOutput@nSamp$age <- nSAMP
  if (!(all(is.na(sex)))) {
    ca <- ca[ca$sex %in% sex, ]
    if (nrow(ca) == 0) 
      stop("no CA data for specified sex in input object!!")
  }
  CAreal <- ca[ca$fishId > 0, ]
  nMEAS <- spdAgreg(list(value = CAreal$age), BY = list(time = CAreal$time, 
                                                        space = CAreal$space), function(x) sum(!is.na(x)))
  dbeOutput@nMeas$age <- nMEAS
  Ldf <-    dbeOutput@lenStruc$estim %>% mutate_if(is.factor, as.character) %>% semi_join(ca %>% filter(!is.na(age))) #!!
  N <- tapply(
    Ldf$value,
    list(
      length = Ldf$length,
      time = Ldf$time,
      space = Ldf$space,
      technical = Ldf$technical
    ),
    sum,
    na.rm = TRUE
  )
  ALK <-
    tapply(ca$age,
           list(
             length = factor(ca$lenCls, levels = dimnames(N)[[1]]),
             age = ca$age,
             time = factor(ca$time, levels = dimnames(N)[[2]]),
             space = factor(ca$space, levels = dimnames(N)[[3]])
           ),
           
           
           length)
  ALK[is.na(ALK)] <- 0
  ll <- dimnames(ALK)
  ll[["technical"]] <- dimnames(N)[[4]]
  
  
  ALK <- array(rep(as.vector(ALK), dim(N)[4]),
               dim = c(dim(ALK),
                       dim(N)[4]),
               dimnames = ll)
  if (all(is.na(dbeOutput@lenStruc$estim)))
    stop("estimates for length structure are missing in 'dbeOutput' object!!")
  nj <- tapply(ca$lenCls, list(
    length = factor(ca$lenCls,
                    levels = dimnames(N)[[1]]),
    time = factor(ca$time, levels = dimnames(N)[[2]]),
    space = factor(ca$space, levels = dimnames(N)[[3]])
  ),
  function(x)
    sum(!is.na(x)))
  nj[is.na(nj)] <- 0
  ll2 <- dimnames(nj)
  ll2[["technical"]] <- dimnames(N)[[4]]
  nj <- array(rep(as.vector(nj), dim(N)[4]), dim = c(dim(nj), 
                                                     dim(N)[4]), dimnames = ll2)
  Nl <- apply(nj, 2:4, sum)
  ns <- apply(ALK, 3:5, sum)
  nl <- apply(nj, 2:4, function(x) sum(x > 0))
  Qij <- aperm(aperm(ALK, c(1, 3, 4, 5, 2))/as.vector(apply(ALK, 
                                                            c(1, 3:5), sum)), c(1, 5, 2:4))
  if (type == "fixedK") {
    njStar <- ns/nl
  }  else {
    njStar <- nj * rep(as.vector(ns/Nl), each = dim(nj)[1])
  }
  lj <- N/rep(as.vector(apply(N, 2:4, sum, na.rm = TRUE)), 
              each = dim(N)[1])
  lj[is.na(lj)] <- 0
  Pi.hat <- apply(aperm(aperm(Qij, c(1, 3, 4, 5, 2)) * as.vector(lj), 
                        c(1, 5, 2, 3, 4)), 2:5, sum, na.rm = TRUE)
  if (!all(round(apply(Pi.hat, 2:4, sum, na.rm = TRUE), 6) == 
           1)) 
    warning("some length classes from 'dbeOutput@lenStruc' slot are not in 'ca' table")
  D.hat <- apply(N, 2:4, sum, na.rm = TRUE)
  D_i <- Pi.hat * rep(D.hat, each = dim(Pi.hat)[1])
  VarDj <- dbeOutput@lenVar
  if (type %in% c("fixedK", "propK", "agesK")) {
    if (type == "agesK") {
      a1 <- Pi.hat * (1 - Pi.hat)
      VarPi <- a1/rep(ns, each = dim(a1)[1])
    }    else {
      if (type == "fixedK") {
        b1 <- apply(aperm(aperm(Qij * (1 - Qij), c(1, 
                                                   3, 4, 5, 2)) * as.vector(lj * (1 - lj)), c(1, 
                                                                                              5, 2, 3, 4)), 2:5, sum, na.rm = TRUE)
        b2 <- apply(aperm(aperm(Qij * (1 - Qij), c(1, 
                                                   3, 4, 5, 2)) * as.vector(lj * lj), c(1, 5, 
                                                                                        2, 3, 4)), 2:5, sum, na.rm = TRUE)
        b3 <- apply(aperm(aperm(Qij * Qij, c(1, 3, 4, 
                                             5, 2)) * as.vector(lj), c(1, 5, 2, 3, 4)), 
                    2:5, sum, na.rm = TRUE) - Pi.hat^2
        VarPi <- b1/rep(Nl * njStar, each = dim(b1)[1]) + 
          b2/rep(njStar, each = dim(b2)[1]) + b3/rep(Nl, 
                                                     each = dim(b3)[1])
      }      else {
        c1 <- apply(aperm(aperm(Qij * (1 - Qij), c(1, 
                                                   3, 4, 5, 2)) * as.vector(lj), c(1, 5, 2, 3, 
                                                                                   4)), 2:5, sum, na.rm = TRUE)
        c2 <- apply(aperm(aperm(Qij * Qij, c(1, 3, 4, 
                                             5, 2)) * as.vector(lj), c(1, 5, 2, 3, 4)), 
                    2:5, sum, na.rm = TRUE) - Pi.hat^2
        VarPi <- c1/rep(ns, each = dim(c1)[1]) + c2/rep(Nl, 
                                                        each = dim(c2)[1])
      }
    }
    V1 <- VarPi * rep(D.hat * D.hat, each = dim(VarPi)[1])
    if (!all(is.na(VarDj))) {
      VarD <- tapply(VarDj$value, list(time = factor(VarDj$time, 
                                                     levels = dimnames(N)[[2]]), space = factor(VarDj$space, 
                                                                                                levels = dimnames(N)[[3]]), technical = factor(VarDj$technical, 
                                                                                                                                               levels = dimnames(N)[[4]])), sum, na.rm = TRUE)
      V2 <- Pi.hat * Pi.hat * rep(VarD, each = dim(Pi.hat)[1])
      V3 <- VarPi * rep(VarD, each = dim(VarPi)[1])
    }    else {
      V2 <- V3 <- V1
    }
    VarD_i <- V1 + V2 + V3
  }  else {
    VarQij <- aperm(Qij * (1 - Qij), c(1, 3, 4, 5, 2))/as.vector(nj)
    V1 <- aperm(VarQij * as.vector(N * N), c(1, 5, 2, 3, 
                                             4))
    if (!all(is.na(VarDj))) {
      VarNj <- tapply(VarDj$value, list(length = VarDj$length, 
                                        time = VarDj$time, space = VarDj$space, technical = VarDj$technical), 
                      sum, na.rm = TRUE)
      V2 <- aperm(aperm(Qij * Qij, c(1, 3, 4, 5, 2)) * 
                    as.vector(VarNj), c(1, 5, 2, 3, 4))
      V3 <- aperm(VarQij * as.vector(VarNj), c(1, 5, 2, 
                                               3, 4))
    }    else {
      V2 <- V3 <- V1
    }
    VarD_i <- apply(V1 + V2 + V3, 2:5, sum, na.rm = TRUE)
  }
  df.D_i <- cbind(expand.grid(dimnames(D_i)), value = as.vector(D_i))
  df.VarD_i <- cbind(expand.grid(dimnames(VarD_i)), value = as.vector(VarD_i))
  df.VarD_i <- df.VarD_i[!is.na(df.D_i$val), ]
  df.D_i <- df.D_i[!is.na(df.D_i$val), ]
  df.VarD_i <- df.VarD_i[df.D_i$val > 0, ]
  df.D_i <- df.D_i[df.D_i$val > 0, ]
  df.D_i <- df.D_i[order(df.D_i$time, df.D_i$space, df.D_i$technical, 
                         df.D_i$age), ]
  rownames(df.D_i) <- 1:nrow(df.D_i)
  dbeOutput@ageStruc$estim <- df.D_i[, names(dbeOutput@ageStruc$estim)]
  df.VarD_i <- df.VarD_i[order(df.VarD_i$time, df.VarD_i$space, 
                               df.VarD_i$technical, df.VarD_i$age), ]
  rownames(df.VarD_i) <- 1:nrow(df.VarD_i)
  if (!all(is.na(VarDj))) 
    dbeOutput@ageVar <- df.VarD_i[, names(dbeOutput@ageVar)]
  return(dbeOutput)
  
}




# N_at_Age_direct ---------------------------------------------------------


## from COSTDBE
##  for the cases where the estimation of age is
# done without the need to use an ALK. This is the case where the collection of otolith is based on a random
# sampling of the population

# N_at_Age_direct=function (dbeOutput, objectcs, objectcl, sex = as.character(NA)){
#   sp <- dbeOutput@species
#   ca <- ca(objectcs)
#   cl <- cl(objectcl)
#   eval(parse("", text = paste("ca <- subset(ca,spp%in%", 
#                               deparse(sp), ")", sep = "")))
#   eval(parse("", text = paste("cl <- subset(cl,taxon%in%", 
#                               deparse(sp), ")", sep = "")))
#   if (!(all(is.na(sex)))) {
#     ca <- ca[ca$sex %in% sex, ]
#     if (nrow(ca) == 0) 
#       stop("no CA data for specified sex in input object!!")
#   }
#   dupTech <- function(mat, matTech) {
#     ll <- dimnames(mat)
#     ll[["technical"]] <- dimnames(matTech)[[3]]
#     array(rep(as.vector(mat), dim(matTech)[3]), dim = c(dim(mat), 
#                                                         dim(matTech)[3]), dimnames = ll)
#   }
#   cl$landMult[is.na(cl$landMult)] <- 1
#   totLand <- mapply(function(w, x, y, z) sum(c(w * x, y, z), 
#                                              na.rm = TRUE), cl$landWt, cl$landMult, cl$unallocCatchWt, 
#                     cl$misRepCatchWt)
#   W.fix_C <- tapply(totLand * 1000, list(time = cl$time, space = cl$space, 
#                                          technical = cl$technical), sum, na.rm = TRUE)
#   w.bar_c <- tapply(ca$indWt, list(time = factor(ca$time, levels = dimnames(W.fix_C)[[1]]), 
#                                    space = factor(ca$space, levels = dimnames(W.fix_C)[[2]])), 
#                     mean, na.rm = TRUE)
#   w.bar_c <- dupTech(w.bar_c, W.fix_C)
#   err <- tapply(ca$indWt, list(time = factor(ca$time, levels = dimnames(W.fix_C)[[1]]), 
#                                space = factor(ca$space, levels = dimnames(W.fix_C)[[2]])), 
#                 sd, na.rm = TRUE)
#   n <- tapply(ca$indWt, list(time = factor(ca$time, levels = dimnames(W.fix_C)[[1]]), 
#                              space = factor(ca$space, levels = dimnames(W.fix_C)[[2]])), 
#               length)
#   w.err_c <- err/sqrt(n)
#   w.err_c <- dupTech(w.err_c, W.fix_C)
#   factAge <- factor(ca$age, levels = seq(min(ca$age, na.rm = TRUE), 
#                                          max(ca$age, na.rm = TRUE), by = 1))
#   Freq <- tapply(ca$indWt, list(age = factAge, time = factor(ca$time, 
#                                                              levels = dimnames(W.fix_C)[[1]]), space = factor(ca$space, 
#                                                                                                               levels = dimnames(W.fix_C)[[2]])), length)
#   Freq[is.na(Freq)] <- 0
#   Freq <- dupTech(Freq, W.fix_C)
#   Freqt <- tapply(ca$indWt, list(time = factor(ca$time, levels = dimnames(W.fix_C)[[1]]), 
#                                  space = factor(ca$space, levels = dimnames(W.fix_C)[[2]])), 
#                   length)
#   Freqt[is.na(Freqt)] <- 0
#   Freqt <- dupTech(Freqt, W.fix_C)
#   phat_kc <- Freq/rep(Freqt, each = dim(Freq)[1])
#   perr_kc <- sqrt(phat_kc * (1 - phat_kc)/rep(n, each = dim(phat_kc)[1]))
#   N_k <- phat_kc * rep(W.fix_C/w.bar_c, each = dim(phat_kc)[1])
#   D1_k <- phat_kc * rep(-W.fix_C/(w.bar_c^2), each = dim(phat_kc)[1])
#   D2_k <- W.fix_C/w.bar_c
#   dEEd <- (D1_k^2) * rep(w.err_c^2, each = dim(D1_k)[1]) + 
#     (perr_kc^2) * rep(D2_k^2, each = dim(perr_kc)[1])
#   df.N_i <- cbind(expand.grid(dimnames(N_k)), value = as.vector(N_k))
#   df.VarN_i <- cbind(expand.grid(dimnames(dEEd)), value = as.vector(dEEd))
#   df.VarN_i <- df.VarN_i[!is.na(df.VarN_i$val), ]
#   df.N_i <- df.N_i[!is.na(df.N_i$val), ]
#   df.VarN_i <- df.VarN_i[df.VarN_i$val > 0, ]
#   df.N_i <- df.N_i[df.N_i$val > 0, ]
#   df.N_i <- df.N_i[order(df.N_i$time, df.N_i$space, df.N_i$technical, 
#                          df.N_i$age), ]
#   rownames(df.N_i) <- 1:nrow(df.N_i)
#   dbeOutput@ageStruc$estim <- df.N_i[, names(dbeOutput@ageStruc$estim)]
#   df.VarN_i <- df.VarN_i[order(df.VarN_i$time, df.VarN_i$space, 
#                                df.VarN_i$technical, df.VarN_i$age), ]
#   rownames(df.VarN_i) <- 1:nrow(df.VarN_i)
#   dbeOutput@ageVar <- df.VarN_i[, names(dbeOutput@ageVar)]
#   return(dbeOutput)
# }

######## the ###########
######## end ###########






