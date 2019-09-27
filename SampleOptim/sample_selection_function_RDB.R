porSex<-function(parte,SexR,numTotF,numTotM){
  q_s1<-NULL;
  if(dim(parte)[1]>0){
    if(SexR){ # selection based on SexRatio
      #Females selection    
      numF<-numTotF;
      parteF<-parte[parte$Sex=="F",];
      if(dim(parteF)[1]>numF) q_s1_F<-sample(parteF$ID_BIO_FISH, numF, replace=F)
      else q_s1_F<-parteF$ID_BIO_FISH;
      numF<-length(q_s1_F)
      
      #Males selection
      numM<-numTotM;
      parteM<-parte[parte$Sex=="M",];
      if(dim(parteM)[1]>numM) q_s1_M<-sample(parteM$ID_BIO_FISH, numM, replace=F)
      else q_s1_M<-parteM$ID_BIO_FISH;
      numM<-length(q_s1_M)
      q_s1<-c(q_s1_F, q_s1_M);
      
      #Select indetermined to complete the number of individuals 
      numI<-numTotF+numTotM-numF-numM;
      if(numI>0){
        parteI<-parte[parte$Sex=="I",];
        if(dim(parteI)[1]>numI) q_s1_I<-sample(parteI$ID_BIO_FISH, numI, replace=F)
        else q_s1_I<-parteI$ID_BIO_FISH;
        q_s1<-c(q_s1, q_s1_I)
      }
    }
    else{ # Selection when the SexRatio is not specified 
      quantidade<-numTotF+numTotM
      if(dim(parte)[1]>quantidade){
        q_s1<-sample(parte$ID_BIO_FISH, quantidade, replace=F);
        if(quantidade != length(q_s1)) print(paste("ERRO na quantidade:",quantidade," diferente de:",parte$ID_BIO_FISH));
      }else{
        q_s1<- parte$ID_BIO_FISH;
      }
    }
  }
  return(q_s1);
}

##
#Length classes by Sex
#Selection of individuals by length class and Sex (SexR=T)
#
porClassesPorSex<-function(todos,classes,SexR,numTotF,numTotM){
  idRes<-NULL;
  for(cl in classes){   #by each length class
    parte<-todos[todos$Length_class == cl, ];
    q_s1<-porSex(parte,SexR,numTotF,numTotM)
    idRes<-c(idRes, q_s1);
  }
  print(paste("Número de individuos: ",length(idRes)))
  return(idRes);
}

##
#Port, length class and Sex
#
# Selection of indivulas by Port, by length class and by Sex (SexR=T) (numTotF - total number of females, numTotM - total number of males)
# Selection of (numTotF+numTotM) individuals by Port and by each length class (distUniPorto=FALSE) (maximum value = numeroPortos*numeroClasses*quantidade)
# Case the distUniPorto=TRUE, the number of individuals selected by length class presents (maximum value = numeroClasses*quantidade)
# an uniform distribution between the different Port of landings.
#
# todos - all the individuals according with the temporal selection pattern
# distUniPorto - the relation between the Port (Area of fishing) with the sample selection
# conjClasses - all the length classes
#
porPortosPorClassesPorSex<-function(todos,classes,SexR,SexRatio,quantidade,distUniPorto){
  idRes<-NULL;
  conjPortos<-unique(todos$Port);
  numPortos<-length(conjPortos)
  
  if(distUniPorto){
    quantidadePorPorto <- trunc(dunif(seq(1,1, length = numPortos))/numPortos*quantidade)
    falta<-quantidade-sum(quantidadePorPorto)
    resto<-c(rep(1,falta),rep(0,(numPortos-falta)))
    quantidadePorPorto<-quantidadePorPorto+sample(resto,numPortos)
  } else quantidadePorPorto<-array(quantidade, numPortos)
  numF<-ceiling(quantidadePorPorto*SexRatio)
  numM<-quantidadePorPorto-numF
  nporto<-1
  for(p in conjPortos){
    print(paste("Porto: ", p));
    pPorto<-todos[todos$Port == p, ];
    q_s1<-porClassesPorSex(pPorto,classes,SexR,numF[nporto],numM[nporto])
    idRes<-c(idRes, q_s1);
    nporto<-nporto+1
  }
  return(idRes);
}

##
#Semester
#
# Sets the semester (1 or 2) according to the month
semestre<-function(month){
  return(ceiling(month/6));
}

##
#Quarter
#
# Sets the quarter (1,2,3 or 4) according to the month
trimestre<-function(month){
  return(ceiling(month/3))
}



# SexRatio - proportion of females and males (ex: 1 - 100% females, 0 - 100% males, 0.2 - 20% females 80% males)
#   if SexRatio <0 the selection in not dependent of the individuals Sex
# conjClasses - length classes range
# quantidade - number of individuals by length class
# tm - temporal option 
#     1S - by year and only concerning 1º semester
#     2S - by year and only concerning 2º semester
#     S  - by year and by semester
#     1T - by year and only concerning 1º quarter
#     2T - by year and only concerning 2º quarter
#     3T - by year and only concerning 3º quarter
#     4T - by year and only concerning 4º quarter
#     T  - by year and by quarter
#     A  - by year
# 
amostraTemporal<-function(tab, quantidade, conjClasses, SexRatio=NaN, tm="T", porto=TRUE, distUniPorto=TRUE){
  tab$SEMESTRE<-semestre(tab$MONTH);
  tab$TRIMESTRE<-trimestre(tab$MONTH);
  original<-tab;
  idRes<-NULL
  tab<-switch(tm,
              "1S" = tab[tab$SEMESTRE==1,],   # 1º semester
              "2S" = tab[tab$SEMESTRE==2,],   # 2º semester
              "1T" = tab[tab$TRIMESTRE==1,],  # 1º quarter
              "2T" = tab[tab$TRIMESTRE==2,],  # 2º quarter
              "3T" = tab[tab$TRIMESTRE==3,],  # 3º quarter
              "4T" = tab[tab$TRIMESTRE==4,],  # 4º quarter
              "A"  = tab,                     # year
              "S"  = tab,                     # semester
              "T"  = tab                      # quarter
  );
  if(is.null(tab)){
    print(paste("ERRO: Período temporal não foi inserido correctamente: <", tm,">"));
    return (NULL);
  }
  print(paste("Período de tempo: ",tm));
  print(paste("Número de indivíduos: ",dim(tab)[1]));
  SexR<-TRUE;
  if(!is.nan(SexRatio) & (SexRatio>=0) & (SexRatio<=1)){ # escolher de acordo com o SexRatio
    #calcular número de fêmeas e macho a escolher
    numTotF<-ceiling(quantidade * SexRatio);
    numTotM<-quantidade -numTotF;
    print(paste("SexRatio: ", SexRatio, "Femêas:",numTotF,"Machos:",numTotM));
  } else { SexR<-FALSE; print("SexRatio: Ignore"); }
  print(paste("Número de classes: ", length(conjClasses)));
  if(tm=="S"){
    ano<-unique(tab$ANO);
    for(a in ano)
      for(s in 1:2){
        print(paste("ano:",a," ",s,"S",sep=''))
        parte<-tab[tab$ANO==a & tab$SEMESTRE==s,]
        if(porto)
          q_s1<-porPortosPorClassesPorSex(parte,conjClasses,SexR,SexRatio,quantidade,distUniPorto)
        else  q_s1<-porClassesPorSex(parte, conjClasses,SexR,numTotF,numTotM)
        idRes<-c(idRes, q_s1);
      }
  }
  else if(tm=="T"){
    ano<-unique(tab$ANO);
    for(a in ano)
      for(t in 1:4){
        print(paste("ano:",a," ",t,"T",sep=''))
        parte<-tab[tab$ANO==a & tab$TRIMESTRE==t,]
        if(porto)
          q_s1<-porPortosPorClassesPorSex(parte,conjClasses,SexR,SexRatio,quantidade,distUniPorto)
        else  q_s1<-porClassesPorSex(parte, conjClasses,SexR,numTotF,numTotM)
        idRes<-c(idRes, q_s1);
      }
  }
  else if(tm=="A"){
    ano<-unique(tab$ANO);
    for(a in ano){
      print(paste("ano:",a,sep=''))
      parte<-tab[tab$ANO==a,]
      print(dim(parte))
      if(porto)
        q_s1<-porPortosPorClassesPorSex(parte,conjClasses,SexR,SexRatio,quantidade,distUniPorto)
      else  q_s1<-porClassesPorSex(parte, conjClasses,SexR,numTotF,numTotM)
      print(length(q_s1))
      idRes<-c(idRes, q_s1);
    }
  }
  else{ #tab contém a escolha de acordo com tm= "1S","2S","1T","2T","3T" ou "4T"
    ano<-unique(tab$ANO);
    for(a in ano){
      print(paste("ano:",a," ",tm,sep=''))
      parte<-tab[tab$ANO==a,]
      print(dim(parte))
      if(porto)
        q_s1<-porPortosPorClassesPorSex(parte,conjClasses,SexR,SexRatio,quantidade,distUniPorto)
      else  q_s1<-porClassesPorSex(parte, conjClasses,SexR,numTotF,numTotM)
      idRes<-c(idRes, q_s1);
    }
  }
  return(original[original$ID_BIO_FISH %in% idRes,])
}
