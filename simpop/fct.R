#Map landings with samplings effort using CLr and CSr objects
mapclcs<-function(CLr,CS){
	#taxon by gear
	mapcl<-CLr@cl%>%
		mutate(gear=substr(foCatEu6,1,3))%>%
		group_by(rect,year,quarter)%>%
		summarise(lan=sum(landWt,na.rm=T)/1000)%>%
		ungroup()
	mapcs<-left_join(CSr@sl,CSr@hh)# %>%
	mapcs<-left_join(CSr@sl,CSr@hh) %>%
		mutate(gear=substr(foCatEu6,1,3),
		       idsamp=paste(trpCode,staNum),
		       quarter=ceiling(as.numeric(substr(date,6,7))))%>%
		group_by(x=lonIni,y=latIni,catchCat)%>%
		summarise(wsamp=sum(wt,na.rm=T),nbsamp=n_distinct(idsamp))
	#draw a map
	mapcl$lon<-mapcl$lat<-mapcl$lonc<-mapcl$latc<-NA
	for(i in 1:nrow(mapcl)){
		if(nchar(mapcl$rect[i])==4){
		mapcl$lon[i]<-DATRAS::icesSquare2coord(mapcl$rect[i],"midpoint")$lon
		mapcl$lat[i]<-DATRAS::icesSquare2coord(mapcl$rect[i],"midpoint")$lat
		mapcl$lonc[i]<-DATRAS::icesSquare2coord(mapcl$rect[i],"corner")$lon
		mapcl$latc[i]<-DATRAS::icesSquare2coord(mapcl$rect[i],"corner")$lat
		}
	}
	rangex<-c(min(mapcl$lon,na.rm=T)-.1,max(mapcl$lon,na.rm=T)+.1)
	rangey<-c(min(mapcl$lat,na.rm=T)-.1,max(mapcl$lat,na.rm=T)+.1)
	#poly map
	map0<-ggplot(mapcl)+theme_bw()+
		geom_raster(data=mapcl,aes(x=lon,y=lat,fill=lan),stat="identity",alpha=.75)+ 
		geom_point(data=mapcs,aes(x=x,y=y,color=catchCat,size=wsamp),alpha=.8,shape=1)+
		borders("worldHires",xlim=rangex,ylim=rangey,fill="light grey",colour="light grey")+
		coord_quickmap(xlim=range(mapcl$lon,na.rm=T),ylim=range(mapcl$lat,na.rm=T))+
		scale_fill_distiller(palette='Spectral',name="Landings\n(kg)")+
		ggtitle(paste(unique(CLr@cl$year)))+
		facet_wrap(~quarter,ncol=2)+xlab("")+ylab("")

	return(map0)
}

#taxon by gear landings and sampling number
pltsppgear<-function(CLr,CS){
sppgear<-CLr@cl%>%
        mutate(gear=substr(foCatEu6,1,3),spp=taxon)%>%
        group_by(gear,spp)%>%
        summarise(lan=sum(landWt,na.rm=T)/1000)%>%
        ungroup()
sppsamp<-left_join(CSr@sl,CSr@hh) %>%
        mutate(gear=substr(foCatEu6,1,3),
		idsamp=paste(trpCode,staNum))%>%
        group_by(gear,spp)%>%
        summarise(sampw=sum(wt,na.rm=T)/1000,nbsamp=n_distinct(idsamp))%>%
        ungroup()
#a plot
p1<-ggplot()+
        geom_raster(data=sppgear,aes(x=gear,y=spp,fill=lan))+
                                scale_fill_distiller(palette='Spectral',name="Landings (t)")+
        geom_point(data=sppsamp,aes(x=gear,y=spp,size=nbsamp),shape=1)+
        theme_bw()+
        theme(axis.text.x = element_text(size=8, angle=90),
                axis.text.y = element_text(size=10, angle=0),
                strip.text.x=element_text(size=8,angle=90),
        strip.text.y=element_text(size=8,angle=0),
        legend.position="bottom")
return(p1)
}

#taxon by gear landings and sampling number for consolidated object
pltsppgearcons<-function(CLr,CS){
lan<-CLc@cl%>%
        group_by(technical,time,spp=taxon)%>%
        summarise(lan=sum(landWt,na.rm=T)/1000)%>%
        ungroup()
samp<-CSc@sl%>%#left_join(CSr@sl,CSr@hh) %>%
        mutate(idsamp=paste(trpCode,staNum))%>%
        group_by(technical,time,spp)%>%
        summarise(sampw=sum(wt,na.rm=T)/1000,nbsamp=n_distinct(idsamp))%>%
        ungroup()
#a plot
p1<-ggplot()+
        geom_raster(data=lan,aes(x=technical,y=spp,fill=lan))+
        scale_fill_distiller(palette='Spectral',name="Landings (t)")+
        geom_point(data=samp,aes(x=technical,y=spp,size=nbsamp),shape=1)+
        theme_bw()+
        theme(axis.text.x = element_text(size=8, angle=90),
                axis.text.y = element_text(size=10, angle=0),
                #strip.text.x=element_text(size=8,angle=90),
        #strip.text.y=element_text(size=8,angle=0),
        legend.position="bottom")+
	facet_wrap(~time)
return(p1)
}

pltlendbe<-function(dbe){
  len<-dbe@lenStruc$estim%>%mutate(length=as.numeric(length))
  p1<-ggplot(data=len,aes(x=length,y=value,colour=time,group=time))+
		geom_point()+
		geom_line()+
		facet_wrap(~technical,scale="free")
  return(p1)
}
pltcvdbe<-function(dbe){
  len<-dbe@lenNum$cv%>%mutate(length=as.numeric(length))
  p1<-ggplot(data=len,aes(x=length,y=value,colour=time,group=time))+
		geom_point()+
		geom_line()+
		facet_wrap(~technical,scale="free")
  return(p1)
}

tookdbeinfo<-function(dbe){
	fct1<-function(a){as.numeric(as.character(a))}
	#RaiseLgth
	lenstruc<-dbe@lenStruc$estim%>%mutate(spp=dbe@species,length=fct1(length))
	lencv<-dbe@lenNum$cv%>%mutate(spp=dbe@species,length=fct1(length))
	wcv<-data.frame(wcv=dbe@lenNum$DCRcvIndicator,spp=dbe@species)
	return(list(lenstruc,lencv,wcv))
}

raiselentrip<-function(CSc,myStr,spp,categ="LAN"){
	dbelan1<-dbeObject(species=spp,catchCat=categ,strataDesc=myStr)
	try(dbelan1<-RaiseLgth(dbelan1,CSc),silent=T)
	return(dbelan1)
}
raiselenlan<-function(CSc,CLc,myStr,spp,categ="LAN"){
	dbelan1<-dbeObject(species=spp,catchCat=categ,strataDesc=myStr)
	try(dbelan1<-RaiseLgth(dbelan1,CSc,CLc),silent=T)
	return(dbelan1)
}
tripsubset<-function(CSc,trip){
	CSc@tr<-CSc@tr%>%filter(trpCode%in%trip)
	CSc@hh<-CSc@hh%>%filter(trpCode%in%trip)
	CSc@sl<-CSc@sl%>%filter(trpCode%in%trip)
	CSc@hl<-CSc@hl%>%filter(trpCode%in%trip)
	CSc@ca<-CSc@ca%>%filter(trpCode%in%trip)
	return(CSc)
}
rbindrez<-function(rez1,rez2){
	aa<-rbind(rez1[[1]],rez2[[1]])
	bb<-rbind(rez1[[2]],rez2[[2]])
	cc<-rbind(rez1[[3]],rez2[[3]])
	return(list(aa,bb,cc))
}
addreprez<-function(rez1,aa,bb){
	rez1[[1]]$rep<-aa
	rez1[[2]]$rep<-aa
	rez1[[3]]$rep<-aa
	rez1[[1]]$n<-bb
	rez1[[2]]$n<-bb
	rez1[[3]]$n<-bb
	return(rez1)
}
sim1triplan<-function(CSc,CLc,myStr,idtrip0){
	CSc0<-tripsubset(CSc,idtrip0)
	listspp0<-unique(CSc0@hl$spp)
	rez<-list(data.frame(),data.frame(),data.frame())
	for(i in as.character(listspp0)){
		rez<-rbindrez(rez,tookdbeinfo(raiselenlan(CSc0,CLc,myStr,i)))
	}
	return(rez)
}

sim1trip<-function(CSc,myStr,idtrip0){
	CSc0<-tripsubset(CSc,idtrip0)
	listspp0<-unique(CSc0@hl$spp)
	rez<-list(data.frame(),data.frame(),data.frame())
	for(i in as.character(listspp0)){
		rez<-rbindrez(rez,tookdbeinfo(raiselentrip(CSc0,myStr,i)))
	}
	return(rez)
}

simutrip<-function(CSc,myStr,n,nrep){
        idtrip<-unique(CSc@hh$trpCode)
	rez<-list(data.frame(),data.frame(),data.frame())
	for(i in n){
		for(j in 1:nrep){
			idtriprand<-sample(idtrip,i,replace=T) 
			reztmp<-sim1trip(CSc,myStr,idtriprand)
			reztmp<-addreprez(reztmp,j,i)
			rez<-rbindrez(rez,reztmp)
		}
	}
	return(rez)
}
simutriplan<-function(CSc,CLc,myStr,n,nrep){
        idtrip<-unique(CSc@hh$trpCode)
	rez<-list(data.frame(),data.frame(),data.frame())
	for(i in n){
		for(j in 1:nrep){
			idtriprand<-sample(idtrip,i,replace=T) 
			reztmp<-sim1triplan(CSc,CLc,myStr,idtriprand)
			reztmp<-addreprez(reztmp,j,i)
			rez<-rbindrez(rez,reztmp)
		}
	}
	return(rez)
}


pipo<-function(){
pipo<-simutrip(CSc,myStr,n=5:10,nrep=1:5)
pipo<-simutriplan(CSc,CLc,myStr,n=c(10),nrep=1)

	idtrip0<-idtrip[1:20]
sim1trip(CSc,myStr,idtrip0)
sim1triplan(CSc,CLc,myStr,idtrip0)

#listspp,n=10,nbtrip=c(10,20,30)
	tripsubset(CSc,idtrip[1:2])
        nbhalftrip<-round(length(idtrip))
        idtriprand<-sample(idtrip,nbhalftrip,replace=T) 
}




