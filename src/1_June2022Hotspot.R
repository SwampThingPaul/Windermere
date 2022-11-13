## 
## Windermere Big Survey
##
## Code was compiled by Paul Julian
## contact info: pauljulianphd@gmail.com

## BAD 
## https://www.tidyverse.org/articles/2017/12/workflow-vs-script/
## Clears Everything...start fresh.
rm(list=ls(all=T));cat("\014");dev.off()

## Libraries
#devtools::install_github("SwampThingPaul/AnalystHelper")
library(AnalystHelper);
library(plyr)
library(reshape2)
library(openxlsx)

# GIS libraries 
library(rgdal)
library(rgeos)
library(raster)
library(gstat)
library(tmap)
library(spdep)

# Other data and vis tools
library(httr)
library(magrittr)
library(dataRetrieval)
library(maptools)
library(maps)

## Paths
wd="C:/Julian_LaCie/_Github/Windermere"

paths=paste0(wd,c("/Plots/","/Export/","/Data/","/GIS","/src/","/_documents/"))
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]

wgs84=CRS("+proj=longlat +datum=WGS84")
utm30=CRS("+init=epsg:32630"); # WGS84 UTM Zone 30N
tmap_mode("view")


# GIS ---------------------------------------------------------------------
uk=readOGR(paste0(GIS.path,"/UK"),"GBR_adm1")
uk=spTransform(uk,utm30)
# plot(uk)

lakes=spTransform(readOGR(paste0(GIS.path,"/Lakes"),"Habitat_Networks_(England)_-_Lakes"),utm30)
# plot(lakes)

# rivers=spTransform(readOGR(paste0(GIS.path,"/Rivers"),"Priority_River_Habitat___Rivers___Natural_England"),utm30)
# rivers=spTransform(readOGR(paste0(GIS.path,"/Rivers/data"),"WatercourseLink"),utm30)
# plot(rivers)
# tm_shape(rivers)+tm_lines()
# Data --------------------------------------------------------------------
dat=read.xlsx(paste0(data.path,"20221105/Master data for partners and cartographer.xlsx"))
dat$date=as.Date("2022-06-26")

dat=subset(dat,is.na(dat$Longitude)==F)
names(dat)

vars=c("Site.Code", "Grid.Reference", "X", "Y", "Latitude", "Longitude", 
       "Comments", "Sample.collection.time", "Time.at.hub", "temp.degC", 
       "pH", "SPC.uScm", "NOx.mgL", "NO2.mgL", 
       "NO3.mgL", "NH4.mgL", "TP.mgL", "TDP.mgL", 
       "SRP.mgL", "DOC.mgL", "SiO2.mgL", "TColiforms.cfu100mL", 
       "Ecoli.cfu100mL", "Entero.cfu100mL", "WFD.P.status", 
       "Lake.or.stream", "Waterbody.name", "N.or.S.basin.for.Windermere", 
       "E.or.W.for.Windermere", "River.site.description", "date")
colnames(dat)=vars

unique(dat$Waterbody.name)


tmp=ddply(dat,"Waterbody.name",summarise,val=min(Latitude,na.rm=T))
tmp[order(-tmp$val),]

WB.names=data.frame(Waterbody.name=c("Rothay us", "Rydal Water ", "Grasmere", "Stock Ghyll", "Great Langdale Beck", 
                            "Brathay us", "Rothay ds", "Blelham Beck ", "Blelham Beck", "Blelham Tarn", 
                            "Trout Beck", "Black Beck", "Esthwaite Water", "Cunsey Beck", 
                            "Ghyll Head Reservoir", NA, "Windermere", "Leven"),
           alias=as.factor(c("Rothy","Rydal","Grasmere","Stock Ghyll","Great Langdale Beck", 
                   "Brathay", "Rothay", "Blelham Beck", "Blelham Beck", "Blelham Tarn", 
                   "Trout Beck", "Black Beck", "Esthwaite", "Cunsey Beck", 
                   "Ghyll Head Reservoir", NA, "Windermere", "Leven")))

dat=merge(dat,WB.names,'Waterbody.name',all.x=T)
dat$alias=factor(dat$alias,levels=unique(WB.names$alias))

# fix character columns
dat[,22:25]=sapply(dat[,22:25],as.numeric)
boxplot(TP.mgL~alias,dat)

## 
summary(dat)

hist(dat$TP.mgL)

range(dat$NOx.mgL[dat$NOx.mgL!=0],na.rm=T)
range(dat$NO3.mgL[dat$NO3.mgL>0],na.rm=T)
range(dat$Ecoli.cfu100mL[dat$Ecoli.cfu100mL!=0],na.rm=T)
range(dat$Entero.cfu100mL[dat$Entero.cfu100mL!=0],na.rm=T)

## remove zero values (not real data)
dat$NOx.mgL[dat$NOx.mgL==0]=NA
dat$NO3.mgL[dat$NO3.mgL<0]=NA
dat$Ecoli.cfu100mL[dat$Ecoli.cfu100mL==0]=NA
dat$Entero.cfu100mL[dat$Entero.cfu100mL==0]=NA

## Data as shapefile
dat.shp=SpatialPointsDataFrame(dat[,c('Longitude',"Latitude")],
                               dat,
                               proj4string = wgs84)
dat.shp=spTransform(dat.shp,utm30)
# quick explore
tm_shape(dat.shp)+tm_dots()

leg.fun=function(b,pal,leg.title,
                 top.val=0.8,bot.val=0.2,mid.v.val=NULL,
                 x.max=0.3,x.min=0.1,mid.val=NULL,
                 txt.offset.val=-0.01,txt.y=NULL,leg.txt=NULL,
                 txt.cex=0.75,txt.adj=0,txt.pos=4,txt.offset=0.5,
                 title.cex=0.8,title.pos=3,title.adj=0,
                 title.x=NULL,title.y=NULL,
                 leg.type=c("continuous","categorical"), ...){
        l.b=length(b)
        labs=c(paste0("< ",b[2]),paste(b[2:(l.b-2)],b[3:(l.b-1)],sep=" - "),paste(paste0(">",b[(l.b-1)])))
        n.bks=length(b)-1
        mid.v.val=if(is.null(mid.v.val)==T){bot.val+(top.val-bot.val)/2}else{mid.v.val}
        
        mid.val=if(is.null(mid.val)==T){x.min+(x.max-x.min)/2}else{mid.val}
        if(leg.type=="continuous"){
                legend_image=as.raster(matrix(rev(pal),ncol=1))
                rasterImage(legend_image,x.min,bot.val,x.max,top.val)
                txt.y=if(is.null(txt.y)==T){c(bot.val,top.val)}else(txt.y)
                leg.txt=if(is.null(leg.txt)==T){format(c(min(b),max(b)))}else(leg.txt)
                text(x=x.max, y = txt.y, labels =leg.txt,cex=txt.cex,adj=txt.adj,pos=txt.pos,offset=txt.offset, ...)
        }
        if(leg.type=="categorical"){
                bx.val= seq(bot.val,top.val,(top.val-bot.val)/n.bks)
                rect(x.min,bx.val[1:n.bks],x.max,bx.val[2:(n.bks+1)],col=rev(pal),lty=0)
                text(y=bx.val[2:(n.bks+1)]-c(mean(diff(bx.val[2:(n.bks+1)]))/2), x = x.max, 
                     labels = rev(labs),cex=txt.cex,xpd=NA,pos=txt.pos,adj=txt.adj)
        }
        
        title.x=if(is.null(title.x)==T){mid.val}else{title.x}
        title.y=if(is.null(title.y)==T){top.val}else{title.y}
        text(x=title.x,y=title.y,leg.title,adj=title.adj,cex=title.cex,pos=title.pos,xpd=NA)
}

surv.date=as.Date("2022-06-26")
# TP Hotspot -------------------------------------------------------------
TP.dat.shp=subset(dat.shp,is.na(TP.mgL)==F)

sp::bubble(TP.dat.shp,"TP.mgL")

# png(filename=paste0(plot.path,"202206_TPBubble.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,0.5,0.5));
layout(matrix(1:2,1,2),widths=c(1,0.5))

az=sqrt(TP.dat.shp$TP.mgL)
q=sqrt(quantile(TP.dat.shp$TP.mgL))
maxsize = 2.5
cex.val=as.vector(maxsize * az/max(az, q))

bbox.lims=bbox(gBuffer(TP.dat.shp,width=2000))
plot(uk,col="cornsilk",border="grey",bg="lightblue",
     ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.1)
plot(subset(lakes,Class=="Restorable Habitat"),add=T,border=NA,col="lightblue")
plot(TP.dat.shp,pch=21,bg="indianred1",cex=cex.val,add=T,lwd=0.1)
box(lwd=1)
mapmisc::scaleBar(utm30,"bottomright",bty="n",cex=1,seg.len=4)
mtext(side=3,line=-1.25,adj=0,paste0(" Survey: ",format(surv.date,"%d %B %Y")))

plot(0:1,0:1,ann=F,axes=F,type="n")
b=format(round(q^2,4))
q.cex = as.vector(maxsize * q/max(az, q))
l.b=length(b)
labs=c(paste(b[1:(l.b-1)],b[2:(l.b)],sep=" - "))
legend("center",legend=c(b),
       pch=c(21),lty=c(NA),lwd=c(0.1),
       pt.bg="indianred1",pt.cex=q.cex,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1,
       title="Total Phosphorus (mg L\u207B\u00B9)",title.adj = 0)
dev.off()

# png(filename=paste0(plot.path,"202206_TPBoxplot.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(4,1,0.5,0.5),oma=c(4,3,0.5,0.1));
ylim.val=c(0,0.10);by.y=0.02;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

x=boxplot(TP.mgL~alias,dat,outline=F,ylim=ylim.val,axes=F,ann=F)
axis_fun(1,1:length(x$names),1:length(x$names),x$names,las=2,cex=0.75)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=2,line=3,"Total Phosphorus (mg L\u207B\u00B9)")
dev.off()

ptdist=pointDistance(TP.dat.shp)

min.dist<-min(ptdist,na.rm=T); # Minimum
mean.dist<-mean(ptdist,na.rm=T); # Mean

nb<-dnearneigh(coordinates(TP.dat.shp),min.dist,mean.dist)

#global
nb_lw=nb2listw(nb,style="B")
globalG.test(TP.dat.shp$TP.mgL,nb_lw,alternative="less")

nb_lw<-nb2listw(nb)

# local G
local_g<-localG(TP.dat.shp$TP.mgL,nb_lw)

# convert to matrix
local_g.ma=as.matrix(local_g)

# column-bind the local_g data
TP.dat.shp<-cbind(TP.dat.shp,local_g.ma)

# change the names of the new column
names(TP.dat.shp)[ncol(TP.dat.shp)]="localg"

TP.dat.shp$pval<- 2*pnorm(-abs(TP.dat.shp$localg))

subset(TP.dat.shp,pval<0.05)

AOI=raster::extent(gBuffer(TP.dat.shp,width=3000))
AOI.poly=as(AOI,"SpatialPolygons")
proj4string(AOI.poly)=utm30


# png(filename=paste0(plot.path,"202206_TPHotspot.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(1,1,1,1));
layout(matrix(1:2,1,2),widths=c(1,0.5))

int=c(-3,-2,-1,0,1,2,3.2)
pal=colorRampPalette(c("blue","grey","red"))(length(int)-1)
# pal=hcl.colors(length(int)-1, "viridis", rev = F,alpha=0.7)
cols.val=findInterval(TP.dat.shp$localg,int)
pch.vals=with(TP.dat.shp@data,ifelse(pval<0.05,21,22))
cex.ind=1# c(2,1)
cex.vals=1 #with(TP.dat.shp@data,ifelse(pval<0.05,cex.ind[1],cex.ind[2]))

bbox.lims=bbox(gBuffer(TP.dat.shp,width=2000))
plot(uk,col="cornsilk",border="grey",bg="lightblue",
     ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.1)
# plot(rivers,add=T,border=NA,col="lightblue")
plot(subset(lakes,Class=="Restorable Habitat"),add=T,border=NA,col="lightblue")
plot(TP.dat.shp,pch=pch.vals,bg=pal[cols.val],cex=cex.vals,add=T,lwd=0.1)
box(lwd=1)
mapmisc::scaleBar(utm30,"bottomright",bty="n",cex=1,seg.len=4)
mtext(side=3,line=-1.25,adj=0,paste0(" Survey: ",format(surv.date,"%d %B %Y")))

plot(0:1,0:1,ann=F,axes=F,type="n")
n=length(int)-1
labs=c("< -2.0","-2.0 - -1.0","-1.0 - 0.0","0.0 - 1.0", "1.0 - 2.0","> 2.0")#NA
#for(i in 1:n){labs[i]=paste(format(round(int.bks[i],2),nsmall=2),format(round(int.bks[i+1],2),nsmall=2),sep=" - ")}
bx.val= seq(0.4,0.85,(0.85-0.4)/n)
rect(0.15,bx.val[1:n],0.25,bx.val[2:(n+1)],col=rev(pal),lty=0)
text(x=0.25, y = bx.val[2:(n+1)]-c(mean(diff(bx.val[2:(n+1)]))/2), labels = rev(labs),cex=0.75,adj=0,pos=4)
text(x=0.15,y=0.90,expression(paste("Total Phosphorus\nLocal Getis-Ord G"["i"]^"*")),adj=0,cex=1)

legend(0.5,0.4,legend=c("< 0.05"," > 0.05"),pch=c(21,22),lty=c(NA),lwd=c(0.1),
       pt.bg="grey",pt.cex=cex.ind,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1,
       title="\u03C1-value",title.adj = 0)
dev.off()


# NOx Hotspot ------------------------------------------------------------
NOx.dat.shp=subset(dat.shp,is.na(NOx.mgL)==F)

sp::bubble(NOx.dat.shp,"NOx.mgL")

# png(filename=paste0(plot.path,"202206_NOxBubble.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,0.5,0.5));
layout(matrix(1:2,1,2),widths=c(1,0.5))

az=sqrt(NOx.dat.shp$NOx.mgL)
q=sqrt(quantile(NOx.dat.shp$NOx.mgL))
maxsize = 2
cex.val=as.vector(maxsize * az/max(az, q))

bbox.lims=bbox(gBuffer(NOx.dat.shp,width=2000))
plot(uk,col="cornsilk",border="grey",bg="lightblue",
     ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.1)
plot(subset(lakes,Class=="Restorable Habitat"),add=T,border=NA,col="lightblue")
plot(NOx.dat.shp,pch=21,bg="indianred1",cex=cex.val,add=T,lwd=0.1)
box(lwd=1)
mapmisc::scaleBar(utm30,"bottomright",bty="n",cex=1,seg.len=4)
mtext(side=3,line=-1.25,adj=0,paste0(" Survey: ",format(surv.date,"%d %B %Y")))

plot(0:1,0:1,ann=F,axes=F,type="n")
b=format(round(q^2,3))
q.cex = as.vector(maxsize * q/max(az, q))
l.b=length(b)
labs=c(paste(b[1:(l.b-1)],b[2:(l.b)],sep=" - "))
legend("center",legend=c(b),
       pch=c(21),lty=c(NA),lwd=c(0.1),
       pt.bg="indianred1",pt.cex=q.cex,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1,
       title="Nitrate-Nitrite (mg L\u207B\u00B9)",title.adj = 0)
dev.off()

# png(filename=paste0(plot.path,"202206_NOxBoxplot.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(4,1,0.5,0.5),oma=c(4,3,0.5,0.1));
ylim.val=c(0,1);by.y=0.25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

x=boxplot(NOx.mgL~alias,dat,outline=F,ylim=ylim.val,axes=F,ann=F)
axis_fun(1,1:length(x$names),1:length(x$names),x$names,las=2,cex=0.75)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=2,line=3,"Nitrate-Nitrite (mg L\u207B\u00B9)")
dev.off()

ptdist=pointDistance(NOx.dat.shp)

min.dist<-min(ptdist,na.rm=T); # Minimum
mean.dist<-mean(ptdist,na.rm=T); # Mean

nb<-dnearneigh(coordinates(NOx.dat.shp),min.dist,mean.dist)

#global
nb_lw=nb2listw(nb,style="B")
globalG.test(NOx.dat.shp$NOx.mgL,nb_lw,alternative="two.sided")

nb_lw<-nb2listw(nb)

# local G
local_g<-localG(NOx.dat.shp$NOx.mgL,nb_lw)

# convert to matrix
local_g.ma=as.matrix(local_g)

# column-bind the local_g data
NOx.dat.shp<-cbind(NOx.dat.shp,local_g.ma)

# change the names of the new column
names(NOx.dat.shp)[ncol(NOx.dat.shp)]="localg"

NOx.dat.shp$pval<- 2*pnorm(-abs(NOx.dat.shp$localg))

subset(NOx.dat.shp,pval<0.05)



# png(filename=paste0(plot.path,"202206_NOxHotspot.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(1,1,1,1));
layout(matrix(1:2,1,2),widths=c(1,0.5))

int=c(-4,-2,-1,0,1,2,5)
pal=colorRampPalette(c("blue","grey","red"))(length(int)-1)
# pal=hcl.colors(length(int)-1, "viridis", rev = F,alpha=0.7)
cols.val=findInterval(NOx.dat.shp$localg,int)
pch.vals=with(NOx.dat.shp@data,ifelse(pval<0.05,21,22))
cex.ind=1# c(2,1)
cex.vals=1 #with(NOx.dat.shp@data,ifelse(pval<0.05,cex.ind[1],cex.ind[2]))

bbox.lims=bbox(gBuffer(NOx.dat.shp,width=2000))
plot(uk,col="cornsilk",border="grey",bg="lightblue",
     ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.1)
# plot(rivers,add=T,border=NA,col="lightblue")
plot(subset(lakes,Class=="Restorable Habitat"),add=T,border=NA,col="lightblue")
plot(NOx.dat.shp,pch=pch.vals,bg=pal[cols.val],cex=cex.vals,add=T,lwd=0.1)
box(lwd=1)
mapmisc::scaleBar(utm30,"bottomright",bty="n",cex=1,seg.len=4)
mtext(side=3,line=-1.25,adj=0,paste0(" Survey: ",format(surv.date,"%d %B %Y")))

plot(0:1,0:1,ann=F,axes=F,type="n")
n=length(int)-1
labs=c("< -4.0","-2.0 - -1.0","-1.0 - 0.0","0.0 - 1.0", "1.0 - 2.0","> 2.0")#NA
#for(i in 1:n){labs[i]=paste(format(round(int.bks[i],2),nsmall=2),format(round(int.bks[i+1],2),nsmall=2),sep=" - ")}
bx.val= seq(0.4,0.85,(0.85-0.4)/n)
rect(0.15,bx.val[1:n],0.25,bx.val[2:(n+1)],col=rev(pal),lty=0)
text(x=0.25, y = bx.val[2:(n+1)]-c(mean(diff(bx.val[2:(n+1)]))/2), labels = rev(labs),cex=0.75,adj=0,pos=4)
text(x=0.15,y=0.90,expression(paste("Nitrate-Nitrite\nLocal Getis-Ord G"["i"]^"*")),adj=0,cex=1)

legend(0.5,0.4,legend=c("< 0.05"," > 0.05"),pch=c(21,22),lty=c(NA),lwd=c(0.1),
       pt.bg="grey",pt.cex=cex.ind,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1,
       title="\u03C1-value",title.adj = 0)
dev.off()

# Si Hotspot ------------------------------------------------------------
Si.dat.shp=subset(dat.shp,is.na(SiO2.mgL)==F)

sp::bubble(Si.dat.shp,"SiO2.mgL")

# png(filename=paste0(plot.path,"202206_SiBubble.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,0.5,0.5));
layout(matrix(1:2,1,2),widths=c(1,0.5))

az=sqrt(Si.dat.shp$SiO2.mgL)
q=sqrt(quantile(Si.dat.shp$SiO2.mgL))
maxsize = 2
cex.val=as.vector(maxsize * az/max(az, q))

bbox.lims=bbox(gBuffer(Si.dat.shp,width=2000))
plot(uk,col="cornsilk",border="grey",bg="lightblue",
     ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.1)
plot(subset(lakes,Class=="Restorable Habitat"),add=T,border=NA,col="lightblue")
plot(Si.dat.shp,pch=21,bg="indianred1",cex=cex.val,add=T,lwd=0.1)
box(lwd=1)
mapmisc::scaleBar(utm30,"bottomright",bty="n",cex=1,seg.len=4)
mtext(side=3,line=-1.25,adj=0,paste0(" Survey: ",format(surv.date,"%d %B %Y")))

plot(0:1,0:1,ann=F,axes=F,type="n")
b=format(round(q^2,2))
q.cex = as.vector(maxsize * q/max(az, q))
l.b=length(b)
labs=c(paste(b[1:(l.b-1)],b[2:(l.b)],sep=" - "))
legend("center",legend=c(b),
       pch=c(21),lty=c(NA),lwd=c(0.1),
       pt.bg="indianred1",pt.cex=q.cex,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1,
       title="Silica (mg L\u207B\u00B9)",title.adj = 0)
dev.off()

# png(filename=paste0(plot.path,"202206_NOxBoxplot.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(4,1,0.5,0.5),oma=c(4,3,0.5,0.1));
ylim.val=c(0,5);by.y=1;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

x=boxplot(SiO2.mgL~alias,dat,outline=F,ylim=ylim.val,axes=F,ann=F)
axis_fun(1,1:length(x$names),1:length(x$names),x$names,las=2,cex=0.75)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=2,line=3,"Silica (mg L\u207B\u00B9)")
dev.off()


# TColiforms Hotspot ------------------------------------------------------------
TColiforms.dat.shp=subset(dat.shp,is.na(TColiforms.cfu100mL)==F)

sp::bubble(TColiforms.dat.shp,"TColiforms.cfu100mL")

# png(filename=paste0(plot.path,"202206_TColiformsBubble.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,0.5,0.5));
layout(matrix(1:2,1,2),widths=c(1,0.5))

az=sqrt(TColiforms.dat.shp$TColiforms.cfu100mL)
q=sqrt(quantile(TColiforms.dat.shp$TColiforms.cfu100mL))
maxsize = 2
cex.val=as.vector(maxsize * az/max(az, q))

bbox.lims=bbox(gBuffer(TColiforms.dat.shp,width=2000))
plot(uk,col="cornsilk",border="grey",bg="lightblue",
     ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.1)
plot(subset(lakes,Class=="Restorable Habitat"),add=T,border=NA,col="lightblue")
plot(TColiforms.dat.shp,pch=21,bg="indianred1",cex=cex.val,add=T,lwd=0.1)
box(lwd=1)
mapmisc::scaleBar(utm30,"bottomright",bty="n",cex=1,seg.len=4)
mtext(side=3,line=-1.25,adj=0,paste0(" Survey: ",format(surv.date,"%d %B %Y")))

plot(0:1,0:1,ann=F,axes=F,type="n")
b=format(round(q^2,3))
q.cex = as.vector(maxsize * q/max(az, q))
l.b=length(b)
labs=c(paste(b[1:(l.b-1)],b[2:(l.b)],sep=" - "))
legend("center",legend=c(b),
       pch=c(21),lty=c(NA),lwd=c(0.1),
       pt.bg="indianred1",pt.cex=q.cex,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1,
       title="Total Coliform\n(CFU 100 mL\u207B\u00B9)",title.adj = 0)
dev.off()

# png(filename=paste0(plot.path,"202206_TColiformsBoxplot.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(4,1,0.5,0.5),oma=c(4,3,0.5,0.1));
ylim.val=c(9,10000);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")# by.y=0.25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

x=boxplot(TColiforms.cfu100mL~alias,dat,outline=F,ylim=ylim.val,axes=F,ann=F,log="y")
axis_fun(1,1:length(x$names),1:length(x$names),x$names,las=2,cex=0.75)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=2,line=3,"Total Coliform (CFU 100 mL\u207B\u00B9)")
dev.off()

ptdist=pointDistance(TColiforms.dat.shp)

min.dist<-min(ptdist,na.rm=T); # Minimum
mean.dist<-mean(ptdist,na.rm=T); # Mean

nb<-dnearneigh(coordinates(TColiforms.dat.shp),min.dist,mean.dist)

#global
nb_lw=nb2listw(nb,style="B")
globalG.test(TColiforms.dat.shp$TColiforms.cfu100mL,nb_lw,alternative="two.sided")

nb_lw<-nb2listw(nb)

# local G
local_g<-localG(TColiforms.dat.shp$TColiforms.cfu100mL,nb_lw)

# convert to matrix
local_g.ma=as.matrix(local_g)

# column-bind the local_g data
TColiforms.dat.shp<-cbind(TColiforms.dat.shp,local_g.ma)

# change the names of the new column
names(TColiforms.dat.shp)[ncol(TColiforms.dat.shp)]="localg"

TColiforms.dat.shp$pval<- 2*pnorm(-abs(TColiforms.dat.shp$localg))

subset(TColiforms.dat.shp,pval<0.05)
range(TColiforms.dat.shp$localg)


# png(filename=paste0(plot.path,"202206_TColiformsHotspot.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(1,1,1,1));
layout(matrix(1:2,1,2),widths=c(1,0.5))

int=c(-4.0,-2,-1,0,1,2,5)
pal=colorRampPalette(c("blue","grey","red"))(length(int)-1)
# pal=hcl.colors(length(int)-1, "viridis", rev = F,alpha=0.7)
cols.val=findInterval(TColiforms.dat.shp$localg,int)
pch.vals=with(TColiforms.dat.shp@data,ifelse(pval<0.05,21,22))
cex.ind=1# c(2,1)
cex.vals=1 #with(TColiforms.dat.shp@data,ifelse(pval<0.05,cex.ind[1],cex.ind[2]))

bbox.lims=bbox(gBuffer(TColiforms.dat.shp,width=2000))
plot(uk,col="cornsilk",border="grey",bg="lightblue",
     ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.1)
# plot(rivers,add=T,border=NA,col="lightblue")
plot(subset(lakes,Class=="Restorable Habitat"),add=T,border=NA,col="lightblue")
plot(TColiforms.dat.shp,pch=pch.vals,bg=pal[cols.val],cex=cex.vals,add=T,lwd=0.1)
box(lwd=1)
mapmisc::scaleBar(utm30,"bottomright",bty="n",cex=1,seg.len=4)
mtext(side=3,line=-1.25,adj=0,paste0(" Survey: ",format(surv.date,"%d %B %Y")))

plot(0:1,0:1,ann=F,axes=F,type="n")
n=length(int)-1
labs=c("< -4.0","-2.0 - -1.0","-1.0 - 0.0","0.0 - 1.0", "1.0 - 2.0","> 2.0")#NA
#for(i in 1:n){labs[i]=paste(format(round(int.bks[i],2),nsmall=2),format(round(int.bks[i+1],2),nsmall=2),sep=" - ")}
bx.val= seq(0.4,0.85,(0.85-0.4)/n)
rect(0.15,bx.val[1:n],0.25,bx.val[2:(n+1)],col=rev(pal),lty=0)
text(x=0.25, y = bx.val[2:(n+1)]-c(mean(diff(bx.val[2:(n+1)]))/2), labels = rev(labs),cex=0.75,adj=0,pos=4)
text(x=0.15,y=0.90,expression(paste("Total Coliforms\nLocal Getis-Ord G"["i"]^"*")),adj=0,cex=1)

legend(0.5,0.4,legend=c("< 0.05"," > 0.05"),pch=c(21,22),lty=c(NA),lwd=c(0.1),
       pt.bg="grey",pt.cex=cex.ind,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1,
       title="\u03C1-value",title.adj = 0)
dev.off()

# Ecoli Hotspot ------------------------------------------------------------
Ecoli.dat.shp=subset(dat.shp,is.na(Ecoli.cfu100mL)==F)

sp::bubble(Ecoli.dat.shp,"Ecoli.cfu100mL")

# png(filename=paste0(plot.path,"202206_EcoliBubble.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,0.5,0.5));
layout(matrix(1:2,1,2),widths=c(1,0.5))

az=sqrt(Ecoli.dat.shp$Ecoli.cfu100mL)
q=sqrt(quantile(Ecoli.dat.shp$Ecoli.cfu100mL))
maxsize = 2
cex.val=as.vector(maxsize * az/max(az, q))

bbox.lims=bbox(gBuffer(Ecoli.dat.shp,width=2000))
plot(uk,col="cornsilk",border="grey",bg="lightblue",
     ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.1)
plot(subset(lakes,Class=="Restorable Habitat"),add=T,border=NA,col="lightblue")
plot(Ecoli.dat.shp,pch=21,bg="indianred1",cex=cex.val,add=T,lwd=0.1)
box(lwd=1)
mapmisc::scaleBar(utm30,"bottomright",bty="n",cex=1,seg.len=4)
mtext(side=3,line=-1.25,adj=0,paste0(" Survey: ",format(surv.date,"%d %B %Y")))

plot(0:1,0:1,ann=F,axes=F,type="n")
b=format(round(q^2,3))
q.cex = as.vector(maxsize * q/max(az, q))
l.b=length(b)
labs=c(paste(b[1:(l.b-1)],b[2:(l.b)],sep=" - "))
legend("center",legend=c(b),
       pch=c(21),lty=c(NA),lwd=c(0.1),
       pt.bg="indianred1",pt.cex=q.cex,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1,
       title="E. coli\n(CFU 100 mL\u207B\u00B9)",title.adj = 0)
dev.off()

# png(filename=paste0(plot.path,"202206_EcoliBoxplot.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(4,1,0.5,0.5),oma=c(4,3,0.5,0.1));
ylim.val=c(1,10000);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")# by.y=0.25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

x=boxplot(Ecoli.cfu100mL~alias,dat,outline=F,ylim=ylim.val,axes=F,ann=F,log="y")
axis_fun(1,1:length(x$names),1:length(x$names),x$names,las=2,cex=0.75)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=2,line=3,"E. coli (CFU 100 mL\u207B\u00B9)")
dev.off()

ptdist=pointDistance(Ecoli.dat.shp)

min.dist<-min(ptdist,na.rm=T); # Minimum
mean.dist<-mean(ptdist,na.rm=T); # Mean

nb<-dnearneigh(coordinates(Ecoli.dat.shp),min.dist,mean.dist)

#global
nb_lw=nb2listw(nb,style="B")
globalG.test(Ecoli.dat.shp$Ecoli.cfu100mL,nb_lw,alternative="two.sided")

nb_lw<-nb2listw(nb)

# local G
local_g<-localG(Ecoli.dat.shp$Ecoli.cfu100mL,nb_lw)

# convert to matrix
local_g.ma=as.matrix(local_g)

# column-bind the local_g data
Ecoli.dat.shp<-cbind(Ecoli.dat.shp,local_g.ma)

# change the names of the new column
names(Ecoli.dat.shp)[ncol(Ecoli.dat.shp)]="localg"

Ecoli.dat.shp$pval<- 2*pnorm(-abs(Ecoli.dat.shp$localg))

subset(Ecoli.dat.shp,pval<0.05)
range(Ecoli.dat.shp$localg)


# png(filename=paste0(plot.path,"202206_EcoliHotspot.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(1,1,1,1));
layout(matrix(1:2,1,2),widths=c(1,0.5))

int=c(-4.0,-2,-1,0,1,2,5)
pal=colorRampPalette(c("blue","grey","red"))(length(int)-1)
# pal=hcl.colors(length(int)-1, "viridis", rev = F,alpha=0.7)
cols.val=findInterval(Ecoli.dat.shp$localg,int)
pch.vals=with(Ecoli.dat.shp@data,ifelse(pval<0.05,21,22))
cex.ind=1# c(2,1)
cex.vals=1 #with(Ecoli.dat.shp@data,ifelse(pval<0.05,cex.ind[1],cex.ind[2]))

bbox.lims=bbox(gBuffer(Ecoli.dat.shp,width=2000))
plot(uk,col="cornsilk",border="grey",bg="lightblue",
     ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.1)
# plot(rivers,add=T,border=NA,col="lightblue")
plot(subset(lakes,Class=="Restorable Habitat"),add=T,border=NA,col="lightblue")
plot(Ecoli.dat.shp,pch=pch.vals,bg=pal[cols.val],cex=cex.vals,add=T,lwd=0.1)
box(lwd=1)
mapmisc::scaleBar(utm30,"bottomright",bty="n",cex=1,seg.len=4)
mtext(side=3,line=-1.25,adj=0,paste0(" Survey: ",format(surv.date,"%d %B %Y")))

plot(0:1,0:1,ann=F,axes=F,type="n")
n=length(int)-1
labs=c("< -4.0","-2.0 - -1.0","-1.0 - 0.0","0.0 - 1.0", "1.0 - 2.0","> 2.0")#NA
#for(i in 1:n){labs[i]=paste(format(round(int.bks[i],2),nsmall=2),format(round(int.bks[i+1],2),nsmall=2),sep=" - ")}
bx.val= seq(0.4,0.85,(0.85-0.4)/n)
rect(0.15,bx.val[1:n],0.25,bx.val[2:(n+1)],col=rev(pal),lty=0)
text(x=0.25, y = bx.val[2:(n+1)]-c(mean(diff(bx.val[2:(n+1)]))/2), labels = rev(labs),cex=0.75,adj=0,pos=4)
text(x=0.15,y=0.90,expression(paste("E. coli\nLocal Getis-Ord G"["i"]^"*")),adj=0,cex=1)

legend(0.5,0.4,legend=c("< 0.05"," > 0.05"),pch=c(21,22),lty=c(NA),lwd=c(0.1),
       pt.bg="grey",pt.cex=cex.ind,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1,
       title="\u03C1-value",title.adj = 0)
dev.off()

# Entero Hotspot ------------------------------------------------------------
Entero.dat.shp=subset(dat.shp,is.na(Entero.cfu100mL)==F)

sp::bubble(Entero.dat.shp,"Entero.cfu100mL")

# png(filename=paste0(plot.path,"202206_EnteroBubble.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,0.5,0.5));
layout(matrix(1:2,1,2),widths=c(1,0.5))

az=sqrt(Entero.dat.shp$Entero.cfu100mL)
q=sqrt(quantile(Entero.dat.shp$Entero.cfu100mL))
maxsize = 2
cex.val=as.vector(maxsize * az/max(az, q))

bbox.lims=bbox(gBuffer(Entero.dat.shp,width=2000))
plot(uk,col="cornsilk",border="grey",bg="lightblue",
     ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.1)
plot(subset(lakes,Class=="Restorable Habitat"),add=T,border=NA,col="lightblue")
plot(Entero.dat.shp,pch=21,bg="indianred1",cex=cex.val,add=T,lwd=0.1)
box(lwd=1)
mapmisc::scaleBar(utm30,"bottomright",bty="n",cex=1,seg.len=4)
mtext(side=3,line=-1.25,adj=0,paste0(" Survey: ",format(surv.date,"%d %B %Y")))

plot(0:1,0:1,ann=F,axes=F,type="n")
b=format(round(q^2,3))
q.cex = as.vector(maxsize * q/max(az, q))
l.b=length(b)
labs=c(paste(b[1:(l.b-1)],b[2:(l.b)],sep=" - "))
legend("center",legend=c(b),
       pch=c(21),lty=c(NA),lwd=c(0.1),
       pt.bg="indianred1",pt.cex=q.cex,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1,
       title="Enterococcus\n(CFU 100 mL\u207B\u00B9)",title.adj = 0)
dev.off()

# png(filename=paste0(plot.path,"202206_EnteroBoxplot.png"),width=6.5,height=4.5,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(4,1,0.5,0.5),oma=c(4,3,0.5,0.1));
ylim.val=c(1,1000);ymaj=log.scale.fun(ylim.val,"major");ymin=log.scale.fun(ylim.val,"minor")# by.y=0.25;ymaj=seq(ylim.val[1],ylim.val[2],by.y);ymin=seq(ylim.val[1],ylim.val[2],by.y/2)

x=boxplot(Entero.cfu100mL~alias,dat,outline=F,ylim=ylim.val,axes=F,ann=F,log="y")
axis_fun(1,1:length(x$names),1:length(x$names),x$names,las=2,cex=0.75)
axis_fun(2,ymaj,ymin,format(ymaj));box(lwd=1)
mtext(side=2,line=3,"Enterococcus (CFU 100 mL\u207B\u00B9)")
dev.off()

ptdist=pointDistance(Entero.dat.shp)

min.dist<-min(ptdist,na.rm=T); # Minimum
mean.dist<-mean(ptdist,na.rm=T); # Mean

nb<-dnearneigh(coordinates(Entero.dat.shp),min.dist,mean.dist)

#global
nb_lw=nb2listw(nb,style="B")
globalG.test(Entero.dat.shp$Entero.cfu100mL,nb_lw,alternative="two.sided")

nb_lw<-nb2listw(nb)

# local G
local_g<-localG(Entero.dat.shp$Entero.cfu100mL,nb_lw)

# convert to matrix
local_g.ma=as.matrix(local_g)

# column-bind the local_g data
Entero.dat.shp<-cbind(Entero.dat.shp,local_g.ma)

# change the names of the new column
names(Entero.dat.shp)[ncol(Entero.dat.shp)]="localg"

Entero.dat.shp$pval<- 2*pnorm(-abs(Entero.dat.shp$localg))

subset(Entero.dat.shp,pval<0.05)
range(Entero.dat.shp$localg)


# png(filename=paste0(plot.path,"202206_EnteroHotspot.png"),width=6.5,height=4,units="in",res=200,type="windows",bg="white")
par(family="serif",mar=c(0.1,0.1,0.1,0.1),oma=c(1,1,1,1));
layout(matrix(1:2,1,2),widths=c(1,0.5))

int=c(-4.0,-2,-1,0,1,2,5)
pal=colorRampPalette(c("blue","grey","red"))(length(int)-1)
# pal=hcl.colors(length(int)-1, "viridis", rev = F,alpha=0.7)
cols.val=findInterval(Entero.dat.shp$localg,int)
pch.vals=with(Entero.dat.shp@data,ifelse(pval<0.05,21,22))
cex.ind=1# c(2,1)
cex.vals=1 #with(Entero.dat.shp@data,ifelse(pval<0.05,cex.ind[1],cex.ind[2]))

bbox.lims=bbox(gBuffer(Entero.dat.shp,width=2000))
plot(uk,col="cornsilk",border="grey",bg="lightblue",
     ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.1)
# plot(rivers,add=T,border=NA,col="lightblue")
plot(subset(lakes,Class=="Restorable Habitat"),add=T,border=NA,col="lightblue")
plot(Entero.dat.shp,pch=pch.vals,bg=pal[cols.val],cex=cex.vals,add=T,lwd=0.1)
box(lwd=1)
mapmisc::scaleBar(utm30,"bottomright",bty="n",cex=1,seg.len=4)
mtext(side=3,line=-1.25,adj=0,paste0(" Survey: ",format(surv.date,"%d %B %Y")))

plot(0:1,0:1,ann=F,axes=F,type="n")
n=length(int)-1
labs=c("< -4.0","-2.0 - -1.0","-1.0 - 0.0","0.0 - 1.0", "1.0 - 2.0","> 2.0")#NA
#for(i in 1:n){labs[i]=paste(format(round(int.bks[i],2),nsmall=2),format(round(int.bks[i+1],2),nsmall=2),sep=" - ")}
bx.val= seq(0.4,0.85,(0.85-0.4)/n)
rect(0.15,bx.val[1:n],0.25,bx.val[2:(n+1)],col=rev(pal),lty=0)
text(x=0.25, y = bx.val[2:(n+1)]-c(mean(diff(bx.val[2:(n+1)]))/2), labels = rev(labs),cex=0.75,adj=0,pos=4)
text(x=0.15,y=0.90,expression(paste("Enterococcus\nLocal Getis-Ord G"["i"]^"*")),adj=0,cex=1)

legend(0.5,0.4,legend=c("< 0.05"," > 0.05"),pch=c(21,22),lty=c(NA),lwd=c(0.1),
       pt.bg="grey",pt.cex=cex.ind,ncol=1,cex=1,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1,
       title="\u03C1-value",title.adj = 0)
dev.off()