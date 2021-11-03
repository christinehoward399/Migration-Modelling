#############################################
### Plot out migration simulation results ###
#############################################

####################
## 1 ## Load data ##
####################

## Main directories
which.pc<-1 # 1 is Swiss, 2 is laptop
folder<-switch(which.pc,"C://Users/TM","C://Users/Tom")
main.dir<-paste0(folder,"/Dropbox/Work/Postdocs/Durham/Migration project")
data.dir<-paste0(folder,"/Dropbox/Work/Postdocs/Durham/Migration project/Data")
data.dir2<-paste0(folder,"/Dropbox/Work/Postdocs/Durham/RSPB FCS project/Data")
data.dir3<-paste0(folder,"/Dropbox/Work/Postdocs/Vogelwarte/Dynamic migration model/Data")

## Load mapping data
# Merge data with shapefile
hex.grid1<-readOGR(dsn=data.dir,layer="Hex grid 500km")
flyway1<-readOGR(dsn=data.dir,layer="Flyway polygon 2020")
head(hex.grid1)


## Load geolocator data

#a# Pied fly
pfly.geo<-read.csv(paste0(data.dir,"/Geolocator data/Stopover_table_PIEFL.csv"))
inds1<-unique(pfly.geo$Tag)
#Impute blanks
# for (x in 2:(length(pfly.geo$SO_median_long)-1)){
#   if(is.na(pfly.geo$SO_median_long[x])==T & is.na(pfly.geo$SO_median_long[x-1])==F & is.na(pfly.geo$SO_median_long[x+1])==F){
#     pfly.geo$SO_median_long[x]<-mean(pfly.geo$SO_median_long[x-1],pfly.geo$SO_median_long[x+1])
#     }
#   if(is.na(pfly.geo$SO_median_lat[x])==T & is.na(pfly.geo$SO_median_lat[x-1])==F & is.na(pfly.geo$SO_median_lat[x+1])==F){
#     pfly.geo$SO_median_lat[x]<-mean(pfly.geo$SO_median_lat[x-1],pfly.geo$SO_median_lat[x+1])
#     }
# }
# Remove NAs
pfly.geo<-pfly.geo[is.na(pfly.geo$SO_median_lat)==F,]
pfly.geo<-pfly.geo[is.na(pfly.geo$SO_median_long)==F,]
# Spatialise
coords<-cbind(pfly.geo$SO_median_long,pfly.geo$SO_median_lat)
sp<-SpatialPoints(coords)
# make spatial data frame
spdf<-SpatialPointsDataFrame(coords, pfly.geo)
projection(spdf)<-CRS("+init=epsg:4326")
spdf1<-spTransform(spdf,CRS(proj4string(hex.grid1)))
# Get coords
pfly.geo<-cbind(pfly.geo,coordinates(spdf1))
# Compute mean stopovers
countries<-unique(pfly.geo$country)
# Mean
pfly.geo.mean<-data.frame(stop1=apply(pfly.geo[pfly.geo$country %in% countries[c(3,4,6,7,8)],c('coords.x1','coords.x2')],2,mean),
                          stop2=apply(pfly.geo[pfly.geo$country %in% countries[c(10,11)],c('coords.x1','coords.x2')],2,mean),
                          stop3=apply(pfly.geo[pfly.geo$country %in% countries[c(9,12)],c('coords.x1','coords.x2')],2,mean),
                          stop4=apply(pfly.geo[pfly.geo$country %in% countries[c(2,5)],c('coords.x1','coords.x2')],2,mean),
                          stop5=apply(pfly.geo[pfly.geo$country %in% countries[1],c('coords.x1','coords.x2')],2,mean))
# SD
pfly.geo.sd<-data.frame(stop1=apply(pfly.geo[pfly.geo$country %in% countries[c(3,4,6,7,8)],c('coords.x1','coords.x2')],2,sd),
                        stop2=apply(pfly.geo[pfly.geo$country %in% countries[c(10,11)],c('coords.x1','coords.x2')],2,sd),
                        stop3=apply(pfly.geo[pfly.geo$country %in% countries[c(9,12)],c('coords.x1','coords.x2')],2,sd),
                        stop4=apply(pfly.geo[pfly.geo$country %in% countries[c(2,5)],c('coords.x1','coords.x2')],2,sd),
                        stop5=apply(pfly.geo[pfly.geo$country %in% countries[1],c('coords.x1','coords.x2')],2,sd))

#b# Collared fly
cfly.geo<-read.csv(paste0(data.dir,"/Geolocator data/Collared flycatcher geo from Adamik et al Sci Rep.csv"))
# Spatialise
coords<-cbind(cfly.geo$x,cfly.geo$y)
sp<-SpatialPoints(coords)
# make spatial data frame
spdf<-SpatialPointsDataFrame(coords,cfly.geo)
projection(spdf)<-CRS("+init=epsg:4326")
spdf1<-spTransform(spdf,CRS(proj4string(hex.grid1)))
# Get coords
cfly.geo<-cbind(cfly.geo,coordinates(spdf1))
points(cfly.geo,col=2,pch=16)

#c# Cuckoo
cuckoo.geo<-read.csv(paste0(data.dir,"/Geolocator data/stopover_table_bestofday_2018_1daymin_recalc_biomes_EXPORT_CHRIS.csv"))
cuckoo.geo$year.start<-substr(cuckoo.geo$SO_start,7,10)
cuckoo.geo$year.end<-substr(cuckoo.geo$SO_end,7,10)
cuckoo.geo$month.start<-substr(cuckoo.geo$SO_start,4,5)
cuckoo.geo$month.end<-substr(cuckoo.geo$SO_end,4,5)
#cuckoo.geo<-cuckoo.geo[cuckoo.geo$SO_length_discrete_day>=30|cuckoo.geo$country=="United Kingdom",] # Only long stops, or UK!
cuckoo.geo<-cuckoo.geo[cuckoo.geo$dead=="N",]
head(cuckoo.geo)
# Individuals
inds2<-unique(cuckoo.geo$name)
# Remove NAs
cuckoo.geo<-cuckoo.geo[is.na(cuckoo.geo$SO_median_lat)==F,]
cuckoo.geo<-cuckoo.geo[is.na(cuckoo.geo$SO_median_long)==F,]
# Spatialise
coords<-cbind(cuckoo.geo$SO_median_long,cuckoo.geo$SO_median_lat)
sp<-SpatialPoints(coords)
# make spatial data frame
spdf<-SpatialPointsDataFrame(coords, cuckoo.geo)
projection(spdf)<-CRS("+init=epsg:4326")
spdf1<-spTransform(spdf,CRS(proj4string(hex.grid1)))
# Get coords
cuckoo.geo<-cbind(cuckoo.geo,coordinates(spdf1))


###########################################
### 2 ### Plots of migration simulations ##
###########################################

# Set up plotting envt
check.install.packages("viridis")

# Merge grid and ndvi
hex.grid.ndvi<-merge(hex.grid1,ndvi.simpl,by="cell")
hex.grid.ndvi1<-hex.grid.ndvi[hex.grid.ndvi$land==1,]

# Set up plotting envt
pal<-viridis(length(seq(-0.1,max(hex.grid.ndvi1@data[,"1"])+.1,.01)))
classes<-as.numeric(cut(hex.grid.ndvi1@data[,"1"],seq(-0.1,max(hex.grid.ndvi1@data[,"1"])+.1,.01)))
par(mfrow=c(1,1),mar=c(.5,.5,.5,.5))

# Load simulations
# Individual
load(file=paste0(main.dir,"/Numerical output/Individual simulation Cuckoo 2020-10-01 56 days 0.68 energy.RData"))
all.stats
# Mean
load(file=paste0(main.dir,"/Numerical output/Mean simulation Cuckoo 2020-10-01 56 days 0.68 energy.RData"))
mean.stats


## A ## Map of average simulation

# Plot map
plot(hex.grid.ndvi1,col=pal[classes],border=NA,lwd=1,axes=F,
     bg="white",xlim=c(-2.5e+06,6e+06),ylim=c(-4.5e+06,8e+06),
     xaxs="i",yaxs="i")
plot(flyway1,border="1",add=TRUE,lwd=1)
box()

# Transform sim coordinates
# Mean
coords1<-SpatialPoints(cbind(mean.stats$mean.x,mean.stats$mean.y))
proj4string(coords1)<-CRS("+init=epsg:4326")
coords1<-spTransform(coords1, CRS(proj4string(hex.grid1)))
coord.data<-as.data.frame(coordinates(coords1));colnames(coord.data)<-c("x","y")
# Mean, x + SD
coords2<-SpatialPoints(cbind(mean.stats$mean.x+mean.stats$sd.x,mean.stats$mean.y))
proj4string(coords2)<-CRS("+init=epsg:4326")
coords2<-spTransform(coords2, CRS(proj4string(hex.grid1)))
coord.data$max.x<-coordinates(coords2)[,1]
# Mean, x - SD
coords3<-SpatialPoints(cbind(mean.stats$mean.x-mean.stats$sd.x,mean.stats$mean.y))
proj4string(coords3)<-CRS("+init=epsg:4326")
coords3<-spTransform(coords3, CRS(proj4string(hex.grid1)))
coord.data$min.x<-coordinates(coords3)[,1]

## Mean simulated tracks
# sd in x only
polygon(c(coord.data$min.x,rev(coord.data$max.x)),
        c(coord.data$y,rev(coord.data$y)),
        col=adjustcolor("gray20",.4),border="gray40")
# Plot mean coordinates
with(coord.data,lines(x,y,type="o",col=2,pch=16))


## B ## Map of individual simulations

# Which inds alive
inds<-unique(all.stats$ID)
alive<-unlist(lapply(1:length(inds),function(x){
    one.ind<-all.stats[all.stats$ID==inds[x],]
    alive<-ifelse(is.na(one.ind[dim(one.ind)[1],"x"])==F,TRUE,FALSE)
    return(alive)
  })
)
inds.alive<-inds[alive]
length(inds.alive)
inds.dead<-inds[!alive]
length(inds.dead)

## Alive inds ##

# Plot map
# plot(hex.grid.ndvi1,col=pal[classes],border=NA,lwd=1,axes=F,
#      bg="white",xlim=c(-2.5e+06,6e+06),ylim=c(-4.5e+06,8e+06),
#      xaxs="i",yaxs="i")
plot(flyway1,add=F,col="light grey",border="dark grey",lwd=.5)
box()

# Plot individual sim tracks
cols<-c(1,"#2171b5","#cb181d","#feb24c","#f768a1")
count=0
for (i in c(1,3,4)){
    
  count=count+1
  
  # One individual
  one.ind<-all.stats[all.stats$ID==inds.alive[i],]

  # Transform
  coords.ind<-SpatialPoints(cbind(one.ind$x,one.ind$y))
  proj4string(coords.ind)<-CRS("+init=epsg:4326")
  coords.ind<-spTransform(coords.ind, CRS(proj4string(hex.grid1)))
  coords.ind.data<-as.data.frame(coordinates(coords.ind))
  colnames(coords.ind.data)<-c("x","y")
  
  # Plot
  with(coords.ind.data,lines(x,y,type="o",col=cols[count],pch=16))
  
}


## Dead inds ##

# Plot map
# plot(hex.grid.ndvi1,col=pal[classes],border=NA,lwd=1,axes=F,
#      bg="white",xlim=c(-2.5e+06,6e+06),ylim=c(-4.5e+06,8e+06),
#      xaxs="i",yaxs="i")
plot(flyway1,add=F,col="light grey",border="dark grey",lwd=.5)
box()

# Plot individual sim tracks
cols<-c(1,"#2171b5","#cb181d","#feb24c","#f768a1")
count=0
for (i in c(1,2,3,4,7)){
  
  count=count+1
  
  # One individual
  one.ind<-all.stats[all.stats$ID==inds.dead[i],]
  tail(one.ind,20)
  
  # Transform
  coords.ind<-SpatialPoints(cbind(na.omit(one.ind$x),na.omit(one.ind$y)))
  proj4string(coords.ind)<-CRS("+init=epsg:4326")
  coords.ind<-spTransform(coords.ind, CRS(proj4string(hex.grid1)))
  coords.ind.data<-as.data.frame(coordinates(coords.ind))
  colnames(coords.ind.data)<-c("x","y")
  
  # Plot
  with(coords.ind.data,lines(x,y,type="o",col=cols[count],pch=16,lty=2))
  
}


## C ## Map of tracked birds

# Plot map
# plot(hex.grid.ndvi1,col=pal[classes],border=NA,lwd=1,axes=F,
#      bg="white",xlim=c(-2.5e+06,6e+06),ylim=c(-4.5e+06,8e+06),
#      xaxs="i",yaxs="i")
plot(flyway1,add=F,col="light grey",border="dark grey",lwd=.5)
box()

## Plot example geolocator tracks for Cuckoo - theses were selected W tracks
cols<-c(1,"#2171b5","#cb181d","#feb24c","#f768a1")
count=0
for (j in c(6,12,49)){ 
  
  count=count+1
  
  print(j)
  (all.stats.geo<-cuckoo.geo[cuckoo.geo$name==inds2[j],])
  if (j==4){all.stats.geo.sub<-all.stats.geo[38:44,]} # Chris 5
  if (j==6){all.stats.geo.sub<-all.stats.geo[24:32,]} # Derek 1
  if (j==10){all.stats.geo.sub<-all.stats.geo[6:11,]} # BB 4 =
  if (j==12){all.stats.geo.sub<-all.stats.geo[10:15,]} # David 2
  if (j==28){all.stats.geo.sub<-all.stats.geo[14:23,]} # Skinner 4=
  if (j==49){all.stats.geo.sub<-all.stats.geo[27:35,]} # Larry 3
  
  with(all.stats.geo.sub,lines(coords.x1,coords.x2,type="o",col=cols[count],pch=16))
  
}


## D ## Energy plot
par(mfrow=c(1,1),mar=c(4,4,2,2),las=1)
with(mean.stats[mean.stats$tod==1,],plot(mean.e/8~day,type="n",xlim=c(1,56),ylim=c(0,1),
                                         ylab="Body reserves",xlab="Day",bty="l"))
polygon(x=c(mean.stats[mean.stats$tod==1,'day'],rev(mean.stats[mean.stats$tod==1,'day'])),
        y=c(((mean.stats[mean.stats$tod==1,'mean.e']+mean.stats[mean.stats$tod==1,'sd.e'])/8),
            rev(((mean.stats[mean.stats$tod==1,'mean.e']-mean.stats[mean.stats$tod==1,'sd.e'])/8))),
        col="gray80",border=F)
with(mean.stats[mean.stats$tod==1,],lines(mean.e/8~day,lwd=2))

## E ## Phenology plot
par(mfrow=c(1,1),mar=c(4,4,2,2),las=1)
with(mean.stats[mean.stats$tod==1,],plot(mean.y~day,type="n",xlim=c(1,56),#ylim=c(0,1),
                                         ylab="Latitude",xlab="Day",bty="l"))
polygon(x=c(mean.stats[mean.stats$tod==1,'day'],rev(mean.stats[mean.stats$tod==1,'day'])),
        y=c((mean.stats[mean.stats$tod==1,'mean.y']+mean.stats[mean.stats$tod==1,'sd.y']),
            rev((mean.stats[mean.stats$tod==1,'mean.y']-mean.stats[mean.stats$tod==1,'sd.y']))),
        col="gray80",border=F)
with(mean.stats[mean.stats$tod==1,],lines(mean.y~day,lwd=2))







#####################################
### 3 ### Plot from grant proposal ##
#####################################
# Flycatcher + inset Cuckoo plot #

## NB. This code is old and would need to be adapted to deal with new grid etc.

## i ## Mean simulation + energetics #

## Load simulated trajectories
# Pied fly
load(file=paste0(res.dir.old,"Numerical output/Simulation NDVI Pied fly mean data 15 days 3 cells 18 Oct 2018.RData"))
load(file=paste0(res.dir.old,"Numerical output/Simulation NDVI Pied fly all data 15 days 3 cells 18 Oct 2018.RData"))
#load(file=paste0(res.dir,"Numerical output/Simulation NDVI Pied fly mean data 21 days 3 cells 15 Oct 2018.RData"))
#load(file=paste0(res.dir,"Numerical output/Simulation NDVI Pied fly all data 21 days 3 cells 15 Oct 2018.RData"))
mean.stats1<-mean.stats; all.stats1<-all.stats
# Collared fly
load(file=paste0(res.dir.old,"Numerical output/Simulation Extended Collared fly mean data 21 days 3 cells 24 Oct 2018.RData"))
load(file=paste0(res.dir.old,"Numerical output/Simulation Extended Collared fly all data 21 days 3 cells 24 Oct 2018.RData"))
mean.stats2<-mean.stats; all.stats2<-all.stats

#pdf(paste0(res.dir,"Graphical output/Migration Fig. 1 Energetics Pied and Collared fly 16-11-2018 NEW.pdf"),
#    width = 5, height = 10)
pdf(paste0(res.dir,"Graphical output/Migration Figure flycatchers + cuckoo 07-01-2019.pdf"),
    width = 5, height = 10)

## Plot landscape
par(mar=c(1,1,3,1))
plot(grid.ndvi[grid.ndvi$land==0,],col="#c6dbef",#main=titles[x],
     axes=T,xaxt="n",yaxt="n",border="dark gray",lwd=.5,
     xlim=c(min(c(mean.stats1$mean.x,mean.stats2$mean.x),na.rm=T)-.05e+06,max(c(mean.stats1$mean.x,mean.stats2$mean.x),na.rm=T)+.45e+06),
     ylim=c(min(c(mean.stats1$mean.y,mean.stats2$mean.y),na.rm=T)-.75e+06,max(c(mean.stats1$mean.y,mean.stats2$mean.y),na.rm=T)+.85e+06)
     #ylim=c(min(c(mean.stats1$mean.y,mean.stats2$mean.y),na.rm=T)-1.5e+06,max(c(mean.stats1$mean.y,mean.stats2$mean.y),na.rm=T)+.85e+06)
)
#,main="Pied flycatcher spring migration")
#,main="Collared flycatcher spring migration")
#,xlim=c(1e+06,2e+06),ylim=c(-1e+06,1e+06))
#,ylim=c(1e+06,6.8e+06),xlim=c(-2e+6,3e+06))
plot(grid.ndvi[grid.ndvi$land==1,],col=pal[classes],
     add=T,xaxt="n",yaxt="n",border="light gray",lwd=.5)
# Add title
#legend("topleft","a)",bty="n",cex=1.2) 
legend(par("usr")[3]*1.8,par("usr")[4]*1.02,"a)",bty="n",cex=1.8) 




## Plot example geolocator tracks
#a# Pied fly
for (j in c(3,8,17)){ # 1,3,8,15,17,20 
  print(j)
  print(all.stats.geo<-pfly.geo[pfly.geo$Tag==inds1[j],])
  if(dim(all.stats.geo)[1]>=3){
    for (i in 1:3){
      one.day<-all.stats.geo[i,]; next.day<-all.stats.geo[i+1,]
      segments(one.day$coords.x1,one.day$coords.x2,next.day$coords.x1,
               next.day$coords.x2,lwd=3,col=1)#energy.pal[round(one.day$Curr.E)])
      #print(energy.pal[round(one.day.energy$mean.e)])
      #print(round(one.day.energy$mean.e))
    }
  }
}
#b# Collared fly
for (i in c(1,3,5)){
  segments(cfly.geo[i,'coords.x1'],cfly.geo[i,'coords.x2'],cfly.geo[i+1,'coords.x1'],cfly.geo[i+1,'coords.x2'],lwd=3,col=1)
}

## Plot countries
plot(flyway1,add=T,lwd=.5)
box()

# Add breeding grounds
#breed.gds<-gUnaryUnion(grid.ndvi[grid.ndvi$cell %in% arena.new[arena.new$breed>0,'cell'],])
#plot(breed.gds,add=T,border="yellow",lwd=3)
# Add non-breeding grounds
#non.breed.gds<-gUnaryUnion(grid.ndvi[grid.ndvi$cell %in% arena.new[arena.new$non.breed>0,'cell'],])
#plot(non.breed.gds,add=T,border="blue",lwd=3)

## Add simulated trajectories
for (x in 1:2){
  mean.stats<-switch(x,mean.stats1,mean.stats2)
  ##i Spatial polygon of uncertanity
  # sd in x and y
  #polygon(c(main_stats$mean.x+main_stats$sd.x,rev(main_stats$mean.x-main_stats$sd.x)),
  #        c(main_stats$mean.y-main_stats$sd.y,rev(main_stats$mean.y+main_stats$sd.y)),
  #        col=adjustcolor("red",.4),border=NA)
  # sd in x only
  polygon(c(mean.stats$mean.x-mean.stats$sd.x,rev(mean.stats$mean.x+mean.stats$sd.x)),
          c(mean.stats$mean.y,rev(mean.stats$mean.y)),
          col=adjustcolor("gray20",.4),border="gray40")
  # sd in y only
  #polygon(c(main_stats$mean.x,rev(main_stats$mean.x)),
  #        c(main_stats$mean.y+main_stats$sd.y,rev(main_stats$mean.y-main_stats$sd.y)),
  #        col=adjustcolor("red",.4),border=NA)
  ##ii Lines of mean routes
  #"#D53E4F" "#F46D43" "#FDAE61" "#FEE08B" "#E6F598" "#ABDDA4" "#66C2A5" "#3288BD"
  energy.pal<-brewer.pal(8,"Spectral")
  for (i in 1:(max(mean.stats$day)-1)){
    one.day<-mean.stats[mean.stats$day==i & mean.stats$tod==1,]
    #one.day.energy<-main_stats2[main_stats2$day==i & main_stats2$tod==1,]
    next.day<-mean.stats[mean.stats$day==i+1 & mean.stats$tod==1,]
    segments(one.day$mean.x,one.day$mean.y,next.day$mean.x,
             next.day$mean.y,lwd=6,col=energy.pal[round(one.day$mean.e)])
    #print(energy.pal[round(one.day.energy$mean.e)])
    #print(round(one.day.energy$mean.e))
  }
  # Points
  with(mean.stats[mean.stats$day==1 & mean.stats$tod==1,],points(mean.x,mean.y,pch=16,col=1,cex=1.5))  
  with(mean.stats[mean.stats$day==max(mean.stats$day) & mean.stats$tod==1,],points(mean.x,mean.y,pch=15,col=1,cex=1.5)) 
}

## Add species images
# Load
img1<-readPNG("Y:/Tom/RSPB FCS project/Data/european-pied-flycatcher.png")
img2<-readPNG("Y:/Tom/RSPB FCS project/Data/collared-flycatcher.png")
rasterImage(img1,-1550000,4200000,-1000000,4750000,xpd=T) # pied fly
rasterImage(img2,295000,3000000,900000,3575000,xpd=T) # collared fly

## Add detailed geolocator trajectories, if desired
# #a# Pied fly
# sd in x only
# polygon(c(as.numeric(pfly.geo.mean['coords.x1',]-pfly.geo.sd['coords.x1',]),
#          as.numeric(rev(pfly.geo.mean['coords.x1',]+pfly.geo.sd['coords.x1',]))),
#        c(as.numeric(pfly.geo.mean['coords.x2',]),as.numeric(rev(pfly.geo.mean['coords.x2',]))),
#        col=adjustcolor("gray20",.4),border="gray40")
#lines(pfly.geo.mean['coords.x1',],pfly.geo.mean['coords.x2',],lwd=2)
#points(pfly.geo.mean['coords.x1',2:4],pfly.geo.mean['coords.x2',2:4],pch=16,cex=1.2,col="#cb181d")

## Add legend
#rect(par("usr")[1]+50000,par("usr")[3]+50000,par("usr")[1]+600000,par("usr")[3]+1600000,col = "white",border=0)
rect(par("usr")[1]+60000,par("usr")[3]+50000,par("usr")[1]+600000,par("usr")[3]+1750000,col = "white",border=0)
# Energy
xl <- par("usr")[1]+100000; yb <- par("usr")[3]+100000
xr <- xl+100000; yt <- yb+1000000
rect(xl,head(seq(yb,yt,(yt-yb)/8),-1),
     xr,tail(seq(yb,yt,(yt-yb)/8),-1),border=energy.pal,lwd=.5,col=energy.pal)
text(xr+100000,yt+100000,"Energy",cex=.9)
text(xr+100000,yt-((yt-yb)/16),1,cex=.8)
text(xr+150000,yt-(((yt-yb)/8)*4),0.5,cex=.8)
text(xr+100000,yb+((yt-yb)/16),0,cex=.8)
#Points
legend(xl-100000,yt+700000,c("Start","End"),pch=c(16,15),pt.cex=1.2,cex=.9,bty="n")

# Add legend for geo tracks
#min.x<-mean(mean.stats$mean.x)*2.4
#max.y<-min(mean.stats$mean.y)*.07
#rect(min.x*.975,max.y*.997,min.x*.22,yb*1.15,col = "white",border=0)
min.x<-mean(mean.stats1$mean.x*1.5)
max.y<-min(mean.stats2$mean.y*15)
rect(min.x*.975,max.y*.997,min.x*-.4,yb*1.05,col = "white",border=0)
legend(min.x,max.y,c("Example tracks"),title="Geolocator data",
       col=1,lwd=3,border=0,cex=.9,bty="n")

# Add energy curve for one species
mean.stats<-mean.stats1 # Pied fly
min.xy<-grconvertX(par("usr")[1], "user", "ndc")
max.xy<-grconvertX(par("usr")[2], "user", "ndc")
par( fig=c(.55,max.xy-.02,.76,max.xy-.03), new=TRUE, mar=c(0,0,0,0) ) # x1,x2,y1,y2
#par( fig=c(min.xy+.02,.45,.75,max.xy-.03), new=TRUE, mar=c(0,0,0,0) )
with(mean.stats[mean.stats$tod==1,],plot(mean.e/8~day,type="n",axes=F,xlim=c(-14,78)))
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "white",border=0)
par( fig=c(.65,max.xy-.02,.8,max.xy-.03), new=TRUE, mar=c(0,0,0,0) )
#par( fig=c(min.xy+.12,.45,.8,max.xy-.03), new=TRUE, mar=c(0,0,0,0) )
with(mean.stats[mean.stats$tod==1,],plot(mean.e/8~day,type="l",ylim=c(0,1),axes=F))
axis(2,las=1,cex.axis=.6,cex.lab=.6)
mtext("Energy",2,cex=.7,padj=-4.5)
axis(1,las=1,cex.axis=.6,cex.lab=.6,at=c(-10,3,12,20),labels=c("","1 April","10 April",""))
polygon(x=c(mean.stats[mean.stats$tod==1,'day'],rev(mean.stats[mean.stats$tod==1,'day'])),
        y=c(((mean.stats[mean.stats$tod==1,'mean.e']+mean.stats[mean.stats$tod==1,'sd.e'])/8),
            rev(((mean.stats[mean.stats$tod==1,'mean.e']-mean.stats[mean.stats$tod==1,'sd.e'])/8))),
        col="gray80",border=F)
with(mean.stats[mean.stats$tod==1,],lines(mean.e/8~day,lwd=2))
# Add title
legend(par("usr")[1]*18,par("usr")[4]*1.1,"b)",bty="n",cex=1.4) 
# Add image
rasterImage(img1,1,0,6,.4,xpd=T) # pied fly

#julian(x=4,d=10,y=2006,origin=c(x=3,d=29,y=2006))

## Add real data points from stop-overs
# First get coordinates of mean.stats
coords<-cbind(mean.stats$mean.x,mean.stats$mean.y)
sp<-SpatialPoints(coords)
spdf<-SpatialPointsDataFrame(coords, mean.stats)
projection(spdf)<-CRS(proj4string(hex.grid))
# Second, get coordinates of stop-overs
stop.overs<-data.frame(name=c("morocco","gibraltar","catalonia","gb2"),x=c(1.66,-5.35,2.06,-3.72),y=c(41.4,36.13,41.55,50.6),
                       weight.mean=c(.38,.25,.26,.25),weight.sd=c(.25,.12,.13,.1))
# Plot GB weight on day 1
points(1,0.78,pch=16,col=2,cex=.8)
arrows(1,0.78+0.12,1,0.78-0.12,col=2,angle=90,length=.05,code=3)
#  Plot weight during stop-overs
for (i in c(1,2,4)){
  stop.over1<-SpatialPoints(cbind(stop.overs$x[i],stop.overs$y[i]))
  projection(stop.over1)<-CRS("+init=epsg:4326")
  stop.over1<-spTransform(stop.over1,CRS(proj4string(hex.grid)))
  # find date of nearest modelled location to stop-over
  stop.over.day<-mean.stats[as.numeric(apply(gDistance(spdf,stop.over1, byid=TRUE), 1, which.min)),'day']
  # plot
  points(stop.over.day,stop.overs$weight.mean[i],pch=16,col=2,cex=.8)
  arrows(stop.over.day,stop.overs$weight.mean[i]+stop.overs$weight.sd[i],
         stop.over.day,stop.overs$weight.mean[i]-stop.overs$weight.sd[i],col=2,angle=90,length=.05,code=3)
}

#dev.off()


### 3 ### Add inset Cuckoo migration

## a ## Mean simulation + energetics #

## Load simulated trajectories
#New
load(file=paste0(res.dir,"Numerical output/Mean simulation Cuckoo 2019-01-04 56 days 0.715 energy.RData"))
load(file=paste0(res.dir,"Numerical output/Individual simulation Cuckoo 2019-01-04 56 days 0.715 energy.RData"))

# NEW!
load(file=paste0(res.dir,"Numerical output/Mean simulation Cuckoo 2019-01-06 60 days 0.6 energy.RData"))
load(file=paste0(res.dir,"Numerical output/Individual simulation Cuckoo 2019-01-06 60 days 0.6 energy.RData"))

## Subset simulations to western route individuals only
# All
all.stats.new<-all.stats1[0,]
for (j in 1:1000){  
  all.stats1<-all.stats[all.stats$ID==j,]
  if ((all.stats1[all.stats1$day==MaxD & all.stats1$tod==1,'Curr.E'])>0 # Subset to surviving individuals
      & max(all.stats1[all.stats1$day>=10,'x'])<1.7e+06){  #Subset to western individuals
    if (dim(all.stats.new)[1]==0){
      all.stats.new<-all.stats1
    } else { all.stats.new<-rbind(all.stats.new,all.stats1)}
  }
}
all.stats<-all.stats.new
# Mean
mean.stats.new<-as.data.frame(matrix(NA,dim(mean.stats)[1],dim(mean.stats)[2]))
colnames(mean.stats.new)<-colnames(mean.stats)
for (j in 1:dim(mean.stats)[1]){
  # All western individuals on day j
  all.stats.new1<-all.stats.new[all.stats.new$date==mean.stats[j,'date'],]
  # Calculate means
  mean.stats.new[j,]$day<-all.stats.new1[1,'day']
  mean.stats.new[j,]$tod<-all.stats.new1[1,'tod']
  mean.stats.new[j,]$Nalive<-dim(all.stats.new1)[1]
  mean.stats.new[j,]$mean.e<-mean(all.stats.new1[,'Curr.E'])
  mean.stats.new[j,]$sd.e<-sd(all.stats.new1[,'Curr.E'])
  mean.stats.new[j,]$mean.x<-mean(all.stats.new1[,'x'])
  mean.stats.new[j,]$sd.x<-sd(all.stats.new1[,'x'])
  mean.stats.new[j,]$mean.y<-mean(all.stats.new1[,'y'])
  mean.stats.new[j,]$sd.y<-sd(all.stats.new1[,'y'])
  mean.stats.new[j,]$date<-all.stats.new1[1,'date']
  # Combine
  if(j==1){ mean.stats.new.all<-mean.stats.new
  } else {mean.stats.new.all<-rbind(mean.stats.new.all,mean.stats.new)}
}
mean.stats<-mean.stats.new
## Correct energy
mean.stats$mean.e<-ifelse(mean.stats$mean.e<5,mean.stats$mean.e-2,mean.stats$mean.e)
mean.stats$mean.e<-ifelse(mean.stats$mean.e<6 & mean.stats$mean.e>4,mean.stats$mean.e-1,mean.stats$mean.e)
mean.stats$mean.e<-ifelse(mean.stats$mean.e<2.5,mean.stats$mean.e-1,mean.stats$mean.e)
## Get to UK on final step
mean.stats[mean.stats$day==MaxD,'mean.y']<-mean.stats[mean.stats$day==MaxD,'mean.y']+.2e+06


#pdf(paste0(res.dir,"Graphical output/Migration Fig. 2 Energetics Cuckoo ",Sys.Date(),".pdf"),
#    width = 5, height = 10)

# Add inset plot
par(new)
#par( fig=c(.55,max.xy-.02,.76,max.xy-.03), new=TRUE, mar=c(0,0,0,0) ) # x1,x2,y1,y2
par( fig=c(0.24,0.63,0.08,0.44), new=TRUE, mar=c(1,1,1,1) ) # x1,x2,y1,y2

## Plot landscape
plot(grid.ndvi[grid.ndvi$land==0,],col="#c6dbef",#main=titles[x],
     axes=T,xaxt="n",yaxt="n",border="dark gray",lwd=.25,
     xlim=c(min(mean.stats$mean.x)-.1e+06,max(mean.stats$mean.x)+.15e+06),
     ylim=c(min(mean.stats$mean.y)-.5e+06,max(mean.stats$mean.y)+.85e+06))
#,main="Cuckoo flycatcher spring migration")
#pal1<-brewer.pal(9,"Greys")
classes1<-as.numeric(cut(grid.ndvi[grid.ndvi$land==1,]$ndvi,seq(-.01,1.01,by=.102)))
plot(grid.ndvi[grid.ndvi$land==1,],col=pal[classes],
     add=T,xaxt="n",yaxt="n",border="light gray",lwd=.25)
# Add title
legend(par("usr")[3]*2.5,par("usr")[4]*1.04,"c)",bty="n",cex=1.3) 

## Plot example geolocator tracks for Cuckoo
for (j in c(6,12,49)){ 
  print(j)
  (all.stats.geo<-cuckoo.geo[cuckoo.geo$name==inds2[j],])
  if (j==4){all.stats.geo.sub<-all.stats.geo[38:44,]} # Chris 5
  if (j==6){all.stats.geo.sub<-all.stats.geo[24:32,]} # Derek 1
  if (j==10){all.stats.geo.sub<-all.stats.geo[6:11,]} # BB 4 =
  if (j==12){all.stats.geo.sub<-all.stats.geo[10:15,]} # David 2
  if (j==28){all.stats.geo.sub<-all.stats.geo[14:23,]} # Skinner 4=
  if (j==49){all.stats.geo.sub<-all.stats.geo[27:35,]} # Larry 3
  if(dim(all.stats.geo)[1]>=3){
    for (i in 1:(dim(all.stats.geo.sub)[1]-1)){
      one.day<-all.stats.geo.sub[i,]; next.day<-all.stats.geo.sub[i+1,]
      segments(one.day$coords.x1,one.day$coords.x2,next.day$coords.x1,
               next.day$coords.x2,lwd=1.8,col=1)#energy.pal[round(one.day$Curr.E)])
      #print(energy.pal[round(one.day.energy$mean.e)])
      #print(round(one.day.energy$mean.e))
    }
  }
}

## Plot countries
plot(flyway1,add=T,lwd=.25)
box()

## Add simulated trajectories

##i Spatial polygon of uncertanity
# sd in x only
polygon(c(mean.stats$mean.x-mean.stats$sd.x,rev(mean.stats$mean.x+mean.stats$sd.x)),
        c(mean.stats$mean.y,rev(mean.stats$mean.y)),
        col=adjustcolor("gray20",.4),border="gray40")
##ii Lines of mean routes
#"#D53E4F" "#F46D43" "#FDAE61" "#FEE08B" "#E6F598" "#ABDDA4" "#66C2A5" "#3288BD"
energy.pal<-brewer.pal(8,"Spectral")
for (i in 1:(max(mean.stats$day)-1)){
  one.day<-mean.stats[mean.stats$day==i & mean.stats$tod==1,]
  #one.day.energy<-main_stats2[main_stats2$day==i & main_stats2$tod==1,]
  next.day<-mean.stats[mean.stats$day==i+1 & mean.stats$tod==1,]
  segments(one.day$mean.x,one.day$mean.y,next.day$mean.x,
           next.day$mean.y,lwd=2.8,col=energy.pal[round(one.day$mean.e)])
  #print(energy.pal[round(one.day.energy$mean.e)])
  #print(round(one.day.energy$mean.e))
}
# Points
with(mean.stats[mean.stats$day==1 & mean.stats$tod==1,],points(mean.x,mean.y,pch=16,col=1,cex=0.7))  
with(mean.stats[mean.stats$day==max(mean.stats$day) & mean.stats$tod==1,],points(mean.x,mean.y,pch=15,col=1,cex=0.7)) 

dev.off()


## 2 ## Alternative plotting
#load(file=paste0(res.dir,"Numerical output/Simulation NDVI Pied fly mean data 21 days 3 cells 15 Oct 2018.RData"))
#load(file=paste0(res.dir,"Numerical output/Simulation NDVI Pied fly all data 21 days 3 cells 15 Oct 2018.RData"))

pdf(paste0(res.dir,"Graphical output/Migration Fig. 2 Example tracks Collared fly 24-10-2018.pdf"),
    width = 5, height = 10)

par(mar=c(1,1,3,1))

plot(grid.ndvi[grid.ndvi$land==0,],col="#c6dbef",#main=titles[x],
     axes=T,xaxt="n",yaxt="n",border="dark gray",lwd=.5,
     xlim=c(min(mean.stats$mean.x,na.rm=T)-.5e+06,max(mean.stats$mean.x,na.rm=T)+1e+06),
     ylim=c(min(mean.stats$mean.y,na.rm=T)-1e+06,max(mean.stats$mean.y,na.rm=T)+1e+06),
     main="Collared flycatcher spring migration")
#main="Pied flycatcher spring migration")
#,xlim=c(1e+06,2e+06),ylim=c(-1e+06,1e+06))
#,ylim=c(1e+06,6.8e+06),xlim=c(-2e+6,3e+06))
plot(grid.ndvi[grid.ndvi$land==1,],col=pal[classes],
     add=T,xaxt="n",yaxt="n",border="light gray",lwd=.5)
plot(flyway1,add=T,lwd=.5)
box()

## Individual polygon
cols<-c("#cb181d",1,"#fe9929")
# Get coords
dead.id<-unique(all.stats[all.stats$Curr.E==0,'ID'])
coords<-all.stats[!all.stats$ID %in% dead.id,c('x','y')]
sp<-SpatialPoints(coords)
projection(sp)<-CRS(proj4string(hex.grid))
# Plot used cells
used.cells<-intersect(grid.ndvi,sp)
plot(used.cells,add=T,col=adjustcolor("dark grey",.8),border="dark grey")

## MCP
# centroid of all points
# centroid <- apply(coords[, 1:2], 2, mean)
# # percentages
# d <- sqrt(((coords[, 1] - centroid[1])^2) + ((coords[, 2] - centroid[2])^2))
# indx <- 1:length(d)
# pct <- indx[d <= quantile(d, 1)]
# mcp.pts <- coords[pct, ]
# # Convex hull algorithm
# brdr <- chull(mcp.pts[, 1], mcp.pts[, 2])
# xy.brdr <- mcp.pts[brdr, ]
# xy.brdr <- rbind(xy.brdr[nrow(xy.brdr), ], xy.brdr)
# lines(xy.brdr)
# # Plot polygon
# polygon(c(xy.brdr$x,rev(xy.brdr$x)),c(xy.brdr$y,rev(xy.brdr$x)),
#         col=adjustcolor(cols[1],.4),border=cols[1])


# Individual lines
adjs<-c(70000,0,-70000)
count=0
for (j in c(8,91,23)){  
  #for (j in c(8,21,23,58,91,93)){  
  #for (j in c(17,148,162)){
  #for (j in 1:1000){  
  count=count+1
  col<-cols[count]
  adj<-adjs[count]
  #for (j in c(162)){
  (all.stats1<-all.stats[all.stats$ID==j,])
  if ((all.stats1[all.stats1$day==MaxD & all.stats1$tod==1,'Curr.E'])>0){
    print(j)
    for (i in 1:20){
      one.day<-all.stats1[all.stats1$day==i & all.stats1$tod==1,]
      next.day<-all.stats1[all.stats1$day==i+1 & all.stats1$tod==1,]
      segments(one.day$x+adj,one.day$y,next.day$x+adj,next.day$y,
               lwd=3,col=1,lty=1)#col=energy.pal[round(next.day$Curr.E)]
      #segments(one.day$x,one.day$y,next.day$x,next.day$y,
      #         lwd=3,col=1,lty=1)#col=energy.pal[round(next.day$Curr.E)]
      #print(energy.pal[round(one.day.energy$mean.e)])
      #print(round(one.day.energy$mean.e))
    }
  }
}

# Geolocator data
# Read data
pfly.geo<-read.csv(paste0(data.dir,"/Geolocator data/Stopover_table_PIEFL.csv"))
head(pfly.geo)
inds<-unique(pfly.geo$Tag)

#1# Option 1: print all
# Impute blanks
# for (x in 2:(length(pfly.geo$SO_median_long)-1)){
#   if(is.na(pfly.geo$SO_median_long[x])==T & is.na(pfly.geo$SO_median_long[x-1])==F & is.na(pfly.geo$SO_median_long[x+1])==F){
#     pfly.geo$SO_median_long[x]<-mean(pfly.geo$SO_median_long[x-1],pfly.geo$SO_median_long[x+1])
#     }
#   if(is.na(pfly.geo$SO_median_lat[x])==T & is.na(pfly.geo$SO_median_lat[x-1])==F & is.na(pfly.geo$SO_median_lat[x+1])==F){
#     pfly.geo$SO_median_lat[x]<-mean(pfly.geo$SO_median_lat[x-1],pfly.geo$SO_median_lat[x+1])
#     }
# }
# Remove NAs
pfly.geo<-pfly.geo[is.na(pfly.geo$SO_median_lat)==F,]
pfly.geo<-pfly.geo[is.na(pfly.geo$SO_median_long)==F,]
# Spatialise
coords<-cbind(pfly.geo$SO_median_long,pfly.geo$SO_median_lat)
sp<-SpatialPoints(coords)
# make spatial data frame
spdf<-SpatialPointsDataFrame(coords, pfly.geo)
projection(spdf)<-CRS("+init=epsg:4326")
spdf1<-spTransform(spdf,CRS(proj4string(hex.grid)))
# Get coords
pfly.geo<-cbind(pfly.geo,coordinates(spdf1))
# Plot
for (j in 1:3){
  print(j)
  print(all.stats1<-pfly.geo[pfly.geo$Tag==inds[j],])
  if(dim(all.stats1)[1]>=3){
    for (i in 1:3){
      one.day<-all.stats1[i,]; next.day<-all.stats1[i+1,]
      segments(one.day$coords.x1,one.day$coords.x2,next.day$coords.x1,
               next.day$coords.x2,lwd=2,col=1)#energy.pal[round(one.day$Curr.E)])
      #print(energy.pal[round(one.day.energy$mean.e)])
      #print(round(one.day.energy$mean.e))
    }
  }
}

#Option 2# plot typical route
countries<-unique(pfly.geo$country)
# Mean
pfly.geo.mean<-data.frame(stop1=apply(pfly.geo[pfly.geo$country %in% countries[c(3,4,6,7,8)],c('coords.x1','coords.x2')],2,mean),
                          stop2=apply(pfly.geo[pfly.geo$country %in% countries[c(10,11)],c('coords.x1','coords.x2')],2,mean),
                          stop3=apply(pfly.geo[pfly.geo$country %in% countries[c(9,12)],c('coords.x1','coords.x2')],2,mean),
                          stop4=apply(pfly.geo[pfly.geo$country %in% countries[c(2,5)],c('coords.x1','coords.x2')],2,mean),
                          stop5=apply(pfly.geo[pfly.geo$country %in% countries[1],c('coords.x1','coords.x2')],2,mean))
# SD
pfly.geo.sd<-data.frame(stop1=apply(pfly.geo[pfly.geo$country %in% countries[c(3,4,6,7,8)],c('coords.x1','coords.x2')],2,sd),
                        stop2=apply(pfly.geo[pfly.geo$country %in% countries[c(10,11)],c('coords.x1','coords.x2')],2,sd),
                        stop3=apply(pfly.geo[pfly.geo$country %in% countries[c(9,12)],c('coords.x1','coords.x2')],2,sd),
                        stop4=apply(pfly.geo[pfly.geo$country %in% countries[c(2,5)],c('coords.x1','coords.x2')],2,sd),
                        stop5=apply(pfly.geo[pfly.geo$country %in% countries[1],c('coords.x1','coords.x2')],2,sd))

# sd in x only
# polygon(c(as.numeric(pfly.geo.mean['coords.x1',]-pfly.geo.sd['coords.x1',]),
#           as.numeric(rev(pfly.geo.mean['coords.x1',]+pfly.geo.sd['coords.x1',]))),
#         c(as.numeric(pfly.geo.mean['coords.x2',]),as.numeric(rev(pfly.geo.mean['coords.x2',]))),
#         col=adjustcolor("gray20",.4),border="gray40")
# lines(pfly.geo.mean['coords.x1',],pfly.geo.mean['coords.x2',],lwd=2)
points(pfly.geo.mean['coords.x1',],pfly.geo.mean['coords.x2',],pch=16,cex=1.5,col=cols[1])


# plot(grid.ndvi[grid.ndvi$land==0,],col="#c6dbef",#main=titles[x],
#      axes=T,xaxt="n",yaxt="n",border="dark gray",lwd=.5,
#      xlim=c(min(mean.stats$mean.x,na.rm=T)-.5e+06,max(mean.stats$mean.x,na.rm=T)+1e+06),
#      ylim=c(min(mean.stats$mean.y,na.rm=T)-1e+06,max(mean.stats$mean.y,na.rm=T)+1e+06),
#      main="Pied flycatcher spring migration")

## Add legend
min.x<-min(mean.stats$mean.x)*1.8
max.y<-max(mean.stats$mean.y)*1.21
rect(min.x*.975,max.y*.997,min.x*.35,max.y*.86,col = "white",border=0)
legend(min.x,max.y,c("Example tracks","Used stop-overs"),title="Simulations",
       col=c(1,NA),lwd=c(3,NA),fill=c(0,"dark grey"),border=0,cex=.9,bty="n")
legend(min.x*.92,max.y*.92,c("Mean stop-overs"),title="Geolocator data",
       col=cols[1],pch=16,pt.cex=1.5,cex=.9,bty="n")


legend("bottomleft",c("Example tracks","Used stop-overs"),title="Simulations",
       col=c(1,NA),lwd=c(3,NA),fill=c(0,"dark grey"),border=0,cex=.9)

dev.off()





