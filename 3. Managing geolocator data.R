
# Load packages
# Which packages
list.packages <- c("raster","rgdal","maptools","sp","rgeos","spatialEco","ks","RColorBrewer")#"deldir","adehabitatMA","adehabitatLT","CircStats","ade4","adehabitatHR")
# Function to install packages
check.install.packages <- function(list.packages){
  for(req.lib in list.packages){
    is.installed <- is.element(req.lib, installed.packages()[,1])
    if(is.installed == FALSE){install.packages(req.lib)}
    require(req.lib,character.only=TRUE)
  }
}
#req.lib<-"adehabitatHR"
check.install.packages(list.packages)

# Set dir
which.pc<-1 # 1 is Swiss, 2 is laptop
folder<-switch(which.pc,"C://Users/TM","C://Users/Tom")
data.dir<-paste0(folder,"/Dropbox/Work/Postdocs/Durham/Migration project/Data")


#1# Cuckoo

# Read data
cuckoo.geo<-read.csv(paste0(data.dir,"/Geolocator data/stopover_table_bestofday_2018_1daymin_recalc_biomes_EXPORT_CHRIS.csv"))
cuckoo.geo$year.start<-substr(cuckoo.geo$SO_start,7,10)
cuckoo.geo$year.end<-substr(cuckoo.geo$SO_end,7,10)
cuckoo.geo$month.start<-substr(cuckoo.geo$SO_start,4,5)
cuckoo.geo$month.end<-substr(cuckoo.geo$SO_end,4,5)
cuckoo.geo<-cuckoo.geo[cuckoo.geo$SO_length_discrete_day>=30|cuckoo.geo$country=="United Kingdom",] # Only long stops, or UK!
cuckoo.geo<-cuckoo.geo[cuckoo.geo$dead=="N",]
head(cuckoo.geo)

## Load mapping data
# Merge data with shapefile
hex.grid1<-readOGR(dsn=data.dir,layer="Hex grid 500km")
flyway1<-readOGR(dsn=data.dir,layer="Flyway polygon 2020")
head(hex.grid1)

# Create spatial polygons from cuckoo locations
coords<-cbind(cuckoo.geo$SO_median_long,cuckoo.geo$SO_median_lat)
sp<-SpatialPoints(coords)
# make spatial data frame
spdf<-SpatialPointsDataFrame(coords, cuckoo.geo[,c(2,5:16)])
proj4string(spdf)<-CRS("+proj=longlat +datum=WGS84")
spdf1<-spTransform(spdf,CRS(proj4string(hex.grid1)))


# Plot locations
head(spdf1)
(names<-unique(spdf1$name))
plot(flyway1,col="light gray",axes=T)
#for (i in 1:length(names)){
all.bird.dat<-lapply(1:length(names), function(i){
  one.bird<-spdf1[spdf1$name==names[i],]
  one.bird@data
  years<-unique(one.bird$year.start)
  #for (j in years){
  one.bird.dat<-lapply(years, function(j){
    one.year<-one.bird[one.bird$year.start==j,]
    breed.loc<-one.year[one.year$SO_median_lat==max(one.year$SO_median_lat) & one.year$SO_median_lat>33,]
    plot(breed.loc,col=2,pch=16,add=T) 
    non.breed.loc<-one.year[one.year$SO_median_lat==min(one.year$SO_median_lat) & one.year$SO_median_lat<33,]
    plot(non.breed.loc,col=4,pch=16,add=T) 
    locs<-rbind(breed.loc,non.breed.loc)
    return(locs)
  })
  one.bird.locs<-do.call(rbind,one.bird.dat)
  one.bird.locs@data
  return(one.bird.locs)
})
all.bird.locs<-do.call(rbind,all.bird.dat)
#write.csv(all.bird.locs@data,paste0(data.dir,"/test cuckoo data.csv"))

# Separate into breeding and non-breeding grounds
breed.locs<-all.bird.locs[all.bird.locs$SO_median_lat>50,]
non.breed.locs<-all.bird.locs[all.bird.locs$SO_median_lat<4,]

##1 Extract breeding ground probabilities
# Estimate kernel density
df<-data.frame(coordinates(breed.locs))
coords.kde<-kde(df) 
coords.raster<-raster(coords.kde,crs=CRS(proj4string(breed.locs)))
# Extract used grid cells from kde
hex.grid.cr<-crop(hex.grid1,extent(coords.raster))
int<-extract(coords.raster,hex.grid.cr,df=T,fun=mean)
hex.grid.cr$breed.prob<-int$layer
hex.grid.cr[is.na(hex.grid.cr$breed.prob)==T,'breed.prob']<-0 # convert NAs to zero
hex.grid.cr[hex.grid.cr$land==0,'breed.prob']<-0 # convert sea cells to zero
hex.grid.cr$breed.prob<-hex.grid.cr$breed.prob/sum(hex.grid.cr$breed.prob) # convert to a proportion
# Add to hex grid
hex.grid1<-merge(hex.grid1,hex.grid.cr)
hex.grid1[is.na(hex.grid1$breed.prob)==T,'breed.prob']<-0
summary(hex.grid1)

##2 Extract non-breeding ground probabilities
# Estimate kernel density
df<-data.frame(coordinates(non.breed.locs))
coords.kde<-kde(df) 
coords.raster<-raster(coords.kde,crs=CRS(proj4string(non.breed.locs)))
# Extract used grid cells from kde
hex.grid.cr<-crop(hex.grid1,extent(coords.raster))
int<-extract(coords.raster,hex.grid.cr,df=T,fun=mean)
hex.grid.cr$non.breed.prob<-int$layer
hex.grid.cr[is.na(hex.grid.cr$non.breed.prob)==T,'non.breed.prob']<-0 # convert NAs to zero
hex.grid.cr[hex.grid.cr$land==0,'non.breed.prob']<-0 # convert sea cells to zero
hex.grid.cr$non.breed.prob<-hex.grid.cr$non.breed.prob/sum(hex.grid.cr$non.breed.prob) # convert to a proportion
# Add to hex grid
hex.grid1<-merge(hex.grid1,hex.grid.cr)
hex.grid1[is.na(hex.grid1$non.breed.prob)==T,'non.breed.prob']<-0

## Plot!

#jpeg(paste0(res.dir,"Graphical output/Presentation plot breeding prob.jpg"),
#     width=10000,height=15000,quality=100,res=3000)
pdf(paste0(res.dir,"Graphical output/Presentation plot breeding prob.pdf"),
    width = 5, height = 10)

par(mar=c(1,1,1,1))
plot(hex.grid1,col=c("light blue","dark grey")[hex.grid1$land+1],lwd=.5)
pal<-c("dark grey",brewer.pal(8,"YlOrRd"))
classes<-as.numeric(cut(hex.grid1[hex.grid1$breed.prob>0,]$breed.prob,seq(-.01,0.22,by=.028)))
plot(hex.grid1[hex.grid1$breed.prob>0,],col=pal[classes],add=T,lwd=.5)
classes<-as.numeric(cut(hex.grid1[hex.grid1$non.breed.prob>0,]$non.breed.prob,seq(-.01,0.09,by=.012)))
plot(hex.grid1[hex.grid1$non.breed.prob>0,],col=pal[classes],add=T,lwd=.5)
plot(flyway1,add=T,lwd=.5)

dev.off()

## Add to arena.new and save
hex.grid$cell<-as.numeric(substring(hex.grid$cell,3))
arena.new<-merge(arena.new[,1:4],hex.grid[,c(1,5,6)],by="cell")
names(arena.new)[5:6]<-c("cuckoo.breed","cuckoo.non.breed")
arena.new$cuckoo.breed<-ifelse(arena.new$cuckoo.breed>.02,arena.new$cuckoo.breed,0) # Remove french cells
#save(arena.new,file=paste0(data.dir,"/Modelling arena extended.RData"))


#2# Pied fly

# Read data
pfly.geo<-read.csv(paste0(data.dir,"/Geolocator data/Stopover_table_PIEFL.csv"))
head(pfly.geo)
pfly.geo$year.start<-substr(pfly.geo$SO_start,7,10)
pfly.geo$year.end<-substr(pfly.geo$SO_end,7,10)
pfly.geo$month.start<-substr(pfly.geo$SO_start,4,5)
pfly.geo$month.end<-substr(pfly.geo$SO_end,4,5)
pfly.geo<-pfly.geo[pfly.geo$SO_length_discrete_day>=30|pfly.geo$country=="United Kingdom",] # Only long stops, or UK!
#pfly.geo<-pfly.geo[pfly.geo$dead=="N",]
head(pfly.geo)

# Create spatial polygons from locations
coords<-cbind(pfly.geo$SO_median_long,pfly.geo$SO_median_lat)
sp<-SpatialPoints(coords)
# make spatial data frame
spdf<-SpatialPointsDataFrame(coords, pfly.geo)
proj4string(spdf)<-CRS("+proj=longlat +datum=WGS84")
spdf1<-spTransform(spdf,CRS(proj4string(hex.grid1)))

# Plot locations
head(spdf1)
names(spdf1)[1]<-"name"
(names<-unique(spdf1$name))
par(mfrow=c(1,1))
plot(flyway1,col="light gray",axes=T)
#for (i in 1:length(names)){
all.bird.dat<-lapply(1:length(names), function(i){
  one.bird<-spdf1[spdf1$name==names[i],]
  one.bird@data
  years<-unique(one.bird$year.start)
  #for (j in years){
  one.bird.dat<-lapply(years, function(j){
    one.year<-one.bird[one.bird$year.start==j,]
    breed.loc<-one.year[one.year$SO_median_lat==max(one.year$SO_median_lat) & one.year$SO_median_lat>33,]
    plot(breed.loc,col=2,pch=16,add=T) 
    non.breed.loc<-one.year[one.year$SO_median_lat==min(one.year$SO_median_lat) & one.year$SO_median_lat<33,]
    plot(non.breed.loc,col=4,pch=16,add=T) 
    locs<-rbind(breed.loc,non.breed.loc)
    return(locs)
  })
  one.bird.locs<-do.call(rbind,one.bird.dat)
  one.bird.locs@data
  return(one.bird.locs)
})
all.bird.locs<-do.call(rbind,all.bird.dat)
#write.csv(all.bird.locs@data,paste0(data.dir,"/test cuckoo data.csv"))

# Separate into breeding and non-breeding grounds
breed.locs<-all.bird.locs[all.bird.locs$SO_median_lat>33,]
non.breed.locs<-all.bird.locs[all.bird.locs$SO_median_lat<33,]

##1 Extract breeding ground probabilities
# Estimate kernel density
df<-data.frame(coordinates(breed.locs))
coords.kde<-kde(df) 
coords.raster<-raster(coords.kde,crs=CRS(proj4string(breed.locs)))
# Extract used grid cells from kde
hex.grid.cr<-crop(hex.grid1,extent(coords.raster))
int<-extract(coords.raster,hex.grid.cr,df=T,fun=mean)
hex.grid.cr$breed.prob<-int$layer
hex.grid.cr[is.na(hex.grid.cr$breed.prob)==T,'breed.prob']<-0 # convert NAs to zero
hex.grid.cr[hex.grid.cr$land==0,'breed.prob']<-0 # convert sea cells to zero
hex.grid.cr$breed.prob<-hex.grid.cr$breed.prob/sum(hex.grid.cr$breed.prob) # convert to a proportion
# Add to hex grid
hex.grid1<-merge(hex.grid1,hex.grid.cr)
hex.grid1[is.na(hex.grid1$breed.prob)==T,'breed.prob']<-0
summary(hex.grid1)

##2 Extract non-breeding ground probabilities
# Estimate kernel density
df<-data.frame(coordinates(non.breed.locs))
coords.kde<-kde(df) 
coords.raster<-raster(coords.kde,crs=CRS(proj4string(non.breed.locs)))
# Extract used grid cells from kde
hex.grid.cr<-crop(hex.grid1,extent(coords.raster))
int<-extract(coords.raster,hex.grid.cr,df=T,fun=mean)
hex.grid.cr$non.breed.prob<-int$layer
hex.grid.cr[is.na(hex.grid.cr$non.breed.prob)==T,'non.breed.prob']<-0 # convert NAs to zero
hex.grid.cr[hex.grid.cr$land==0,'non.breed.prob']<-0 # convert sea cells to zero
hex.grid.cr$non.breed.prob<-hex.grid.cr$non.breed.prob/sum(hex.grid.cr$non.breed.prob) # convert to a proportion
# Add to hex grid
hex.grid1<-merge(hex.grid1,hex.grid.cr)
hex.grid1[is.na(hex.grid1$non.breed.prob)==T,'non.breed.prob']<-0
summary(hex.grid1)

## Plot!
plot(hex.grid1,col=c("light blue","white")[hex.grid1$land+1])
pal<-c("white",brewer.pal(8,"YlOrRd"))
plot(hex.grid1[hex.grid1$breed.prob>0,],col=pal[9],add=T)
classes<-as.numeric(cut(hex.grid1[hex.grid1$non.breed.prob>0,]$non.breed.prob,seq(-.01,0.06,by=.0075)))
plot(hex.grid1[hex.grid1$non.breed.prob>0,],col=pal[classes],add=T)
plot(flyway1,add=T)

## Add to arena.new and save
hex.grid1$cell<-as.numeric(substring(hex.grid1$cell,3))
arena.new<-merge(arena.new,hex.grid[,c(1,5,6)],by="cell")
names(arena.new)[7:8]<-c("pfly.breed","pfly.non.breed")
#save(arena.new,file=paste0(data.dir,"/Modelling arena extended.RData"))



