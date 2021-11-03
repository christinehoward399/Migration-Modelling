######################################################################
# Code for creating a hexagonal grid, extracting NDVI for each cell, #
###################################################################### 


###############################
## 1. Setting up environment ##
###############################

### Check for and install all required packages ###

## Which packages
list.packages <- c("rgeos","sp","raster","rgdal","maptools","plyr","snow","snowfall",
                   "chron","RColorBrewer","e1071","jpeg","png","dplyr","dggridR","geosphere")

## Function to install packages
check.install.packages <- function(list.packages){
  for(req.lib in list.packages){
    is.installed <- is.element(req.lib, installed.packages()[,1])
    if(is.installed == FALSE){install.packages(req.lib)}
    require(req.lib,character.only=TRUE)
  }
}

## Install packages
check.install.packages(list.packages)
#req.lib<-"dggridR"


### Set directories and projections ###
which.pc<-1 # 1 is Swiss, 2 is laptop
folder<-switch(which.pc,"C://Users/TM","C://Users/Tom")
## Main directories
main.dir<-paste0(folder,"/Dropbox/Work/Postdocs/Durham/Migration project")
data.dir<-paste0(folder,"/Dropbox/Work/Postdocs/Durham/Migration project/Data")
data.dir2<-paste0(folder,"/Dropbox/Work/Postdocs/Durham/RSPB FCS project/Data")
data.dir3<-paste0(folder,"/Dropbox/Work/Postdocs/Vogelwarte/Dynamic migration model/Data")

## Selecting a sensible projection to put grid on 
# Mollweide projection - was recommended to me once for looking at Europe-Africa
r.proj<-"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

## Directory and projection of NDVI data
#ndvi.dir<-"Y://NDVI Data/Globe/"
ndvi.dir<-paste0(folder,"Dropbox/Work/Postdocs/Vogelwarte/Phenology shift/Data/NDVI/VHP_SM_SMN")
ndvi.proj<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

## Check for any temporary files (left over from extraction of raster NDVI data)
rasterOptions(tmpdir=paste0(main.dir,"/Raster temp"))
rasterOptions()$tmpdir
showTmpFiles()
removeTmpFiles()


### Load world map and crop to flyway ###
world <- readOGR(dsn=paste0(data.dir2,"/World map"),
                 layer="ne_10m_admin_0_countries")
flyway<-crop(world,extent(-25,58,-40,70.5))
flyway<-flyway[flyway$ADMIN!="Greenland",]
plot(flyway)


##########################
## 2. Creating the grid ##
##########################

### Create hexagonal grid using the dggridR package ###

## Generate grid with intercell spacing of ~150km
#dggs<-dgconstruct(spacing=150,metric=TRUE,resround='nearest')
dggs<-dgconstruct(spacing=500,metric=TRUE,resround='nearest')
max.cell<-dgmaxcell(dggs)
grid<-dgcellstogrid(dggs,1:max.cell,frame=T,wrapcells=TRUE)
names(grid)[1]<-"lon"

## Convert grid to spatial polygon
grid.list<-split(grid,grid$cell)
# only want lon-lats in the list, not the names
grid.list<-lapply(grid.list, function(x) { x[!names(x) %in% c("lon","lat")] <- NULL; x })
# create SpatialPolygons Object, convert coords to polygon
ps<-lapply(grid.list, Polygon)
# add id variable
p1<-lapply(seq_along(ps), function(i) Polygons(list(ps[[i]]),ID=i))
# create SpatialPolygons object
grid.polys<-SpatialPolygons(p1, proj4string = CRS("+proj=longlat +datum=WGS84") ) 
grid.polys.df<-SpatialPolygonsDataFrame(grid.polys,data.frame(id = unique(grid$cell)))

## Work out which cells are predominantly land
flyway.d<-gUnaryUnion(flyway) # Dissolve flyway for calculations
cells.intersect<-raster::intersect(flyway.d,grid.polys.df) # Intersect flyway and grid
# Calculate proportional area of land in each cell
cells.intersect1<-spTransform(cells.intersect,CRS(r.proj))
grid.polys.df1<-spTransform(grid.polys.df,CRS(r.proj))
cells.intersect$prop.area <- sapply(cells.intersect$id, function(i) 
  gArea(cells.intersect1[cells.intersect1$id==i,])/gArea(grid.polys.df1[grid.polys.df1$id==i,]))
# Subset cells by land coverage e.g. >= 25% land coverage
#land.cells<-cells.intersect[cells.intersect$prop.area>=.5,]
land.cells<-cells.intersect[cells.intersect$prop.area>=.25,]
grid.polys.df$land<-ifelse(grid.polys.df$id %in% land.cells$id,1,0)

## Create final grid
# Restrict to grid cells in flyway
cells.to.keep<-crop(grid.polys.df,extent(flyway.d))
hex.grid<-grid.polys.df[grid.polys.df$id %in% cells.to.keep$id,]
hex.grid$x<-coordinates(hex.grid)[,1]
hex.grid$y<-coordinates(hex.grid)[,2]
names(hex.grid)[1]<-"cell"
# Transform to chosen projection
flyway1<-spTransform(flyway,CRS(r.proj))
hex.grid1<-spTransform(hex.grid,CRS(r.proj))
hex.grid1$cell<-1:dim(hex.grid1)[1]
row.names(hex.grid1)<-as.character(1:dim(hex.grid1)[1])

### Test plot
par(mfrow=c(1,1),mar=c(0,0,0,0))
plot(hex.grid1[hex.grid1$land==0,],col="#c6dbef",border="dark grey",lwd=1)
plot(hex.grid1[hex.grid1$land==1,],col="#74c476",border="light grey",lwd=1,add=T)
plot(flyway1,border=1,add=TRUE)

### Save
# Save spatial grid
#writeOGR(hex.grid1,dsn=data.dir,layer="Hex grid 500km",driver="ESRI Shapefile")
# Save flyway
#writeOGR(flyway1,dsn=data.dir,layer="Flyway polygon 2020",driver="ESRI Shapefile")



#######################################
## 3. Extracting NDVI for grid cells ##
#######################################

## NDVI directory
ndvi.dir1<-paste0(data.dir,"/NDVI/VHP_SM_SMN")

## Select NDVI data
ndvi.files<-dir(ndvi.dir1)
ndvi.unique<-substr(dir(ndvi.dir1),17,23)
weeks<-1:52
days<-seq(1,358,7)
year<-2018

## Load grid
hex.grid1<-readOGR(dsn=data.dir,layer="Hex grid 500km")
flyway1<-readOGR(dsn=data.dir,layer="Flyway polygon 2020")

## Set up environment for extracting NDVI
hex.grid.ndvi<-spTransform(hex.grid1,CRS(ndvi.proj)) # put grid on projection of NDVI rasters
cells<-hex.grid.ndvi$cell

# Run for different grid years
start.time <- Sys.time()
## Setting up envt for each processor
sfInit(parallel = T, cpus = 6) 
sfExport("hex.grid.ndvi","cells","weeks","days","year",
         "ndvi.dir1","ndvi.unique","ndvi.files") # All objects needed
sfLibrary(sp)
sfLibrary(raster)
sfLibrary(rgdal)
sfLibrary(rgeos)
sfLibrary(maptools)
sfLibrary(chron)
options(error=traceback)

## For each year (to average across)
ndvi.data<-sfClusterApplyLB(cells, function(j) { 
  
  ## Subset raster and grid to one cell
  one.cell<-hex.grid.ndvi[hex.grid.ndvi$cell==j,]
  head(one.cell)
  
  ## If land cell
  if (one.cell$land==1){
  
    ## For each day of year
    one.cell.ndvi<-lapply(weeks, function(k) {
  
      # Name of 
      if(nchar(k)==1){ ndvi.name<-paste0(year,"00",k) } 
      if(nchar(k)==2){ ndvi.name<-paste0(year,"0",k) } 
      
      # If data for that year and day exist
      if (ndvi.name %in% ndvi.unique){ 
        ## now read in the NDVI data:
        ndvi.file<-ndvi.files[ndvi.unique %in% ndvi.name] 
        ndvi <- raster(paste0(ndvi.dir1,"/",ndvi.file))
        one.ndvi<-crop(ndvi,extent(one.cell)+1)
        # plot(one.ndvi)
        # plot(one.cell,add=T,lwd=2)
        
        # Extract NDVI - either weighted mean or max value per cell
        ndvi.data<-as.data.frame(na.omit(extract(one.ndvi,one.cell,weights=T,cellnumbers=T)))
        ndvi.data<-ndvi.data[ndvi.data$value>=0,]
        mean.ndvi<-sum(ndvi.data$weight*ndvi.data$value)/sum(ndvi.data$weight)
        # uq.ndvi<-as.numeric(quantile(ndvi.data[ndvi.data$value>=0,]$value)[4])
        #max.ndvi<-max(ndvi.data$value)
        
        # Return
        return(mean.ndvi)
      }
  
    })
  
  #Combine and interpolate across days
  all.ndvi<-do.call(rbind,one.cell.ndvi)
  ndvi.int<-spline(x=days,y=all.ndvi,xout=1:365,method="natural")
  ndvi.int.y<-ndvi.int$y
  # plot(all.ndvi~days)
  # with(ndvi.int,lines(y~x))
    
  ## If not land cell
  } else {
    ndvi.int.y<-rep(0,365)
  }
    
  # Save interpolated ndvi object
  cell.data<-hex.grid.ndvi@data[hex.grid.ndvi@data$cell==j,]
  one.cell.ndvi.int<-cbind(cell.data,t(ndvi.int.y))
  
  # Return object
  return(one.cell.ndvi.int)
})

# Combine and save all cells for one year
ndvi.all<-do.call(rbind,ndvi.data)
colnames(ndvi.all)[-c(1,2,3,4)]<-as.character(1:365)
write.csv(ndvi.all,file=paste0(data.dir,"/NDVI NOAA STAR 500km mean ",year,".csv"))
  
end.time <- Sys.time()
(time.taken <- end.time - start.time)

sfStop() # Stop!

## Remove temp files
showTmpFiles()
removeTmpFiles(h=1)


#test<-read.csv(file=paste0(data.dir,"/NDVI 150 km cells Cuckoo test 2019.csv"))


################################################
## 4. Create 'modelling arena' from NDVI grid ##
################################################

## Load grid
hex.grid1<-readOGR(dsn=data.dir,layer="Hex grid 500km")
flyway1<-readOGR(dsn=data.dir,layer="Flyway polygon 2020")

## Load processed ndvi data
ndvi.data<-read.csv(file=paste0(data.dir,"/NDVI NOAA STAR 500km mean 2018.csv"))[,-1]
head(ndvi.data); dim(ndvi.data)

## Subset
arena.new<-ndvi.data[,1:4]
#arena.new<-hex.grid1@data
arena.new<-arena.new[!duplicated(arena.new$cell),]
head(arena.new); dim(arena.new)
#arena.new$cell<-as.numeric(substring(arena.new$cell,3))

## Define breeding and non-breeding cells

# Breeding
## breeding.cells<-c(2254,2292,2293,2330)
## arena.new$breed<-ifelse(arena.new$cell %in% breeding.cells,1,0)
# breeding.cells.new<-c(2109,2146,2147,2184,2222,2223,2260)
# arena.new$breed<-ifelse(arena.new$cell %in% breeding.cells.new,1,0)
#breeding.cells.new<-c(22,23)
breeding.cells.new<-c(102)#,96,99)
arena.new$breed<-ifelse(arena.new$cell %in% breeding.cells.new,1,0)
#Test plot
plot(flyway1,col="grey50",bg="light blue",axes=TRUE)
plot(hex.grid1[hex.grid1$land==1,],border="orange",add=T,lwd=1)
plot(hex.grid1[hex.grid1$cell %in% breeding.cells.new,],add=T,col=2)

# Non-breeding
# Cuckoo non breeding
# non.breeding.cells<-c(780,817,818,855,856,857,892,893,894,930,931,932,968,969)
# arena.new$non.breed<-ifelse(arena.new$cell %in% non.breeding.cells,1,0)
#non.breeding.cells<-c(67,76,77)
non.breeding.cells<-c(266,275,276,285,293,294,304)
arena.new$non.breed<-ifelse(arena.new$cell %in% non.breeding.cells,1,0)

# Test plot
# plot(flyway1,col="grey50",bg="light blue",axes=TRUE)
# plot(hex.grid1[hex.grid1$land==1,],border="orange",add=T,lwd=1)
plot(hex.grid1[hex.grid1$cell %in% non.breeding.cells,],add=T,col=2)

## Shrike non breeding
##non.breeding.cells2<-c(289,290,327,328,329,365,366)
##arena.new$non.breed2<-ifelse(arena.new$cell %in% non.breeding.cells2,1,0)
## Eastern Africa non breeding
##non.breeding.cells<-c(931,932,968,969,970,1006,1007,1008,1009,1047,1048,1049,1086,1087,1088)
##arena.new$non.breed<-ifelse(arena.new$cell %in% non.breeding.cells,1,0)
## Eastern Africa non breeding 2 - further south
##non.breeding.cells4<-c(715,716,753)
##arena.new$non.breed4<-ifelse(arena.new$cell %in% non.breeding.cells4,1,0)

## Save modelling arena
save(arena.new,file=paste0(data.dir,"/Modelling arena 500km_Cuckoo.RData"))


###################################################
## 5. Work out the neighbours for each grid cell ##
###################################################

# NB. I think it's a bit messy having different objects representing the same landscape
#  - hex.grid1, ndvi.data, arena.new, nbrs_small - could possibly be streamlined

## Load grid
hex.grid1<-readOGR(dsn=data.dir,layer="Hex grid 500km")
flyway1<-readOGR(dsn=data.dir,layer="Flyway polygon 2020")

## Load modelling arena
load(file=paste0(data.dir,"/Modelling arena 500km_Cuckoo.RData"))

## Work out neighbouring cells
#arena.new<-hex.grid.ndvi@data
nbrs <- arena.new[,c("cell","x","y","land")]

# Run through each cell
nbrs2 <- lapply(1:dim(nbrs)[1],function(i){
  
  ## If testing
  # i=294
  # print(i)

  ## Focal cell
  focal<-nbrs[nbrs$cell==i,]

  ## Crop grid around focal cell
  target<-nbrs[nbrs$x>=(focal$x-10) & nbrs$x<=(focal$x+10) & # change if changing max. flight distances per time step
              nbrs$y<=(focal$y+10) & nbrs$y>=(focal$y-1) & # NB. this prevents birds from going south, to save computation time later
              !(nbrs$x==focal$x & nbrs$y==focal$y),]
  target1<-dat<-rbind(focal,target) # include focal cell at start

  ## Calculate flight distances from focal cell
  # Distances in km 
  # NB. Could use 'distVincentyEllipsoid' for greater accuracy
  dat$dist<-distVincentySphere(focal[c("x","y")],target1[c("x","y")])/1000 
  ## Remove cells out of range
  dat1<-dat[dat$dist<=1100,] # Potential FR of Cuckoo is 521 in 12 hrs, or 1042 in 24 hrs 
  
  ## Test plot
  plot(hex.grid1,axes=T,border="light gray")
  plot(flyway1,add=T)
  plot(hex.grid1[hex.grid1$cell %in% dat1$cell,],col=3,add=T)
  plot(hex.grid1[hex.grid1$cell==nbrs[i,"cell"],],col=2,add=T)
  
  ## Save neighbour data
  dat1$focal.cell <- focal$cell
  dat1$focal.x <- focal$x
  dat1$focal.y <- focal$y
  dat1$focal.land <- focal$land
  
  ## Return
  return(dat1)
})
nbrs <- do.call(rbind,nbrs2)
rm(nbrs2)
nbrs <- nbrs[,c("focal.cell","focal.x","focal.y","focal.land","cell","x","y","land","dist")]
nbrs_small <- nbrs[,c('focal.cell','cell','dist')]
names(nbrs_small) <- c('cell','destination','dist')
head(nbrs_small,20)
dim(nbrs_small)
summary(nbrs_small)
# Save or load
#save(nbrs_small,file=paste0(data.dir,"/Neighbouring cells 500km_Cuckoo.RData"))

load(file=paste0(data.dir,"/Neighbouring cells 500km_Cuckoo.RData"))
head(nbrs_small)

head(hex.grid1)














### TESTING...

## Testing non breeding grounds
plot(grid.ndvi[grid.ndvi$land==0,])
#plot(grid.ndvi[grid.ndvi$land==0,],col="#c6dbef",#main=titles[x],
#    axes=T,xaxt="n",yaxt="n",border="dark gray",lwd=.5
#    ,xlim=c(1e+06,2e+06),ylim=c(-1e+06,.2e+06))
#,ylim=c(1e+06,6.8e+06),xlim=c(-2e+6,3e+06))
plot(grid.ndvi[grid.ndvi$land==1,],col=pal[classes],
     add=T,xaxt="n",yaxt="n",border="light gray",lwd=.5)
plot(flyway1,add=T,lwd=.5)

#test<-crop(grid.ndvi,extent(c(0.8e+06,2.5e+06,-0.5e+06,.5e+06)))
test<-crop(grid.ndvi,extent(c(0.6e+06,1.5e+06,4.5e+06,6e+06)))
plot(test)
plot(test[test$land==1,],col="light green",add=T)
plot(flyway1,add=T,lwd=2)
(cells<-unique(test$cell))
plot(grid.ndvi[grid.ndvi$cell %in% 
                 cells[c(21,25,26,31,36,37,42)],],col="yellow",add=T)

dev.off()

## Test plot 1
#par(mfrow=c(1,1))
par(mfrow=c(2,2))
sort(unique(ndvi.cells$day))
titles<-c("15 Jan","15 Feb","1 Apr","1 May")
days<-c(1,32,76,106)
days<-c(3,18)

ndvi.cells$cell<-paste0("ID",ndvi.cells$cell)
head(ndvi.cells)

for (x in 1){
  ## Combine
  grid.ndvi<-merge(hex.grid[,1],ndvi.cells[ndvi.cells$day==days[x],],by="cell")
  dim(grid.ndvi);summary(grid.ndvi)
  ## Plot
  pal<-brewer.pal(9,"Greens")
  classes<-as.numeric(cut(grid.ndvi[grid.ndvi$land==1,]$ndvi,seq(-.01,1.01,by=.102)))
  plot(grid.ndvi[grid.ndvi$land==0,],col="#c6dbef",main=titles[x],
       axes=T,xaxt="n",yaxt="n",border="dark gray",lwd=.5)
  #,xlim=c(1.3e+06,1.8e+06),ylim=c(5.3e+06,5.8e+06))
  #,ylim=c(-1e+06,6.8e+06),xlim=c(-2e+6,3e+06))
  plot(grid.ndvi[grid.ndvi$land==1,],col=pal[classes],
       add=T,xaxt="n",yaxt="n",border="light gray",lwd=.5)
  plot(flyway1,add=T,lwd=.5)
  # Add breeding grounds
  breed.gds<-gUnaryUnion(grid.ndvi[grid.ndvi$cell %in% paste0("ID",arena.new[arena.new$breed==1,'cell']),])
  plot(breed.gds,add=T,border="yellow",lwd=2)
  # Add non-breeding grounds
  non.breed.gds<-gUnaryUnion(grid.ndvi[grid.ndvi$cell %in% paste0("ID",arena.new[arena.new$non.breed==1,'cell']),])
  plot(non.breed.gds,add=T,border="red",lwd=2)
  
}

summary(ndvi.cells)
summary(hex.grid)

## Test plot 2
hex.grid<-readOGR(dsn=data.dir,layer="Hexagonal grid")
flyway1<-readOGR(dsn=data.dir,layer="Flyway polygon")
head(hex.grid)
#hex.grid@data$test<-substr(hex.grid@data$cell,3)
head(ndvi.cells)
grid.ndvi<-merge(hex.grid[,2:3],ndvi.cells,by=c("x","y"))
summary(grid.ndvi)
# Set up envt
pal<-brewer.pal(9,"Greens")
classes<-as.numeric(cut(grid.ndvi[grid.ndvi$land==1,]$ndvi,seq(-.01,.29,by=.033)))
summary(grid.ndvi$ndvi)
# Plot
head(grid.ndvi)
plot(grid.ndvi[grid.ndvi$land==0,],col="#c6dbef",#main=titles[x],
     axes=T,xaxt="n",yaxt="n",border="dark gray",lwd=.5)#,
#xlim=c(1.8e+06,2.8e+06),ylim=c(-3.4e+06,-2.6e+06))
plot(grid.ndvi[grid.ndvi$land==1,],col=pal[classes],
     add=T,xaxt="n",yaxt="n",border="light gray",lwd=.5)
plot(flyway1,add=T,lwd=.5)
axis(1); axis(2)


