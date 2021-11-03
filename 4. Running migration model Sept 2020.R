
################################################
### Code for running gridded migration model ###
################################################


###############################
## 1. Setting up environment ##
###############################

# Clear R's workspace #
rm(list=ls()); graphics.off()

### Check for and install all required packages ###
# Which packages
list.packages <- c("raster","rgdal","sp","maptools","rgeos","RColorBrewer",
                   "ggplot2","grid","gridExtra","data.table","NISTunits",
                   "snow","snowfall","png")
# Function to install packages
check.install.packages <- function(list.packages){
  for(req.lib in list.packages){
    is.installed <- is.element(req.lib, installed.packages()[,1])
    if(is.installed == FALSE){install.packages(req.lib)}
    require(req.lib,character.only=TRUE)
  }
}
check.install.packages(list.packages)
#
keys <- c("cell","e","a")

### Set directories ###
### Set directories and projections ###
which.pc<-1 # 1 is Swiss, 2 is laptop
folder<-switch(which.pc,"C://Users/TM","C://Users/Tom")
# Working dir
setwd(paste0(folder,"/Dropbox/Work/Postdocs/Durham/Migration project"))
# Data dir
data.dir<-paste0(folder,"/Dropbox/Work/Postdocs/Durham/Migration project/Data")
# Results directory
res.dir<-paste0(folder,"/Dropbox/Work/Postdocs/Durham/Migration project/")


###################################
## 2. Set species and parameters ##
###################################

### Set species and parameters ###
# NB. These would be better set up in single parameter object

## Choose species
s=2
species<-c("Pied Flycatcher","Cuckoo")[s]

# Constant parameters 
NI <- 1000 # number of individuals for simulation
#MaxD <- c(15,60)[s] # No. of days in season; Pied fly: 1 (30th March) and 35 (13th April); Cuckoo: 
MaxD <- c(15,56)[s] # No. of days in season; Pied fly: 1 (30th March) and 35 (13th April); Cuckoo: 
#julian(x=4,d=1,y=2006,origin=c(x=3,d=29,y=2006))

# Energy parameters
MaxE <- 8 # Max energy level
#e_amplitude <- c(1,1)[s] ## Energy gain from foraging # Cuckoo should be 0.71 (?)
e_amplitude <- c(1.55,0.68)[s] ## Energy gain from foraging # Cuckoo should be 0.71 (?)
overnight_cost <- c(0.29,0.18)[s] ## Overnight energy loss
flight_cost <- c(536,536)[s] 
##flight_cost <- c(0.57,0.36)[s] ## Energy cost of flight # OLD

## Set max NDVI, to scale ndvi effects
max.ndvi<-0.4762654 ## Max NDVI, for scaling ndvi effects - estimated from max NDVI across landscape

### Make folders to put decisions and values in for each day, and sim output for each day ###
# Make sure warnings are off to do that
DandVfldr <- paste0(res.dir,sprintf("Numerical output/DecAndVals/MaxE_%d, Fcos_%d, Eamp_%0.3f, ONcost_%0.3f",MaxE,flight_cost,e_amplitude,overnight_cost))
fatesFldr <- paste0(res.dir,sprintf("Numerical output/Histories/MaxE_%d, Fcost_%d, Eamp_%0.3f, ONcost_%0.3f",MaxE,flight_cost,e_amplitude,overnight_cost))
suppressWarnings(dir.create(DandVfldr))
suppressWarnings(dir.create(fatesFldr))


##########################################
## 3. Load necessary functions and data ##
##########################################

## Set up some useful functions
source('Functions/LoadFunctions 2019.R') # Only one used function in this file

## Load modelling arena
load(file=paste0(data.dir,"/Modelling arena 500km_Cuckoo.RData"))
summary(arena.new); dim(arena.new)

## Load data on neighbouring cells
###if (s==1){load(file=paste0(data.dir,"/Neighbouring cells 3 cells extended.RData"))}
if (s==2){load(file=paste0(data.dir,"/Neighbouring cells 500km_Cuckoo.RData"))}
head(nbrs_small); dim(nbrs_small)

## Load NDVI data
ndvi.data<-read.csv(file=paste0(data.dir,"/NDVI NOAA STAR 500km mean 2018.csv"))[,-1]
colnames(ndvi.data)[-c(1,2,3,4)]<-as.character(1:365)
# Cuckoo, subset to 30th March-24th May
if (s==2){ndvi.simpl<-ndvi.data[,colnames(ndvi.data) %in% c("cell",as.character(89:144))]} 
colnames(ndvi.simpl)[-1]<-as.character(1:MaxD) # Rename NDVI days
head(ndvi.simpl)


##########################
## 4. Running the model ##
##########################

## Start time ##
start.time <- Sys.time()

### Set terminal reward ###
source('Functions/TerminalReward 2019.R')
nextFit_small <- data.frame(cbind(ID = paste(TR$cell,TR$e,TR$a,sep="_"),V = as.numeric(TR$V)))
nextFit_small$V <- as.numeric(levels(factor(nextFit_small$V))[factor(nextFit_small$V)])
#nextFit_small$V <- as.numeric(levels(nextFit_small$V)[nextFit_small$V])
nextFit_sm <- as.numeric(nextFit_small$V)
names(nextFit_sm) <- as.character(nextFit_small$ID)
# nextFit <- as.matrix(TR)
#nextFit <- data.table(nextFit, key=keys)

### Run the dynamic program ###
source('Functions/DynamicProgram Sept 2020.R')

### And now the forward sim ###
source('Functions/FwdSim Sept 2020.R')

## End time ##
end.time <- Sys.time()
(time.taken <- end.time - start.time)


# now consider the dynamic model
# bird's state to be characterised by
# (x,y,e,a,t,d) --> x and y give location, e gives energy reserves, a gives airborne (1) or not (0), t gives day, d gives (start of) daytime (1) or night-time (2)

