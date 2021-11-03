
# Load packages
# Which packages
list.packages <- c("lubridate","pspline")
# Function to install packages
check.install.packages <- function(list.packages){
  for(req.lib in list.packages){
    is.installed <- is.element(req.lib, installed.packages()[,1])
    if(is.installed == FALSE){install.packages(req.lib)}
    require(req.lib,character.only=TRUE)
  }
}
#req.lib<-"rgdal"
check.install.packages(list.packages)

# Set dir
which.pc<-1 # 1 is Swiss, 2 is laptop
folder<-switch(which.pc,"C://Users/TM","C://Users/Tom")
data.dir<-paste0(folder,"/Dropbox/Work/Postdocs/Durham/Migration project/Data")

#1# BTO data
bto.data<-read.delim(paste0(data.dir,"/Mass data/Biometrics_extraction_31052018_BAILLIE.txt"),sep="|", dec = ".")
bto.data<-bto.data[,colnames(bto.data) %in% colnames(bto.data)[1:58]] #lose some col names
bto.data<-bto.data[is.na(bto.data$weight)==F,] #only records with weights
bto.data<-bto.data[bto.data$scheme=="GBT",] #only UK records
head(bto.data)
# One species
j=1
species<-c("Pied Flycatcher","Nightingale","Cuckoo")
bto1<-bto.data[bto.data$english_name==species[j],]
# Get julian day
bto1$jday<-NA
for (i in 1:dim(bto1)[1]){
  bto1$jday[i]<-lubridate::yday(as.Date(paste0(bto1$year[i],"-",bto1$month[i],"-",bto1$day[i])))
}
hist(bto1$jday)

table(bto1$age)
with(bto1,plot(weight~jday,main=as.character(species[j]),type="n"))#,ylim=c(7,25),xlim=c(50,300)))
age2<-bto1[bto1$age=="3"|bto1$age=="3J",]
with(age2,points(weight~jday,pch=16,col=1))
age3<-bto1[bto1$age=="4"|bto1$age=="5"|bto1$age=="6"|bto1$age=="2",]
with(age3,points(weight~jday,pch=16,col=2))
age1<-bto1[bto1$age=="1"|bto1$age=="1J",]
with(age1,points(weight~jday,pch=16,col="dark grey"))
legend("bottomleft",c("pullus","first-years","second-years+"),col=c("light grey",1,2),pch=16)

##A## Working out per day mass gain
# Pied fly
#departing<-bto1[bto1$weight<100 & bto1$jday>=250 & bto1$jday<=300,]# & bto1$sex=="M",]
# Cuckoo
departing<-bto1[bto1$age=="3"|bto1$age=="3J",]# & bto1$sex=="M",]
#departing<-departing[departing$jday>150,]

# Across individuals
with(departing,plot(weight~jday,pch=16,col=1))
mod<-glm(weight~jday,data=departing)
abline(mod,lwd=2)
hist(departing$weight)
new.data<-data.frame(jday=c(250,251))
(predict(mod,new.data))
# Pied fly 0.1g per day

# Within individuals
departing$ring_no<-factor(departing$ring_no)
table1<-table(departing$ring_no,departing$year)
table1<-apply(table1,1,max) # Any birds with two captures in a given year
inds<-names(table1[table1>1])
# Checking if there are multiple years for any individuals
# test<-departing[departing$ring_no %in% inds,]
# test$ring_no<-factor(test$ring_no)
# (multi.years<-table(test$ring_no,test$year))
changes.dat<-data.frame(ind=rep(NA,length(inds)),change=rep(NA,length(inds)))
for (x in 1:length(inds)){
  #x<-147
  ind1<-departing[departing$ring_no==inds[x],]
  for (y in 1:(dim(ind1)[1]-1)){
    change1<-(ind1$weight[y+1]-ind1$weight[y])/(ind1$jday[y+1]-ind1$jday[y])
    if(y==1){changes<-change1
    } else {changes<-c(changes,change1)}
  }
  changes.dat$ind[x]<-ind1$ring_no[1]
  changes.dat$change[x]<-max(changes[is.finite(changes)==T])
}
(changes.dat<-changes.dat[is.finite(changes.dat$change)==T & changes.dat$change>0,])
changes.quant<-quantile(changes.dat$change)
mean(changes.dat[changes.dat$change>=changes.quant[4],'change'])
changes.quant<-quantile(changes.dat$change,.95)
hist(changes.dat$change)


##B## Working out departing mass distributions
# departing<-departing[departing$jday>150,]
# table(departing$weight)
# with(departing,plot(weight~jday,pch=16,col=1))
# departing<-bto1[bto1$weight<100 & bto1$jday>=272 & bto1$jday<=280,]
# with(departing,points(weight~jday,pch=16,col=2))
# 
# # Subset to departing individuals
# quants<-as.numeric(quantile(departing$weight))
# departing<-departing[departing$weight>=quants[4],] # Top 75% percentile
# with(departing,points(weight~jday,pch=16,col=4))
# hist(departing$weight)
# table(departing$weight)
# # Convert to proportional weight
# int<-seq(11.04,17.9,0.857)
# departing$weight.class<-NA
# for (j in 1:dim(departing)[1]){
#   departing$weight.class[j]<-ifelse(departing$weight[j] < min(int), 0, findInterval(departing$weight[j], int))
#   if(departing$weight.class[j]==9){departing$weight.class[j]<-8}
# }
# hist(departing$weight.class)
# table(departing$weight.class)/length(departing$weight.class)
# mean(departing$weight.class)/8
# sd(departing$weight.class)/8

##C## Working out arriving mass distributions
# Pied fly
#arriving<-bto1[bto1$jday<130 & bto1$weight<14 & bto1$sex=="M",]
# Cuckoo
arriving<-bto1[bto1$jday<125 & bto1$sex=="M" & bto1$weight>80,]
with(arriving, plot(weight~jday,pch=16,col=1))
table(arriving$weight)
hist(arriving$weight)

#pdf(paste0(res.dir,"Graphical output/Presentation plot mass dist.pdf"),
#    width = 3, height = 2.5)
#par(mar=c(2,2,1,1))
#hist(bto1$weight)
#dev.off()

# Convert to proportional weight
#int<-seq(11.04,17.9,0.857) Pied fly
int<-seq(92.4,156,7.95) # Cuckoo
arriving$weight.class<-NA
for (j in 1:dim(arriving)[1]){
  arriving$weight.class[j]<-ifelse(arriving$weight[j] < min(int), 0, findInterval(arriving$weight[j], int))
  if(arriving$weight.class[j]==9){arriving$weight.class[j]<-8}
}
hist(arriving$weight.class)
arriving<-arriving[arriving$weight.class>0,]
table(arriving$weight.class)/length(arriving$weight.class)
mean(arriving$weight.class)/8
sd(arriving$weight.class)/8


## 2 Gibraltar data
gib.data<-read.csv(paste0(data.dir,"/Mass data/Pied_Flycatcher_Gibraltar_Data.csv"))
#gib.data<-read.csv(paste0(data.dir,"/Nightingale Gibraltar Data.csv"))
gib.data<-gib.data[is.na(gib.data$WEIGHT)==F,] #only records with weights
head(gib.data)

# Get julian day
gib.data$jday<-NA
for (i in 1:dim(gib.data)[1]){
  gib.data$jday[i]<-lubridate::yday(as.Date(gib.data$DATE[i],format="%d/%m/%Y"))
}
hist(gib.data$jday)
ages<-unique(gib.data$AGE)

par(mfrow=c(1,2))

# Spring
with(gib.data,plot(WEIGHT~jday,main="Nightingale: Spring",type="n",ylim=c(0,35),xlim=c(70,130)))
age2<-gib.data[gib.data$AGE %in% ages[c(4,5,7)],]
with(age2,points(WEIGHT~jday,pch=16,col=1))
age3<-gib.data[gib.data$AGE %in% ages[c(1:3,6)],]
with(age3,points(WEIGHT~jday,pch=16,col=2))
#lines(sm.spline(age3[age3$jday<140,]$jday,age3[age3$jday<140,]$WEIGHT,cv=T,norder=5),lwd=2,col=1)
legend("bottomleft",c("first-years","second-years+"),col=c(1,2),pch=16)

# Autumn
with(gib.data,plot(WEIGHT~jday,main="Nightingale: Autumn",type="n",ylim=c(0,35),xlim=c(200,300)))
with(age2,points(WEIGHT~jday,pch=16,col=1))
with(age3,points(WEIGHT~jday,pch=16,col=2))
#lines(sm.spline(age2[age2$jday>140,]$jday,age2[age2$jday>140,]$WEIGHT,cv=T,norder=5),lwd=2,col=2)

# Get mean + sd for spring migration
# Convert to proportional weight
int<-seq(11.04,17.9,0.857)
departing<-gib.data[gib.data$jday>70 & gib.data$jday<135 & gib.data$WEIGHT>5 
                    & gib.data$WEIGHT<16 & gib.data$SEX=="M",]
plot(departing$WEIGHT~departing$jday)
departing$weight.class<-NA
for (j in 1:dim(departing)[1]){
  departing$weight.class[j]<-ifelse(departing$WEIGHT[j] < min(int), 0, findInterval(departing$WEIGHT[j], int))
  if(departing$weight.class[j]==9){departing$weight.class[j]<-8}
}
hist(departing$weight.class)
departing<-departing[departing$weight.class>0,]
table(departing$weight.class)/length(departing$weight.class)
mean(departing$weight.class)/8
sd(departing$weight.class)/8


#3# Catalonia data
cat.data<-read.csv(paste0(data.dir,"/Mass data/Migration_SB_CAT.csv"),sep="|")
cat.data<-cat.data[is.na(cat.data$Mass)==F & cat.data$Mass<30,]
head(cat.data)
table(cat.data$Species_code)

# One species
j=2
species<-c("CUCCAN","FICHYP","LUSMEG")
cat.data1<-cat.data[cat.data$Species_code==species[j] & cat.data$Sex=="M",]
head(cat.data1)

# Get julian day
cat.data1$jday<-NA
for (i in 1:dim(cat.data1)[1]){
  cat.data1$jday[i]<-lubridate::yday(as.Date(cat.data1$Date[i],format="%d/%m/%Y"))
}
with(cat.data1,plot(Mass~jday))

# Get mean + sd for spring migration
# Convert to proportional weight
int<-seq(11.04,17.9,0.857)
departing<-cat.data1[cat.data1$jday<150,]
plot(departing$Mass~departing$jday)
departing$weight.class<-NA
for (j in 1:dim(departing)[1]){
  departing$weight.class[j]<-ifelse(departing$Mass[j] < min(int), 0, findInterval(departing$Mass[j], int))
  if(departing$weight.class[j]==9){departing$weight.class[j]<-8}
}
hist(departing$weight.class)
departing<-departing[departing$weight.class>0,]
table(departing$weight.class)/length(departing$weight.class)
mean(departing$weight.class)/8
sd(departing$weight.class)/8

# Get mean coordinates
departing$Site_name<-factor(departing$Site_name)
table(departing$Site_name)
mean(departing$X); mean(departing$Y)


#4# Morocco data
mor.data<-read.csv(paste0(data.dir,"/Mass data/Migration_SB_MAR.csv"),sep="|")
mor.data<-mor.data[is.na(mor.data$Mass)==F,]# & mor.data$Mass<30,]
head(mor.data)
table(mor.data$Species_code)

# One species
j=2
species<-c("CUCCAN","FICHYP","LUSMEG")
mor.data1<-mor.data[mor.data$Species_code==species[j] & mor.data$Sex=="M",]
head(mor.data1)

# Get julian day
mor.data1$jday<-NA
for (i in 1:dim(mor.data1)[1]){
  mor.data1$jday[i]<-lubridate::yday(as.Date(mor.data1$Date[i],format="%d/%m/%Y"))
}
with(mor.data1,plot(Mass~jday))

# Get mean + sd for spring migration
# Convert to proportional weight
int<-seq(11.04,17.9,0.857)
departing<-cat.data1[mor.data1$jday<150,]
plot(departing$Mass~departing$jday)
departing$weight.class<-NA
for (j in 1:dim(departing)[1]){
  departing$weight.class[j]<-ifelse(departing$Mass[j] < min(int), 0, findInterval(departing$Mass[j], int))
  if(departing$weight.class[j]==9){departing$weight.class[j]<-8}
}
hist(departing$weight.class)
departing<-departing[departing$weight.class>0,]
table(departing$weight.class)/length(departing$weight.class)
mean(departing$weight.class)/8
sd(departing$weight.class)/8

# Get mean coordinates
departing$Site_name<-factor(departing$Site_name)
table(departing$Site_name)
mean(departing$X); mean(departing$Y)



#5# ESF data
esf.data<-read.csv(paste0(data.dir,"/Mass data/ESF_FICHYP.csv"),sep="|")
esf.data.sites<-read.csv(paste0(data.dir,"/Mass data/ESF_sites_coord3.csv"),sep="|")
esf.data<-merge(esf.data,esf.data.sites,by="site")
head(esf.data)
ESF_sites_coord3
esf.tab<-table(esf.data$country)
esf.tab[esf.tab>100]

esf.data<-esf.data[is.na(esf.data$wgt)==F,]# & mor.data$Mass<30,]
head(esf.data)

# Get julian day
esf.data$jday<-NA
esf.data$date<-paste0(esf.data)
for (i in 1:dim(esf.data)[1]){
  esf.data$jday[i]<-lubridate::yday(as.Date(esf.data$Date[i],format="%d/%m/%Y"))
}
with(mor.data1,plot(Mass~jday))

# One site
j=2
species<-c("CUCCAN","FICHYP","LUSMEG")
mor.data1<-mor.data[mor.data$Species_code==species[j] & mor.data$Sex=="M",]
head(mor.data1)

# Get mean + sd for spring migration
# Convert to proportional weight
int<-seq(11.04,17.9,0.857)
departing<-cat.data1[mor.data1$jday<150,]
plot(departing$Mass~departing$jday)
departing$weight.class<-NA
for (j in 1:dim(departing)[1]){
  departing$weight.class[j]<-ifelse(departing$Mass[j] < min(int), 0, findInterval(departing$Mass[j], int))
  if(departing$weight.class[j]==9){departing$weight.class[j]<-8}
}
hist(departing$weight.class)
departing<-departing[departing$weight.class>0,]
table(departing$weight.class)/length(departing$weight.class)
mean(departing$weight.class)/8
sd(departing$weight.class)/8

# Get mean coordinates
departing$Site_name<-factor(departing$Site_name)
table(departing$Site_name)
mean(departing$X); mean(departing$Y)