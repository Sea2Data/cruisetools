#install.packages("data.table")
library(ggplot2)
library(dplyr)
library(tidyr)

# Read year-serialnumber list from aarsmaterialet
aarsmat <- read.table('fisk1932-2014_s.csv',sep=";",col.names=c("year","serialnumber"))

# Read NMDbiotic with snapshots
NMDbiotic <- data.table::fread("NMDbiotic.csv")
NMDbiotic$snapshottime = as.POSIXct(NMDbiotic$snapshotpath,format = "%Y-%m-%dT%H.%M.%OSZ")

# Check variable names
names(aarsmat)
names(NMDbiotic)

# Create sequence of snapshot dates
snt <- seq(from=min(NMDbiotic$snapshottime),to=max(NMDbiotic$snapshottime),length.out=30)

# Initialize matrix
dat_m <- matrix(ncol = 3, nrow = length(snt))

# Loop over snapshots and count stations in each set (x-axis)
k=1
for (t in snt){
  # Filter stations present only before t
  ind <- NMDbiotic$snapshottime<t
  # Calculate sets from year and serialnumbers
  NMDbiotic_sub <- unique(NMDbiotic[ind,c(1,6)])
  # unique(
  dat_m[k,1] <- dim(intersect(NMDbiotic_sub, aarsmat))[1]
  dat_m[k,2] <- dim(setdiff(NMDbiotic_sub, aarsmat))[1]
  dat_m[k,3] <- dim(setdiff(aarsmat, NMDbiotic_sub))[1]
  k <- k +1
}
colnames(dat_m) <- c("Both","NMDbiotic","Årsmaterialet")
data <- as.data.frame(dat_m)
data$snapshottime <- snt

# Gather data set for efficient ploting in ggplot
data2 <- gather(data,set,count,Both,Årsmaterialet,NMDbiotic)
data2$set <- factor(data2$set , levels=c("Årsmaterialet", "NMDbiotic", "Both") )
names(data2)


# Organize data by year and code the sets (clunky but works)
NMDbiotic$NMDbiotic <- 1
aarsmat$aarmat <- 2
yeardata <- full_join(NMDbiotic,aarsmat)
yeardata$aarmat[is.na(yeardata$aarmat)] =0
yeardata$NMDbiotic[is.na(yeardata$NMDbiotic)] =0
yeardata$set = "Both"
yeardata$set[(yeardata$NMDbiotic + yeardata$aarmat)==1]="NMDbiotic"
yeardata$set[(yeardata$NMDbiotic + yeardata$aarmat)==2]="Årsmaterialet"
names(yeardata)


# Plotting section

# Plot today's status of stations by set over years
ggplot(data=yeardata, aes(year, fill = set)) + geom_histogram() +
  ggtitle(paste("Status as of ",max(NMDbiotic$snapshottime,na=T)))

# Plot histogram over all the snapshots
ggplot(data=NMDbiotic, aes(snapshottime)) + geom_histogram() + 
  ggtitle("Effort per station from 1914 to (including) 2015")

# Plot number of stations by set over snapshottime
ggplot(data=data2) +
  geom_area(aes(x=snapshottime,y=count, fill=set))+
  ggtitle("Number of biotic stations from 1914 to (including) 2015")

# The today's status
today <- tail(data,n=1)
print(today)
total=NULL
total$both <- sum(today[1:3])
total$NMDbiotic <- sum(today[1:2])

print(paste("Total stations in both sets: ",total$both))
print(paste("Total stations in NMDbiotic: ",total$NMDbiotic))

status <- 100*today[1:3]/sum(today[1:3])

print(paste("Percentage in NMDbiotic ",sum(status[1:2]),"%"))

# How are we fairing?
time0 <- as.POSIXct("2020-04-01",format = "%Y-%m-%d")
ind <- data$snapshottime > time0
nilz <- data[ind,c(1,2)]
addst <-sum(tail(nilz,n=1)) - sum(nilz[1,])
print(paste("Stations added since 2020-04-01: ",addst))
dt <- tail(data[ind,4],n=1)-time0
vel <- addst/as.vector(dt)
print(paste("Average added stations per day: ",vel))

print(paste("Time to complete: ",(total$both - total$NMDbiotic)/(vel*356), " years"))

