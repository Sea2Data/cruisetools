# Script for various IBTS tasks
# By: Erik Olsen
# v. Feb 2019

###
# LIBRARIES
library("ggplot2")
library("RColorBrewer")
library("qqplotr")
library("lattice")

###
# SOURCES
setwd("~/ownCloud/Research/iBTS/IBTS Q1 2019/2019102/Cruistools_IBTS")
source("~/ownCloud/Research/iBTS/R IBTS/BackgroundMap.R")

#' SETTING SYSTEM TO NORWEGIAN
Sys.setlocale("LC_CTYPE", "no_NO")

#### IMPORT DATA FILES
# Import data files exported from S2D
# must first import samples.txt file in Excel with Latin Encoding, saving as .csv
# samples <- read.table("../data/S2D/catchsample.txt", header=TRUE, sep="\t", dec=".")
samples <- read.csv2("../data/S2D/catchsample.csv", header=TRUE, dec=".")
individual <- read.table("../data/S2D/individual.txt", header=TRUE, sep="\t", dec=".")
station <- read.table("../data/S2D/fishstation.txt", header=TRUE, sep="\t", dec=".")

# add serialno + sampleid column to be able to match samples and individuals
samples$ser.samp <- samples$f.serialnumber*100+samples$catchsampleid
individual$ser.samp <- individual$f.serialnumber*100+individual$c.catchsampleid

individual$c.species <-samples$catchcategory[match(individual$ser.samp, samples$ser.samp)]

# Lese loggfiler
logg.list <- list.files("../data/log/ACTIVITY/log/")
logg.station<-data.frame(lat=double(), lon=double(), stcode=integer())

for (i in 1: length(logg.list)){
  ld <- read.table(paste("../data/log/ACTIVITY/log/",logg.list[i], sep=""), sep=",")
  #regne ut Lat (V13) og Lon (V14)
  ld$lat <- as.numeric(substr(ld$V13, 1,2)) + as.numeric(substr(ld$V13, 3,9))/60
  ld$lon <- as.numeric(substr(ld$V14, 1,3)) + as.numeric(substr(ld$V14, 4,9))/60 * ifelse(substr(ld$V14,12,12)=="E", 1, -1)
  logg.station <- rbind(logg.station, cbind(ld$lat, ld$lon, ld$V5))
}
colnames(logg.station) <- c("lat", "lon", "stcode")

st.types <- data.frame(stcode=c(2800, 2801, 1000, 1001, 1600, 1601), sttype=c("MIK Start", "MIK Stop", "CTD Start", "CTD Stop", "BT Start", "BT Stop"))
logg.station$sttype <- st.types$sttype[match(logg.station$stcode, st.types$stcode)]


#######
# IBTS GRID MAPs w all station info from IBTS Q1 2019 survey
#MAP 1 - Survey area 2019 w KINO positions
png("../Kart/NorSquares w KINO positions.png", width=1000, height = 1000)
icesmap(xMin=-3,xMax=10,yMin=56,yMax=62) #norwegian area 2018

# ADD SQUARE NAMES
square.names <- read.csv2("~/ownCloud/Research/iBTS/R IBTS/NOR IBTS SQUARES.csv")
colnames(square.names) <- c("sq", "lat", "lon")

text(square.names$lon, square.names$lat, labels = square.names$sq, cex=1.5, col = "gray50")

# add positions for additional WP2 hauls for Mateos Riviera - KINO project
#png("NorSquares w KINO positions.png", width=1000, height = 1000)
kino.pos <- data.frame(lat=c(56.45, 58.5, 61.2167), lon=c(3.2167, 1.833, 2), name=c("A", "B", "C"))
text(kino.pos$lon, kino.pos$lat, labels = kino.pos$name, cex=3, col="darkred")

dev.off()



# MAP 2 - Survey area with all GOV and MIK stations that we have taken plotted on map. 
png("../Kart/IBTS Q1 2019 GOV and MIK.png", width=1000, height = 1000)
icesmap(xMin=-3,xMax=10,yMin=56,yMax=62) #norwegian area 2018

# ADD SQUARE NAMES
square.names <- read.csv2("~/ownCloud/Research/iBTS/R IBTS/NOR IBTS SQUARES.csv")
colnames(square.names) <- c("sq", "lat", "lon")

text(square.names$lon, square.names$lat, labels = square.names$sq, cex=1.5, col = "gray50")

# Add positions of GOVs
bt<-subset(logg.station, sttype=="BT Start")
points(bt$lon, bt$lat, pch=15, col="blue3", cex=2)
mik<-subset(logg.station, sttype=="MIK Start")
points(mik$lon, mik$lat, pch=2, col="red3", cex=2)

leg.text <- c("MIK", "Bottom Trawl")
legend("topright", leg.text, pch = c(2,15), pt.bg = "white", col = c("red3", "blue3"), cex=2)

dev.off()


#plot MIK stations on ICES NS square map
MIK.stations <- read.csv2("../data/MIK/MIK_stations.csv")
#points(MIK.stations$Lon, MIK.stations$Lat)
text(MIK.stations$Lon, MIK.stations$Lat, labels=MIK.stations$station, cex=0.4)

#######
# Export data to IBTS coordinator during survey
#
# 1) Import data files exported from S2D
# must first import files in Excel with Latin Encoding, saving as .csv
# samples <- read.table("../data/S2D/catchsample.txt", header=TRUE, sep="\t", dec=".")
samples <- read.csv2("../data/S2D/catchsample.csv", header=TRUE, dec=".")
individual <- read.table("../data/S2D/individual.txt", header=TRUE, sep="\t", dec=".")
station <- read.table("../data/S2D/fishstation.txt", header=TRUE, sep="\t", dec=".")

# add serialno + sampleid column to be able to match samples and individuals
samples$ser.samp <- samples$f.serialnumber*100+samples$catchsampleid
individual$ser.samp <- individual$f.serialnumber*100+individual$c.catchsampleid

individual$c.species <-samples$catchcategory[match(individual$ser.samp, samples$ser.samp)]


#smpl <- tbl_df(samples)

species<-c("SILD'G05", "TORSK", "HYSE", "HVITTING", "ØYEPÅL", "BRISLING", "MAKRELL")

st.list<-station$serialnumber

#create a data-table to store information
catchtable <- data.frame(ship="GOS", serial=st.list[4:length(st.list)], rect="NN", herr=0, cod=0, had=0, whi=0, Npout=0, sprat=0, mack=0, MIK=0, MIKeyM=0)

#populate the catchtable with species specific data
for (i in 1:length(st.list)) {
  smp <- subset(samples, f.serialnumber==st.list[i])
  ind <- subset(individual, f.serialnumber==st.list[i])
  j <- i-3
  
  #currently assume 30min hauls, should be updated with the real trawl time based on the station data
  catchtable$herr[j] <- nrow(subset(subset(ind, c.species=="SILD'G05"), length<0.2)) * (sum(subset(smp, catchcategory=="SILD'G05")$catchcount) / nrow(subset(ind, c.species=="SILD'G05"))) *2
  
  catchtable$cod[j] <- nrow(subset(subset(ind, c.species=="TORSK"), length<0.25)) * (sum(subset(smp, catchcategory=="TORSK")$catchcount) / nrow(subset(ind, c.species=="TORSK"))) *2
  
  catchtable$had[j] <- nrow(subset(subset(ind, c.species=="HYSE"), length<0.20)) * (sum(subset(smp, catchcategory=="HYSE")$catchcount) / nrow(subset(ind, c.species=="HYSE"))) *2
  
  catchtable$whi[j] <- nrow(subset(subset(ind, c.species=="HVITTING"), length<0.20)) * (sum(subset(smp, catchcategory=="HVITTING")$catchcount) / nrow(subset(ind, c.species=="HVITTING"))) *2
  
  catchtable$Npout[j] <- nrow(subset(subset(ind, c.species=="ØYEPÅL"), length<0.15)) * (sum(subset(smp, catchcategory=="ØYEPÅL")$catchcount) / nrow(subset(ind, c.species=="ØYEPÅL"))) *2
  
  catchtable$sprat[j] <- nrow(subset(subset(ind, c.species=="BRISLING"), length<0.10)) * (sum(subset(smp, catchcategory=="BRISLING")$catchcount) / nrow(subset(ind, c.species=="BRISLING"))) *2
  
  catchtable$mack[j] <- nrow(subset(subset(ind, c.species=="MAKRELL"), length<0.25)) * (sum(subset(smp, catchcategory=="MAKRELL")$catchcount) / nrow(subset(ind, c.species=="MAKRELL"))) *2
  
  #catchtable$herr[j] <- herr
  #catchtable$cod[j] <- cod
  #catchtable$had[j] <- had
  #catchtable$whi[j] <- whi
  #catchtable$Npout[j] <- Npout
  #catchtable$sprat[j] <- sprat
  #catchtable$mack[j] <- mack
  
 }

write.csv2(catchtable, "../analyses/catches/catchtable.csv")


###########
# Analysis of scanmar data

# Read scanmar files
scanmar.files <- list.files("../data/scanmar/")



for(i in 1:length(st.list)) {
  plotdata <- assign(paste("scanmar_", st.list[i], sep=""), read.delim2(paste("../data/scanmar/",scanmar.files[i], sep=""), sep=";"))
  plotdata$DTG <- strptime(paste(plotdata$Dag, plotdata$Tid), "%d.%m.%Y %H:%M:%S")

    # Trawl OPENING plot
  topen.plot <- ggplot(plotdata, aes(as.POSIXct(DTG), TEY.1.H)) + geom_point(size=0.3) + geom_hline(yintercept = 3.8, col="blue", linetype=2) + geom_hline(yintercept = 5.1, col="blue", linetype=2) + geom_hline(yintercept = 3, col="green3", linetype=1) + geom_hline(yintercept = 3.5, col="green3", linetype=1) +theme_classic()  + labs(x="time", y="Vertical opening (m)") + coord_cartesian( ylim = c(2,10), expand = TRUE)   + ggtitle(st.list[i])
  
  ggsave(paste("../analyses/calibration_trawl/opening", st.list[i], ".png", sep=""))
  
  # Door spread plot
  dspread.plot <- ggplot(plotdata, aes(as.POSIXct(DTG), DVTLAM.1.S)) + geom_point(size=0.3) + geom_hline(yintercept = 80, col="blue", linetype=2) + geom_hline(yintercept = 100, col="blue", linetype=2) + geom_hline(yintercept = 100, col="green3", linetype=1) + geom_hline(yintercept = 110, col="green3", linetype=1) +theme_classic()  + labs(x="time", y="Door spread (m)") + coord_cartesian( ylim = c(80,120), expand = TRUE)   + ggtitle(st.list[i])
  
  ggsave(paste("../analyses/calibration_trawl/door_spread", st.list[i], ".png", sep=""))
  
  #Door spread + Wing spread plot
  #plotdata$DS <- plotdata$TSP.1.X+50
 # plotdata$DS2 <- plotdata$TSP.1.Y+50
  dwspread.plot <- ggplot(plotdata, aes(as.POSIXct(DTG), DVTLAM.1.S)) + geom_hline(yintercept = 80, col="blue", linetype=2) + geom_hline(yintercept = 100, col="blue", linetype=2) + geom_hline(yintercept = 100, col="green3", linetype=1) + geom_hline(yintercept = 110, col="green3", linetype=1) +theme_classic()  + labs(x="time", y="Wing spread (m)") + coord_cartesian( ylim = c(10,120), expand = TRUE)  + geom_point(size=0.3) + geom_point(aes(x=as.POSIXct(DTG), y=CVTLAM.1.), data=plotdata, col="RED", size=0.3)  + ggtitle(st.list[i])
  
  ggsave(paste("../analyses/calibration_trawl/wing_door_spread", st.list[i], ".png", sep=""))
}


#combine all SCANMAR data into three variables, 'doors','wings' and 'vopen'
doors <- data.frame( opening=double(),station=integer(), DTG=integer(), No=integer())
wings <- data.frame(station=integer(), opening_x=double(), opening_y=double(), DTG=integer(), No=integer())
vopen <- data.frame( opening=double(),station=integer(), DTG=integer(), No=integer())

for(i in 1:length(st.list)) {
  plotdata <- assign(paste("scanmar_", st.list[i], sep=""), read.delim2(paste("../data/scanmar/",scanmar.files[i], sep=""), sep=";"))
  plotdata$DTG <- strptime(paste(plotdata$Dag, plotdata$Tid), "%d.%m.%Y %H:%M:%S")
  
  d <- as.data.frame(plotdata$DVTLAM.1.S)
  d$station <- st.list[i]
  d$DTG<-plotdata$DTG
  d$No <- c(1:nrow(d))
  colnames(d) <- c("opening", "station", "DTG", "No")
  doors <- rbind(doors, d)
  
  o <-  as.data.frame(cbind(plotdata$TSP.1.X, plotdata$CVTLAM.1.))
  o$station <- st.list[i]
  o$DTG<-plotdata$DTG
  o$No <- c(1:nrow(o))
  colnames(o) <- c("opening_x", "opening_y", "station", "DTG", "No")
  wings <- rbind(wings, o)
  
  v <- as.data.frame(plotdata$TEY.1.H)
  v$station <- st.list[i]
  v$DTG<-plotdata$DTG
  v$No <- c(1:nrow(v))
  colnames(v) <- c("opening", "station", "DTG", "No")
  vopen <- rbind(vopen, v)
  
  colnames(doors) <- c("opening", "station", "DTG", "No")
  colnames(vopen) <- c("opening", "station", "DTG", "No")
  colnames(wings) <- c("opening_x", "opening_y", "station", "DTG", "No")
  
  #print(c("O",st.list[i],dim(vopen)))
  #print(c("V",st.list[i],dim(wings)))
}


#plot all door spread
ds.plot <- ggplot(doors, aes(x=No, y=opening), group=station) + geom_point(aes( colour = factor(station)), size=0.1) + coord_cartesian(ylim=c(75, 135)) +ggtitle("Door spread through haul") + xlab("time (5-sec intervals)") + ylab("Door spread (m)")
ds.plot

ggsave("../analyses/calibration_trawl/door_spread_all_stations.png")

#plot all wing spread
#wings$maxo <- apply(wings[1:2], 1, max) 
ws.plot <- ggplot(wings, aes(x=No, y=opening_y), group=station) + geom_point(aes( colour = factor(station)), size=0.1) + coord_cartesian(ylim=c(15, 25)) +ggtitle("Wings spread through haul") + xlab("time (5-sec intervals)") + ylab("wing spread (m)")
ws.plot

ggsave("../analyses/calibration_trawl/wing_spread_all_stations.png")

#plot all vertical openings
vo.plot <- ggplot(vopen, aes(x=No, y=opening), group=station) + geom_point(aes( colour = factor(station)), size=0.1) + coord_cartesian(ylim=c(1.5, 7)) +ggtitle("Vertical trawl opening through haul") + xlab("time (5-sec intervals)") + ylab("Vertical opening (m)")
vo.plot

ggsave("../analyses/calibration_trawl/vertical_opening_all_stations.png")



#### NOT FULLY FUNCTIONAL
    # trawl opening vs. water speed - NOT FULLY FUNCTIONAL
    twaterspeed.plot <- ggplot(plotdata, aes(TSP.1.X, TEY.1.O)) +theme_classic()  + labs(y="Trawl opening", x="Trawl speed in water") + geom_point(size=0.3) + coord_cartesian( xlim = c(0,4), ylim=c(2,5), expand = TRUE)
    twaterspeed.plot 
    
    # check correlation between Trawl speed and Opening
    to<-as.data.frame(na.omit(cbind(plotdata$TSP.1.X, plotdata$TEY.1.O)))
    colnames(to) <- c("Speed", "Opening")
    to<-subset(to, Speed<5)
    cor(to$Speed, to$Opening, method="pearson")


#### Q-Q plots of door-spread, wing-spread and vertical opening

# quantile plots
#door opening
dd <- as.data.frame(na.omit(doors))
ww <- as.data.frame(na.omit(wings[,c(2:5)]))
#ww$maxo <- apply(ww[1:2], 1, max) 

quant.doors <- ggplot(subset(subset(dd, opening>50), opening<120)) + stat_qq(aes(sample = opening, colour = factor(station)), size=0.3)  + labs(colour = "GOV Station") + scale_colour_hue("clarity")  + ggtitle("Door spread")
quant.doors

ggsave("../analyses/calibration_trawl/door_spread_qq.png")

#faceted qqlotr door opening 
dd1 <- subset(subset(dd, opening>50), opening<120)
gg <- ggplot(data = dd1, mapping = aes(sample = opening,  fill = factor(station))) + stat_qq_band() + stat_qq_line() + stat_qq_point(size=0.2) + facet_wrap(~ station, scales="free") +  labs(x = "Theoretical Quantiles", y = "Door spread (observed)") + ggtitle("Door Opening , Q-Q plots")
gg

ggsave("../analyses/calibration_trawl/door_spread_facet_qq.png")

#faceted qqlotr door opening  - calibration stations
dd2 <- rbind(subset(dd1, station==60001), subset(dd1, station==60002), subset(dd1, station==60003), subset(dd1, station==60004))
gg <- ggplot(data = dd2, mapping = aes(sample = opening,  fill = factor(station))) + stat_qq_band() + stat_qq_line() + stat_qq_point(size=0.2) + facet_wrap(~ station, scales="free") +  labs(x = "Theoretical Quantiles", y = "Door spread (observed)") + ggtitle("Door Spread - Calibration tows, Q-Q plots")
gg

ggsave("../analyses/calibration_trawl/calibration_door_spread_facet_qq.png")

# all wing-spread data in one plot
pp <- ggplot(data = dd1, mapping = aes(sample = opening)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") +ggtitle("Door spread")
pp

ggsave("../analyses/calibration_trawl/door_spread_all_qq.png")

### wing spread
p <- ggplot(ww, aes(sample = opening_y))
p + stat_qq()

quant.wings <- ggplot(ww) + stat_qq(aes(sample = opening_y, colour = factor(station)), size=0.3) +  scale_colour_hue("clarity")  + labs(colour = "GOV Station") + ggtitle("Wing spread") + coord_cartesian( ylim = c(15,30), expand = TRUE)
quant.wings
ggsave("../analyses/calibration_trawl/wing_spread_qq.png")

#faceted qqlotr wing spread
gg <- ggplot(data = ww, mapping = aes(sample = opening_y,  fill = factor(station))) + stat_qq_band() + stat_qq_line() + stat_qq_point(size=0.2) + facet_wrap(~ station, scales="free") +  labs(x = "Theoretical Quantiles", y = "Wing spread (observed)") + ggtitle("Wing Spread, Q-Q plots")
gg

ggsave("../analyses/calibration_trawl/wing_spread_facet_qq.png")

# all wing-spread data in one plot
pp <- ggplot(data = ww, mapping = aes(sample = opening_y)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Wing spread")
pp

ggsave("../analyses/calibration_trawl/wing_spread_all_qq.png")



#vertical opening Q-Q plot
vo <- as.data.frame(na.omit(vopen))
vo <- subset(vo, opening>2)
vo <- subset(vo, opening<6)
qq_vopen.plot <- ggplot(vo) + stat_qq(aes(sample = opening, colour = factor(station)), size=0.3) +  scale_colour_hue("clarity")  + labs(colour = "GOV Station") + ggtitle("Vertical trawl opening") + coord_cartesian( ylim = c(2,7), expand = TRUE)
qq_vopen.plot
ggsave("../analyses/calibration_trawl/vertical_opening_qq.png")

#faceted qqlotr vertical opening
qq_vopen.plot <- ggplot(data = vo, mapping = aes(sample = opening,  fill = factor(station))) + stat_qq_band() + stat_qq_line() + stat_qq_point(size=0.2) + facet_wrap(~ station, scales="free") +  labs(x = "Theoretical Quantiles", y = "Vertical opening (observed)") + ggtitle("Vertical trawl opening, Q-Q plots")
qq_vopen.plot

ggsave("../analyses/calibration_trawl/vertical_trawl_opening_facet_qq.png")

#faceted qqlotr vertical opening - calibration tows
vo2 <- rbind(subset(vo, station==60001), subset(vo, station==60002), subset(vo, station==60003), subset(vo, station==60004))
qq_vopen.plot <- ggplot(data = vo2, mapping = aes(sample = opening,  fill = factor(station))) + stat_qq_band() + stat_qq_line() + stat_qq_point(size=0.2) + facet_wrap(~ station, scales="free") +  labs(x = "Theoretical Quantiles", y = "Vertical opening (observed)") + ggtitle("Vertical trawl opening - Calibration tows, Q-Q plots")
qq_vopen.plot

ggsave("../analyses/calibration_trawl/calibration_vertical_trawl_opening_facet_qq.png")

# all vertical opening data in one plot
pp <- ggplot(data = vo, mapping = aes(sample = opening)) +
  stat_qq_band() +
  stat_qq_line() +
  stat_qq_point() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles") + ggtitle("Vertical opening")
pp

ggsave("../analyses/calibration_trawl/vertical_opening_all_qq.png")
