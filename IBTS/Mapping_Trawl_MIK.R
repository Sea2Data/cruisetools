#' Mapping IBTS Q1 progress (MIK and GOV Bottom trawl stations)
#' by: Erik Olsen
#' Feb. 2019



#' SETTING SYSTEM TO NORWEGIAN
Sys.setlocale("LC_CTYPE", "no_NO")

#' PATH & SOURCES
setwd("~/ownCloud/Research/iBTS/IBTS Q1 2019/2019102/Cruistools_IBTS")
#' BacgroundMap.R creates a map of the North Sea with the IBTS squares that the Norwegian vessel is to suvey highlighted
#' To change the highlighted squares, change 'lines' arguments in line 92 - line 113 in the source code
source("~/ownCloud/Research/iBTS/R IBTS/BackgroundMap.R")


#' IMPORT LOG FILES
logg.list <- list.files("../data/log/ACTIVITY/log/")
logg.station<-data.frame(lat=double(), lon=double(), stcode=integer())

for (i in 1: length(logg.list)){
  ld <- read.table(paste("../data/log/ACTIVITY/log/",logg.list[i], sep=""), sep=",")
  ld$lat <- as.numeric(substr(ld$V13, 1,2)) + as.numeric(substr(ld$V13, 3,9))/60
  ld$lon <- as.numeric(substr(ld$V14, 1,3)) + as.numeric(substr(ld$V14, 4,9))/60 * ifelse(substr(ld$V14,12,12)=="E", 1, -1)
  logg.station <- rbind(logg.station, cbind(ld$lat, ld$lon, ld$V5))
}
colnames(logg.station) <- c("lat", "lon", "stcode")

st.types <- data.frame(stcode=c(2800, 2801, 1000, 1001, 1600, 1601), sttype=c("MIK Start", "MIK Stop", "CTD Start", "CTD Stop", "BT Start", "BT Stop"))
logg.station$sttype <- st.types$sttype[match(logg.station$stcode, st.types$stcode)]

#' import track files
track.files <- list.files("../data/log/TRACK/")
track <- data.frame(lat=double(), lon=double())

for (i in 1: length(track.files)){
  ld <- read.table(paste("../data/log/TRACK/",track.files[i], sep=""), sep=",", fill=TRUE)
  ld$lat <- as.numeric(substr(ld$V13, 1,2)) + as.numeric(substr(ld$V13, 3,9))/60
  ld$lon <- as.numeric(substr(ld$V14, 1,3)) + as.numeric(substr(ld$V14, 4,9))/60 * ifelse(substr(ld$V14,12,12)=="E", 1, -1)
  track <- rbind(track, cbind(ld$lat, ld$lon))
}
colnames(track) <- c("lat", "lon")

#' CREATE MAPS
#' 
#' Map 1 - Survey area 2019 with all GOV and MIK stations that we have taken plotted on map. 
png("../Kart/IBTS Q1 2019 GOV and MIK.png", width=1000, height = 1000)
icesmap(xMin=-3,xMax=10,yMin=56,yMax=62) #norwegian area 2018

#' Add square names used in 2019 survey
square.names <- read.csv2("~/ownCloud/Research/iBTS/R IBTS/NOR IBTS SQUARES.csv")
colnames(square.names) <- c("sq", "lat", "lon")
text(square.names$lon, square.names$lat, labels = square.names$sq, cex=1.5, col = "gray50")

#' Add positions of GOVs
bt<-subset(logg.station, sttype=="BT Start")
points(bt$lon, bt$lat, pch=15, col="blue3", cex=2)

#' Add postion of MIKs
mik<-subset(logg.station, sttype=="MIK Start")
points(mik$lon, mik$lat, pch=2, col="red3", cex=2)

#' Add legend
leg.text <- c("MIK", "Bottom Trawl")
legend("topright", leg.text, pch = c(2,15), pt.bg = "white", col = c("red3", "blue3"), cex=2)

dev.off()


#' MAP - ship track
png("../Kart/IBTS Q1 2019 track.png", width=1000, height = 1000)
icesmap(xMin=-3,xMax=10,yMin=56,yMax=62) #norwegian area 2018

#' Add square names used in 2019 survey
square.names <- read.csv2("~/ownCloud/Research/iBTS/R IBTS/NOR IBTS SQUARES.csv")
colnames(square.names) <- c("sq", "lat", "lon")
text(square.names$lon, square.names$lat, labels = square.names$sq, cex=1.5, col = "gray50")

#' Add positions of track
points(track$lon, track$lat, pch=20, col="navy", cex=0.5)


dev.off()




#' MAP  - Survey area 2019 w KINO positions
png("../Kart/NorSquares w KINO positions.png", width=1000, height = 1000)
icesmap(xMin=-3,xMax=10,yMin=56,yMax=62) #norwegian area 2018

#' Add square names
square.names <- read.csv2("~/ownCloud/Research/iBTS/R IBTS/NOR IBTS SQUARES.csv")
colnames(square.names) <- c("sq", "lat", "lon")
text(square.names$lon, square.names$lat, labels = square.names$sq, cex=1.5, col = "gray50")

#' add positions for additional WP2 hauls for Mateos Riviera - KINO project
kino.pos <- data.frame(lat=c(56.45, 58.5, 61.2167), lon=c(3.2167, 1.833, 2), name=c("A", "B", "C"))
text(kino.pos$lon, kino.pos$lat, labels = kino.pos$name, cex=3, col="darkred")

dev.off()


