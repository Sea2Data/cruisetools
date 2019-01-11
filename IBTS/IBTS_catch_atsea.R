#' @title R-script for analyzing IBTS survey data at sea
#' @author Erik Olsen
#' @date feb 2018




#' LIBRARIES AND DEPENDENCIES
#' ----------------------------
library("ggplot2")
library("RColorBrewer")
library("maps")
library("mapdata")
library("reshape2")
library("dplyr")
library("tidyr")

#' SETTING SYSTEM TO NORWEGIAN
Sys.setlocale("LC_CTYPE", "no_NO")

#' LOAD DATA
#' ----------------------------
setwd("~/ownCloud/Research/iBTS/IBTS Q1 2018")

samples <- read.csv2("./data/samples.csv", dec=".")
individual <- read.csv2("./data/individual.csv", dec=".")
station <- read.csv2("./data/station.csv", dec=".")

#' add georef data to individual, sample and prey tables
#' individual
lat<-c(1:length(individual$c.species))
lon<-c(1:length(individual$c.species))
individual<-cbind(individual, lat, lon)

for (i in 1:length(individual$lat)) 
{
  individual$lat[i]<- station$latitudestart[grep(individual[i,5], station$serialno)]
  individual$lon[i]<- station$longitudestart[grep(individual[i,5], station$serialno)]
}

#' Samples
lat<-c(1:length(samples$species))
lon<-c(1:length(samples$species))
samples<-cbind(samples, lat, lon)

for (i in 1:length(samples$lat)) 
{
  samples$lat[i]<- station$latitudestart[grep(samples[i,5], station$serialno)]
  samples$lon[i]<- station$longitudestart[grep(samples[i,5], station$serialno)]
}



#' DATA ANALYSIS AND PLOTTING
#' __________________________________

#' Length - weight plots pr species

individual.cm<-subset(individual, lengthunit==2)
individual.cm<-subset(individual.cm, weight!="NA")

LWplot<-ggplot(individual.cm, aes(x=log(length), y=log(weight), group=c.species)) +geom_point(size=2, shape="+", aes(colour=c.species)) + guides(colour=FALSE) + facet_wrap(~c.species) + theme_grey() 
LWplot

ggsave("./L_W plots/LW_plot_ibts_q1_2018.png")

#non LG-scale
LWplot<-ggplot(individual.cm, aes(x=length, y=weight, group=c.species)) +geom_point(size=2, shape="+", aes(colour=c.species)) + guides(colour=FALSE) + facet_wrap(~c.species, scales="free") + theme_grey() 
LWplot

ggsave("./L_W plots/LW_plot_CM_ibts_q1_2018.png")

#' Length - histograms
#' makes one for each species

lengde_hist_art<-by(individual$length, individual$c.species, hist) 

setwd("../length_hist/")
for (i in 1:nrow(summary(lengde_hist_art))){
  png(filename=paste(names(lengde_hist_art[i]), ".png", sep=""))
  plot(lengde_hist_art[[i]], main = names(lengde_hist_art[i]), xlab ="length bins")
  dev.off()
}



# alternative histogram plotting using ggplot
art<-c("TORSK","HYSE", "HVITTING", "ØYEPÅL", "SILD'G05", "SEI", "MAKRELL", "BRISLING")
ii <- individual.cm<-subset(individual, lengthunit>=2)
ii$c.species <- factor(ii$c.species)
iii <- subset(ii, subset = c.species %in% art)
iii$c.species <- factor(iii$c.species)

ggplot(data=ii, aes(x=length*100)) + geom_histogram(breaks=seq(0, 75, by=2), col="steelblue", fill="green", alpha = .2)  + labs(title="Histogram of length", x="Length", y="Count") + facet_wrap(~c.species, scales="free")

ggsave("./LengthHist/histogram_length_all_facets.png", width=12, height=9)

#ggplot(data=iii, aes(x=length*100)) + geom_histogram(breaks=seq(0, 75, by=2), col="steelblue", fill="green", alpha = .2)  + labs(title="Histogram of length", x="Length", y="Count") + facet_wrap(~c.species, scales="free", drop=FALSE)

#ggplot(iii, aes(x = length*100)) +
  #geom_histogram(bins=50,na.rm=TRUE) +
  #facet_wrap(~c.species, scales="free") +
  #theme_bw()



#' Plotting stations on map
#' 
northsea<- map_data("worldHires", c("Norway", "Sweden", "Denmark", "UK", "Holland", "Germany")) 

toktmap<-ggplot(northsea, aes(x=long, y=lat, group=group)) +  geom_polygon(colour="gray35", fill="gray85") +  coord_cartesian(xlim = c(-4, 13), ylim=c(50, 62)) + theme_bw()
toktmap + geom_point(data=station, shape="+", size=6,  colour= "violetred4", aes(x=longitudestart, y=latitudestart, group=gear))


ggsave("./maps/stationmap.png")


#' Plotting catches on map (circles)
#' 
art<-c("TORSK","HYSE", "HVITTING", "ØYEPÅL", "SILD'G05", "SEI", "MAKRELL", "BRISLING")
for (i in 1:length(art)){ 
  toktmap + geom_point(data=subset(samples, species==art[i]) , colour= "blue", aes(x=lon, y=lat, group=weight, size=weight)) + scale_size(name="Fangst (kg)", range=c(1,15), breaks=c(0,50,100,200, 1000)) + ggtitle(art[i])
  ggsave(paste("./Catchmaps/catchmap_", art[i],".png", sep=""))
}


#' Grouping and plotting total catch
#' using 'dplyr' package

samples.tbl<-tbl_df(samples)
samples.station<-group_by(samples.tbl, f.serialno)
tot.catch.station<-summarise(samples.station, catch.weight.kg=sum(weight), lat=max(lat), lon=max(lon))

p <- ggplot(tot.catch.station, aes(lon, lat))
#p<- p + geom_path(data=tot.catch.station,, colour="green4") 
p  +  geom_polygon(data =northsea, colour="gray35", fill="gray85", aes(x=long, y=lat, group=group)) +  coord_cartesian(xlim = c(-4, 13), ylim=c(50, 62)) + theme_bw() + geom_point(colour="slateblue", aes(size=catch.weight.kg))   + scale_size(name="Fangst (kg)", range=c(1,12), breaks=c(0,50,100,250, 500)) + ggtitle("Total fangst pr stasjon")
ggsave("./Catchmaps/total_catch.png")





