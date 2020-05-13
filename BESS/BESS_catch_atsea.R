#' @title R-script for analyzing IBTS survey  catch data at sea
#' @author Erik Olsen
#' @date feb 2019


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
setwd("~/ownCloud/Research/iBTS/IBTS Q1 2019/2019102/Cruistools_IBTS")

#' Import data files exported from S2D
#' must first open 'samples.txt' file in Excel with UTF8 Encoding, saving as .csv
samples <- read.csv2("../data/S2D/catchsample.csv", header=TRUE, dec=",")
# samples <- read.table("../data/S2D/catchsample.txt", header=TRUE, sep="\t",dec=".", fill=TRUE)
individual <- read.table("../data/S2D/individual.txt", header=TRUE, sep="\t", dec=".")
station <- read.table("../data/S2D/fishstation.txt", header=TRUE, sep="\t", dec=".")

#' add serialno + sampleid column to be able to match samples and individuals
samples$ser.samp <- samples$f.serialnumber*100+samples$catchsampleid
individual$ser.samp <- individual$f.serialnumber*100+individual$c.catchsampleid

individual$c.species <-samples$catchcategory[match(individual$ser.samp, samples$ser.samp)]

#' add georef data to individual & sample tables
individual$lat <- station$latitudestart[match(individual$f.serialnumber, station$serialnumber)]
individual$lon <- station$longitudestart[match(individual$f.serialnumber, station$serialnumber)]

samples$lat <- station$latitudestart[match(samples$f.serialnumber, station$serialnumber)]
samples$lon <- station$longitudestart[match(samples$f.serialnumber, station$serialnumber)]


#' Import MIK data
mik <- read.csv2("../data/MIK/MIK.csv", header=TRUE, dec=",")

#' DATA ANALYSIS AND PLOTTING
#' __________________________________

#' Length - weight plots pr species

individual.cm<-subset(individual, lengthresolution>=2 &  individualweight!="NA")

LWplot<-ggplot(individual.cm, aes(x=length*100, y=individualweight, group=c.species)) +geom_point(size=2, shape="+", aes(colour=c.species)) + guides(colour=FALSE) + facet_wrap(~c.species, scales="free") + theme_grey() + labs(x="Lengde (cm)", y="Vekt (kg)")
LWplot

ggsave("../analyses/Catch/L_W plots/LW_plot_ibts_q1_2018.png")


#' Length - histograms

art<-c("TORSK","HYSE", "HVITTING", "ØYEPÅL", "SILD'G05", "SEI", "MAKRELL", "BRISLING")
ii <-subset(individual, lengthresolution>=2)
ii$c.species <- factor(ii$c.species)
iii <- subset(ii, subset = c.species %in% art)
iii$c.species <- factor(iii$c.species)

l1 <- ggplot(data=ii, aes(x=length*100)) + geom_histogram(breaks=seq(0, 75, by=2), col="steelblue", fill="green", alpha = .2)  + labs(title="Histogram of length", x="Length", y="Count") + facet_wrap(~c.species, scales="free")
l1

ggsave("../analyses/Catch/length_hist/histogram_length_all_facets.png", width=12, height=9)



#' Plotting catches on map (circles)

#' Plotting stations on map
#' 
northsea<- map_data("worldHires", c("Norway", "Sweden", "Denmark", "UK", "Holland", "Germany")) 

toktmap<-ggplot(northsea, aes(x=long, y=lat, group=group)) +  geom_polygon(colour="gray35", fill="gray85")  + coord_quickmap(xlim = c(-4, 13), ylim=c(50, 62)) + theme_bw()

art<-c("TORSK","HYSE", "HVITTING", "ØYEPÅL", "SILD'G05", "SEI", "MAKRELL", "BRISLING")
for (i in 1:length(art)){ 
  toktmap + geom_point(data=subset(samples, catchcategory==art[i]) , colour= "lightskyblue3", aes(x=lon, y=lat, group=catchweight, size=catchweight)) + scale_size(name="Fangst (kg)", range=c(1,15), breaks=c(0,1, 5,10,50,100,200, 1000)) + ggtitle(art[i])
  ggsave(paste("../analyses/Catch/Catchmaps/catchmap_", art[i],".png", sep=""))
}


#' Grouping and plotting total catch
#' using 'dplyr' package

samples.tbl<-tbl_df(samples)
samples.station<-group_by(samples.tbl, f.serialnumber)
tot.catch.station<-summarise(samples.station, catch.weight.kg=sum(catchweight), lat=max(lat), lon=max(lon))

p <- ggplot(tot.catch.station, aes(lon, lat))
p  +  geom_polygon(data =northsea, colour="gray35", fill="gray85", aes(x=long, y=lat, group=group)) +  coord_quickmap(xlim = c(-4, 13), ylim=c(50, 62)) + theme_bw() + geom_point(colour="slateblue", aes(size=catch.weight.kg))   + scale_size(name="Fangst (kg)", range=c(1,6), breaks=c(0,10, 20, 50,100,150, 200,250,300, 400, 500)) + ggtitle("Total fangst pr stasjon")
ggsave("../analyses/Catch/Catchmaps/total_catch.png")


#' Plotting MIK stations with herring

m <- ggplot(mik, aes(lon, lat))
m  +  geom_polygon(data =northsea, colour="gray35", fill="gray85", aes(x=long, y=lat, group=group)) +  coord_quickmap(xlim = c(-4, 13), ylim=c(50, 62)) + theme_bw() + geom_point(colour="slateblue", aes(size=Clupea.harengus))   + scale_size(name="Antall larver", range=c(1,6), breaks=c(0,1, 2,3,4, 5,10,15, 20)) + ggtitle("MIK fangst av sildelarver")
ggsave("../analyses/Catch/Catchmaps/MIK_herring.png")

#' Plotting MIK stations with Crystallogobius linearis 

m <- ggplot(mik, aes(lon, lat))
m  +  geom_polygon(data =northsea, colour="gray35", fill="gray85", aes(x=long, y=lat, group=group)) +  coord_quickmap(xlim = c(-4, 13), ylim=c(50, 62)) + theme_bw() + geom_point(colour="slateblue", aes(size=Crystallogobius.linearis))   + scale_size(name="Antall larver", range=c(1,6), breaks=c(0,1, 2,3,4, 5,10,15, 20)) + ggtitle("MIK fangst av larver av krystallkutling")
ggsave("../analyses/Catch/Catchmaps/MIK_krystallkutling.png")

#' some simple statistics
#' sum catch weight
sum(samples$catchweight, na.rm=TRUE)

#' sum catch numbers
sum(samples$catchcount, na.rm=TRUE)

#' number of species
levels(samples$catchcategory)



#' CHECK OF DATRAS DATA
#' ---------------------

#' Import DATRAS data

d_stat <- read.csv2("../DATRAS export/datras_station.csv")
d_ind <- read.csv2("../DATRAS export/datras_individual.csv")
d_age <- read.csv2("../DATRAS export/datras_age.csv")

#' add georef data to individual & sample tables
d_ind$lat <- d_stat$HaulLat[match(d_ind$StNo, d_stat$StNo)]
d_ind$lon <- d_stat$HaulLong[match(d_ind$StNo, d_stat$StNo)]


#' LW plots


ggplot(subset(d_age, IndWgt>0), aes(x=LngtClass, y=IndWgt, group=SpecCode)) +geom_point(size=2, shape="+", aes(colour=SpecCode)) + guides(colour=FALSE) + facet_wrap(~SpecCode, scales="free") + theme_grey() + labs(x="Lengde (cm)", y="Vekt (g)")
ggsave("../DATRAS export/LW_datras.png")

#' plot stations
p2 <- ggplot(d_ind, aes(lon, lat))
p2  +  geom_polygon(data =northsea, colour="gray35", fill="gray85", aes(x=long, y=lat, group=group)) +  coord_quickmap(xlim = c(-4, 13), ylim=c(50, 62)) + theme_bw() + geom_point(colour="slateblue", aes(size=CatCatchWgt))   + scale_size(name="Fangst (kg)", range=c(1,6), breaks=c(0,10, 20, 50,100,150, 200,250,300, 400, 500)) + ggtitle("Total fangst pr stasjon")
