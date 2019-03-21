# Data Export During IBTS Q1 Survey
# By: Erik Olsen
# v. Feb 2019


#' SETTING SYSTEM TO NORWEGIAN
Sys.setlocale("LC_CTYPE", "no_NO")


#' SOURCES
setwd("~/ownCloud/Research/iBTS/IBTS Q1 2019/2019102/Cruistools_IBTS")


#' LIBRARIES


#' IMPORT DATA FILES
#' Import data files exported from S2D
#' must first open 'samples.txt' file in Excel with Latin Encoding, saving as .csv
samples <- read.csv2("../data/S2D/catchsample.csv", header=TRUE, dec=",")
individual <- read.table("../data/S2D/individual.txt", header=TRUE, sep="\t", dec=".")
station <- read.table("../data/S2D/fishstation.txt", header=TRUE, sep="\t", dec=".")

#' add serialno + sampleid column to be able to match samples and individuals
samples$ser.samp <- samples$f.serialnumber*100+samples$catchsampleid
individual$ser.samp <- individual$f.serialnumber*100+individual$c.catchsampleid

individual$c.species <-samples$catchcategory[match(individual$ser.samp, samples$ser.samp)]

species<-c("SILD'G05", "TORSK", "HYSE", "HVITTING", "ØYEPÅL", "BRISLING", "MAKRELL")

st.list<-station$serialnumber


#' DATA TABLE FOR EXPORT
#' create a data-table to store information
catchtable <- data.frame(ship="GOS", serial=st.list[4:length(st.list)], rect="NN", herr=0, cod=0, had=0, whi=0, Npout=0, sprat=0, mack=0, MIK=0, MIKeyM=0)

#' populate the catchtable with species specific data
#' assumes 30min hauls, should be updated with the real trawl time based on the station data
for (i in 1:length(st.list)) {
  smp <- subset(samples, f.serialnumber==st.list[i])
  ind <- subset(individual, f.serialnumber==st.list[i])
  j <- i-3

  catchtable$herr[j] <- nrow(subset(subset(ind, c.species=="SILD'G05"), length<0.2)) * (sum(subset(smp, catchcategory=="SILD'G05")$catchcount) / nrow(subset(ind, c.species=="SILD'G05"))) *2
  
  catchtable$cod[j] <- nrow(subset(subset(ind, c.species=="TORSK"), length<0.25)) * (sum(subset(smp, catchcategory=="TORSK")$catchcount) / nrow(subset(ind, c.species=="TORSK"))) *2
  
  catchtable$had[j] <- nrow(subset(subset(ind, c.species=="HYSE"), length<0.20)) * (sum(subset(smp, catchcategory=="HYSE")$catchcount) / nrow(subset(ind, c.species=="HYSE"))) *2
  
  catchtable$whi[j] <- nrow(subset(subset(ind, c.species=="HVITTING"), length<0.20)) * (sum(subset(smp, catchcategory=="HVITTING")$catchcount) / nrow(subset(ind, c.species=="HVITTING"))) *2
  
  catchtable$Npout[j] <- nrow(subset(subset(ind, c.species=="ØYEPÅL"), length<0.15)) * (sum(subset(smp, catchcategory=="ØYEPÅL")$catchcount) / nrow(subset(ind, c.species=="ØYEPÅL"))) *2
  
  catchtable$sprat[j] <- nrow(subset(subset(ind, c.species=="BRISLING"), length<0.10)) * (sum(subset(smp, catchcategory=="BRISLING")$catchcount) / nrow(subset(ind, c.species=="BRISLING"))) *2
  
  catchtable$mack[j] <- nrow(subset(subset(ind, c.species=="MAKRELL"), length<0.25)) * (sum(subset(smp, catchcategory=="MAKRELL")$catchcount) / nrow(subset(ind, c.species=="MAKRELL"))) *2
  
 }

#' Export data table that can then be copied to online Excel sheet in the Cloud
write.csv2(catchtable, "../analyses/catches/catchtable.csv")

