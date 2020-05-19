# This script generates an overview of stations in NMDbiotic with snapshots

# set up environment
library(Rstox)
library(XML)
library(httr)
library(jsonlite)

# Set parameters
setwd('D:/repos/Github/cruisetools/NMDbioticprogress')
base <- "http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v3"

year0 = 2007

# List *all* missions @ NMD
CS <- getNMDinfo("c", recursive=FALSE)
# Remove any data newer than 2015 (since they are using NMDbiotic as the primary system
C <- CS[CS$startyear<2015,]
# Reduse data for testing...
#C <- C[C$startyear==year0,]
C <- as.data.frame(C)

#C[i,4]$missiontype,C[i]$startyear,C[i]$platform)
I <- dim(C)
data <- NULL

for (i in seq(1,I[1])){
#for (i in seq(1,2)){
  print(paste("Mission ",i,' of ',I[1]))
  # Get "platformpath" (add call signal if it exist)
  if(is.na(C[i,17])){platformpath <- paste0(C[i,7])}
  else {platformpath <- paste0(C[i,7],"_",C[i,17])}
  
  # Get "delivery" (check if the mission has cruise number or not)
  if(is.na(C[i,18])){delivery <- C[i,3]}
  else {delivery <- C[i,18]}
  
  # Get "missiontype"
  missiontype <- C[i,5]
  
  # Get "year"
  year <- C[i,11]
  
  # Get the list of snapshots from the API
  #/{missiontype}/{year}/{platform}/{delivery}/snapshot
  snsht <- paste0(base,"/",missiontype,"/",year,"/",platformpath,"/",delivery,"/snapshot?version=3.0")
  snapshots_raw<- xmlParse(URLencode(snsht))
  snapshots <- xmlToList(snapshots_raw)
  
  # Loop over snapshots
  for (j in seq(1,length(snapshots))){ 
    # Get NMDbiotic snapshot URL and download data
    #/{missiontype}/{year}/{platform}/{delivery}/snapshot/{time}    
    snapshottime <- snapshots[j]$row$element$text
    biotic_url <- URLencode(paste0(base,"/",missiontype,"/",year,"/",platformpath,"/",delivery,"/snapshot/",snapshottime,"?version=3.0"))
    # Download biotic file
    download.file(biotic_url,destfile='biotic_temp.xml',quiet = T)
    # Read the biotic file
    biotic <- readXMLfiles('biotic_temp.xml')
    
    # check if the data is NULL
    fs_dim <- dim(biotic$ReadBioticXML_BioticData_fishstation.txt)
    if (!fs_dim[1]==0){
      # Create data frame 
      serialnumber <- (biotic$ReadBioticXML_BioticData_fishstation.txt$serialnumber)
      datasub <- as.data.frame(serialnumber)
      datasub$snapshotpath = snapshottime
      #datasub$snapshottime = as.POSIXct(snapshots[j]$row$element$text,format = "%Y-%m-%dT%H.%M.%OSZ")
      datasub$platformpath = platformpath
      datasub$delivery = delivery
      datasub$missiontype = missiontype
      datasub$year = year
      # Coerce to main data frame
      data <- rbind.data.frame(data,datasub)
    }
  }
}
# Write data frame to disk
head(data)
write.table(data, file = 'NMDbiotic.csv', sep = ";", row.names = F, fileEncoding = "UTF-8")
?write.table
