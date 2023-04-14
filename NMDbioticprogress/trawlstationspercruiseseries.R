# This script generates an overview of stations in NMDbiotic with snapshots

# set up environment
library(RstoxData)
library(XML)
library(httr)
library(jsonlite)

# Set parameters
setwd('C:/repos/cruisetools/NMDbioticprogress')
base <- "http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v3/"

#http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v3/Forskningsfart??y/2017/G.O.Sars_LMEL/2017150/snapshot?version=3.0

year0 = 2007
data = NULL
# Loop over mission types
missions = GET(URLencode("http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v3?type=ListAll"))
datasets <- xmlToList(xmlParse(rawToChar(missions$content)))

# Loop over datasets
for (dataset in datasets){
  # Get path to biotic data
  for (dataset_ in dataset){if (dataset_$.attrs=='path'){platformpath<-dataset_$text}}
  
  # List snapshots for this delivery
  snapshotURL <- paste0(base,platformpath,'/snapshot/')
  ss = GET(URLencode(snapshotURL))
  snapshots <- xmlToList(xmlParse(rawToChar(ss$content)))
  # Loop over snapshots
  for (snapshot in snapshots){
    # Get NMDbiotic snapshot URL and download data
    #/{missiontype}/{year}/{platform}/{delivery}/snapshot/{time}
    snapshottime <- snapshot$element$text
    URL_sn <- paste0(snapshotURL,snapshottime)
    print(URL_sn)
    # Download biotic file
    download.file(URLencode(URL_sn),destfile='biotic_temp.xml',quiet = T)
    
    # Read the biotic file
    biotic <- ReadBiotic('biotic_temp.xml')
    serialnumber <- biotic$biotic_temp.xml$fishstation$serialnumber
    if (length(serialnumber)>0){
    # Create data frame 
    datasub <- as.data.frame(serialnumber)
    datasub$snapshotpath = snapshottime
    #datasub$snapshottime = as.POSIXct(snapshots[j]$row$element$text,format = "%Y-%m-%dT%H.%M.%OSZ")
    datasub$platformpath = URL_sn
    #datasub$delivery = delivery
    #datasub$missiontype = missiontype
    #datasub$year = year
    # Coerce to main data frame
    data <- rbind.data.frame(data,datasub) 
    }
  }
}
# # Write data frame to disk
# head(data)
write.table(data, file = 'NMDbiotic.csv', sep = ";", row.names = F, fileEncoding = "UTF-8")
# ?write.table