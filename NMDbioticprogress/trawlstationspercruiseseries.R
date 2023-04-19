# This script generates an overview of stations in NMDbiotic with snapshots

# set up environment
library(RstoxData)
library(XML)
library(httr)
library(jsonlite)
library(utils)
# Set parameters
setwd('C:/repos/cruisetools/NMDbioticprogress')
base <- "http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v3/"

year0 = 2007
data = NULL
# Loop over mission types
missions = GET(URLencode("http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v3?type=ListAll"))
datasets <- xmlToList(xmlParse(rawToChar(missions$content)))

pb = txtProgressBar(min = 0, max = length(datasets), initial = 0) 

# Loop over datasets
i<-0

for (dataset in datasets){
  i<-i+1
  setTxtProgressBar(pb,i)
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
    # Download biotic file
    out <- tryCatch({
      # download the NMDbiotic file
      download.file(URLencode(URL_sn),destfile='biotic_temp.xml',quiet = T)
      # Read the NMDbiotic file
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
      },
      error=function(cond) {
        message(paste("URL does not seem to exist:", URL_sn))
        message("Here's the original error message:")
        message(cond)
        # Choose a return value in case of error
        return(NULL)
      },
      warning=function(cond) {
        message(paste("URL caused a warning:", URL_sn))
        message("Here's the original warning message:")
        message(cond)
        # Choose a return value in case of warning
        return(NULL)
      }
      )
    }
}
# Write data frame to disk
write.table(data, file = 'NMDbiotic.csv', sep = ";", row.names = F, fileEncoding = "UTF-8")

# 
# [1] "http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v3/Forskningsfart??y - fjerne farvann/2022/Dr.Fridtjof Nansen_LDLG/2022410/snapshot/2022-12-13T23.08.53.517Z"
# [1] "http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v3/Forskningsfart??y - fjerne farvann/2022/Dr.Fridtjof Nansen_LDLG/2022411/snapshot/2023-01-13T23.01.25.606Z"
#http://tomcat7.imr.no:8080/apis/nmdapi/biotic/v3/Forskningsfart??y - fjerne farvann/2023/Dr.Fridtjof Nansen_LDLG/2023004001/snapshot/
#https://datasetexplorer.hi.no/apps/datasetexplorer/v2/navigation/Other/Forskningsfart%C3%B8y%20-%20fjerne%20farvann/2023/Dr.Fridtjof%20Nansen_LDLG/2023004001