library(XML)
#
#
# Extracts stations with sponges from historical data and exports as openCPN files.
# Used in winter survey 2019 to determined reduced towing times in sponge rich areas.
# Tested with OpenCPN 4.8.8
#
# Adapted afterward to example data extrated with the code at:
# https://github.com/Sea2Data/FDAtools/tree/723534cdee8d863470b6a1358a4e7d428386bde7/dataAcess/makeBioticDataSet
#

# example data: winter survey 2013 - 2018
winter <- readRDS("wintersurvey.rda")

#catches coded as non-represenative due to large volumes of sponges, rocks or mud.
q6hauls <- winter[winter$samplequality==6,]
q6hauls <- q6hauls[!duplicated(q6hauls[,c("startyear", "missiontype", "missionnumber", "platform", "serialnumber")]),]

#catches where weight of sponges in catch has been recorded and is more than 200 kg
svampfangst <- winter[winter$catchcategory=="46861",]
svampfangst <- svampfangst[svampfangst$catchweight>200,]


# Export waypoints for import to openCPN, with weight of sponge-catch annotated.
# Filtering for sponge content must be done before. This function merely annotates the present weights etc.
export_sponge_waypoints <- function(data, filename, symbol="Symbol-Question-Red", encoding="UTF-8"){
  t <- XML::xmlOutputDOM("gpx")
  for (i in 1:nrow(data)){
    t$addTag("wpt", attrs = c(lat=data[i, "latitudestart"], lon=data[i, "longitudestart"]), close=F)
    t$addTag("desc", paste(data[i, "stationcomment"], data[i, "cruise"], "#", data[i, "serialnumber"], "end Lat:", data[i,"latitudeend"], "end Lon:", data[i, "longitudeend"]))
    times <- data[i, "stationstarttime"]
    ss <- unlist(strsplit(data[i, "stationstartdate"], "/"))
    dates <- paste(ss[3], ss[2], ss[1], sep="-")
    t$addTag("time", paste(dates, "T", times, "L", sep=""))
    t$addTag("name", data[i, "catchweight"])
    t$addTag("cmt", "")
    t$addTag("sym", symbol)
    t$closeTag() #/wpt    
  }

  saveXML(t$value(), file=filename)

}

# Export waypoints for import to openCPN
export_waypoints <- function(data, filename, symbol="Symbol-Question-Red", encoding="UTF-8"){
  t <- XML::xmlOutputDOM("gpx")
  for (i in 1:nrow(data)){
    t$addTag("wpt", attrs = c(lat=data[i, "latitudestart"], lon=data[i, "longitudestart"]), close=F)
    t$addTag("desc", paste(data[i, "stationcomment"], data[i, "cruise"], "#", data[i, "serialnumber"], "end Lat:", data[i,"latitudeend"], "end Lon:", data[i, "longitudeend"]))
    times <- data[i, "stationstarttime"]
    ss <- unlist(strsplit(data[i, "stationstartdate"], "/"))
    dates <- paste(ss[3], ss[2], ss[1], sep="-")
    t$addTag("time", paste(dates, "T", times, "L", sep=""))
    t$addTag("name", "")
    t$addTag("cmt", "")
    t$addTag("sym", symbol)
    t$closeTag() #/wpt    
  }
  
  saveXML(t$value(), file=filename)
  
}

export_sponge_waypoints(svampfangst, "svamper.gpx")
export_waypoints(q6hauls, "q6.gpx")
