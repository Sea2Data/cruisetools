#' @noRd
parsePSCMSM2 <- function(sentences){
  messages <- data.table::as.data.table(data.table::tstrsplit(sentences, "[,*]", type.convert = T, fixed=F))
  names(messages) <- c("msgFormat", "sensortimestamp", "status", "sensorType", "sensorId", "measurementId", "measurementValue", "quality", "checksum")
  return(messages)
}

#' Parse SCANMAR sensor data
#' @description
#'  Parse PSCMSM2 telegrams from SCANMAR trawls sensor data
#'  trawleye(TEY), port trawldoor (DVTLAM) and starboard trawldoor (DVTLAS)
#'  Consult SCANMAR documentation for interpetation of fields (Sensor Measurements NMEA protocol v 3.0)
#' @details
#'  sensor ids are not checked, relies on sensorTye info only.
#' @param nmeaMessages nmea messages as returned by \code{\link[toktlogger]{fetchNmeaAtActivity}}
#' @return nmeaMessages with annotations. The column 'measurementValue' contains the value. The column 'measurment' contains explanation for the value.
#' @export
extractTrawlSensorData <- function(nmeaMessages){

  sensorMessages <- nmeaMessages[nmeaMessages$identifier=="PSCMSM2",]
  sensorMessages <- cbind(sensorMessages, parsePSCMSM2(sensorMessages$sentence))
  sensorMessages$timestamp <-  getPosixFromToktLogger(sensorMessages$timestamp)

  sensorMessages$measurment <- as.character(NA)
  sensorMessages$measurment[sensorMessages$sensorType=="TEY" & sensorMessages$measurementId=="H"] <- "Trawl height"
  sensorMessages$measurment[sensorMessages$sensorType=="TEY" & sensorMessages$measurementId=="O"] <- "Trawl v. opening"
  sensorMessages$measurment[sensorMessages$sensorType=="TEY" & sensorMessages$measurementId=="C"] <- "Trawl clearance"
  sensorMessages$measurment[sensorMessages$sensorType=="TEY" & sensorMessages$measurementId=="F"] <- "Fish density"
  sensorMessages$measurment[sensorMessages$sensorType=="TSP" & sensorMessages$measurementId=="X"] <- "Flow (across)"
  sensorMessages$measurment[sensorMessages$sensorType=="TSP" & sensorMessages$measurementId=="Y"] <- "Flow (along)"
  sensorMessages$measurment[sensorMessages$sensorType=="DVTLAM" & sensorMessages$measurementId=="S"] <- "Door spread"
  sensorMessages$measurment[sensorMessages$sensorType=="DVTLAM" & sensorMessages$measurementId=="R"] <- "Roll (port door)"
  sensorMessages$measurment[sensorMessages$sensorType=="DVTLAM" & sensorMessages$measurementId=="P"] <- "Pitch (port door)"
  sensorMessages$measurment[sensorMessages$sensorType=="DVTLAM" & sensorMessages$measurementId=="A"] <- "Roll Rattle factor (port door)"
  sensorMessages$measurment[sensorMessages$sensorType=="DVTLAM" & sensorMessages$measurementId=="B"] <- "Pitch Rattle factor (port door)"
  sensorMessages$measurment[sensorMessages$sensorType=="DVTLAM" & sensorMessages$measurementId=="T"] <- "Temperature (port door)"
  sensorMessages$measurment[sensorMessages$sensorType=="DVTLAM" & sensorMessages$measurementId=="D"] <- "Depth (port door)"
  sensorMessages$measurment[sensorMessages$sensorType=="DVTLAM" & sensorMessages$measurementId=="N"] <- "Tension (port door)"
  sensorMessages$measurment[sensorMessages$sensorType=="DVTLAS" & sensorMessages$measurementId=="R"] <- "Roll (starboard door)"
  sensorMessages$measurment[sensorMessages$sensorType=="DVTLAS" & sensorMessages$measurementId=="P"] <- "Pitch (starboard door)"
  sensorMessages$measurment[sensorMessages$sensorType=="DVTLAS" & sensorMessages$measurementId=="A"] <- "Roll Rattle factor (starboard door)"
  sensorMessages$measurment[sensorMessages$sensorType=="DVTLAS" & sensorMessages$measurementId=="B"] <- "Pitch Rattle factor (starboard door)"
  sensorMessages$measurment[sensorMessages$sensorType=="DVTLAS" & sensorMessages$measurementId=="T"] <- "Temperature (starboard door)"
  sensorMessages$measurment[sensorMessages$sensorType=="DVTLAS" & sensorMessages$measurementId=="D"] <- "Depth (starboard door)"
  sensorMessages$measurment[sensorMessages$sensorType=="DVTLAS" & sensorMessages$measurementId=="N"] <- "Tension (starboard door)"

  return(sensorMessages)
}



