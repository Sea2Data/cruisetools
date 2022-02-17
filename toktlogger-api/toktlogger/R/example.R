
#' Plot calibration report
#' @examples
#'  testCalibrationReport(970)
#' @export
testCalibrationReport <- function(localStationNumber){
  nmea <- fetchNmeaAtActivity(localStationNumber)
  sensordata <- extractTrawlSensorData(nmea)
  plotReport(sensordata)
}


#' Plot trawl depth pelagic trawls (headline)
#' @examples
#'  testCalibrationReport(107)
#' @export
testTrawlDepthPlot <- function(localStationNumber){
  nmea <- fetchNmeaAtActivity(localStationNumber)
  sensordata <- extractTrawlSensorData(nmea)

  if (!("Trawl depth" %in% sensordata$measurment) | all(is.na(sensordata$measurementValue["Trawl depth" == sensordata$measurment]))){
    stop("Trawl depth measurements not found.")
  }

  pl<-plotDepthProfileTrawlDepth(sensordata)
  pl
}
