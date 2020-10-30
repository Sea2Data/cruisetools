
#' Plot calibration report
#' @examples
#'  testCalibrationReport(970)
#' @export
testCalibrationReport <- function(localStationNumber){
  nmea <- fetchNmeaAtActivity(localStationNumber)
  sensordata <- extractTrawlSensorData(nmea)
  plotReport(sensordata)
}



