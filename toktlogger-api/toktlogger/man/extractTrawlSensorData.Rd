% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nmea.R
\name{extractTrawlSensorData}
\alias{extractTrawlSensorData}
\title{Parse SCANMAR sensor data}
\usage{
extractTrawlSensorData(nmeaMessages)
}
\arguments{
\item{nmeaMessages}{nmea messages as returned by \code{\link[toktlogger]{fetchNmeaAtActivity}}}
}
\value{
nmeaMessages with annotations. The column 'measurementValue' contains the value. The column 'measurment' contains explanation for the value.
}
\description{
Parse PSCMSM2 telegrams from SCANMAR trawls sensor data
trawleye(TEY), port trawldoor (DVTLAM) and starboard trawldoor (DVTLAS)
Consult SCANMAR documentation for interpetation of fields (Sensor Measurements NMEA protocol v 3.0)
}
\details{
sensor ids are not checked, relies on sensorTye info only.
}
