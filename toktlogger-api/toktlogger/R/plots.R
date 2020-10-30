#' Plots door spread
#' @param nmeaMessages nmea message for station, see \code{\link[toktlogger]{extractTrawlSensorData}}
#' @param tolerance upper and lower limit tolerated for door spread
#' @param ylim y-axis (door spread) range of plot
#' @return ggplot2 object
#' @export
plotDoorSpread <- function(nmeaMessages, tolerance=c(48,52), ylim=c(35,65)){
  ds <- nmeaMessages[nmeaMessages$measurment == "Door spread",]
  ds$timestamp <- as.numeric(difftime(ds$timestamp, min(ds$timestamp), units = "m"))
  pl <- ggplot2::ggplot(ds, ggplot2::aes(x=timestamp, y=measurementValue)) +
    ggplot2::geom_line() +
    ggplot2::xlab("time (min)") +
    ggplot2::ylab("door spread (m)") +
    ggplot2::geom_hline(yintercept = min(tolerance), color="red") +
    ggplot2::geom_hline(yintercept = max(tolerance), color="red") +
    ggplot2::ylim(ylim) +
    ggplot2::ggtitle(paste("Door spread (", median(ds$measurementValue), " m)", sep=""))

  return(pl)
}

#' Plots trawl opening
#' @param nmeaMessages nmea message for station, see \code{\link[toktlogger]{extractTrawlSensorData}}
#' @param tolerance upper and lower limit tolerated for trawl opening
#' @param ylim y-axis (trawl opening) range of plot
#' @return ggplot2 object
#' @export
plotTrawlOpening <- function(nmeaMessages, tolerance=c(3.5,4.5), ylim=c(2.5,5.5)){
  ds <- nmeaMessages[nmeaMessages$measurment == "Trawl v. opening",]
  ds$timestamp <- as.numeric(difftime(ds$timestamp, min(ds$timestamp), units = "m"))
  pl <- ggplot2::ggplot(ds, ggplot2::aes(x=timestamp, y=measurementValue)) +
    ggplot2::geom_line() +
    ggplot2::xlab("time (min)") +
    ggplot2::ylab("trawl opening (m)") +
    ggplot2::geom_hline(yintercept = min(tolerance), color="red") +
    ggplot2::geom_hline(yintercept = max(tolerance), color="red") +
    ggplot2::ylim(ylim) +
    ggplot2::ggtitle(paste("Trawl opening (", median(ds$measurementValue), " m)", sep=""))

  return(pl)
}

#' Plots trawl clearance
#' @param nmeaMessages nmea message for station, see \code{\link[toktlogger]{extractTrawlSensorData}}
#' @param tolerance upper and lower limit tolerated for trawl opening
#' @param ylim y-axis (trawl clearance) range of plot
#' @return ggplot2 object
#' @export
plotTrawlClearance <- function(nmeaMessages, ylim=c(-.01,4)){
  ds <- nmeaMessages[nmeaMessages$measurment == "Trawl clearance",]
  ds$timestamp <- as.numeric(difftime(ds$timestamp, min(ds$timestamp), units = "m"))
  ds <- ds[order(ds$timestamp, decreasing = F),]
  ds$interim <- ds$timestamp - c(0, ds$timestamp[1:(nrow(ds)-1)])
  pl <- ggplot2::ggplot(ds, ggplot2::aes(x=timestamp, y=measurementValue)) +
    ggplot2::geom_point() +
    ggplot2::xlab("time (min)") +
    ggplot2::ylab("trawl clearance (m)") +
    ggplot2::ylim(ylim) +
    ggplot2::ggtitle(paste("Trawl clearance (", format(sum(ds$interim[ds$measurementValue==0])*100/sum(ds$interim), digits=2), "% contact)", sep=""))

  return(pl)
}

#' Plots depth profile
#' @description
#'  Plots depth profile from trawl door sensors
#' @param nmeaMessages nmea message for station, see \code{\link[toktlogger]{extractTrawlSensorData}}
#' @return ggplot2 object
#' @export
plotDepthProfile <- function(nmeaMessages){
  ds <- nmeaMessages[nmeaMessages$measurment == "Depth (port door)" | nmeaMessages$measurment == "Depth (starboard door)",]
  ds$door <- NA
  ds$door[ds$measurment=="Depth (starboard door)"] <- "Starb."
  ds$door[ds$measurment=="Depth (port door)"] <- "Port"
  ds$timestamp <- as.numeric(difftime(ds$timestamp, min(ds$timestamp), units = "m"))

  pl <- ggplot2::ggplot(ds, ggplot2::aes(x=timestamp, y=measurementValue)) +
    ggplot2::geom_line(ggplot2::aes(col=door)) +
    ggplot2::xlab("time (min)") +
    ggplot2::ylab("door depth (m)") +
    ggplot2::ggtitle(paste("Door depth (", median(ds$measurementValue), "m)", sep="")) +
    ggplot2::theme(legend.position="bottom")

}

#' Plots trawl calibration report
#' @param nmeaMessages nmea message for station, see \code{\link[toktlogger]{extractTrawlSensorData}}
#' @export
plotReport <- function(nmeaMessages){
  p1 <- plotDoorSpread(nmeaMessages)
  p2 <- plotTrawlOpening(nmeaMessages)
  p3 <- plotTrawlClearance(nmeaMessages)
  p4 <- plotDepthProfile(nmeaMessages)

  gridExtra::grid.arrange(p1,p2,p3,p4,nrow=2)
}
