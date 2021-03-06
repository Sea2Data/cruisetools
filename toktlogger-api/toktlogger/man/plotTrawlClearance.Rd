% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{plotTrawlClearance}
\alias{plotTrawlClearance}
\title{Plots trawl clearance}
\usage{
plotTrawlClearance(nmeaMessages, ylim = c(-0.01, 4))
}
\arguments{
\item{nmeaMessages}{nmea message for station, see \code{\link[toktlogger]{extractTrawlSensorData}}}

\item{ylim}{y-axis (trawl clearance) range of plot}

\item{tolerance}{upper and lower limit tolerated for trawl opening}
}
\value{
ggplot2 object
}
\description{
Plots trawl clearance
}
