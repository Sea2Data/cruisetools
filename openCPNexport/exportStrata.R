library(XML)
#
# make gpx tracks that correspond to polygons in strata files.
#

#' Writes spatial strata to gpx file
#' which are readable by e.g. openCPN
#' Strata borders are modelled as tracks
#' Strata names are represented by a waypoint in each strata
#' @param stratumPolygon sp::SpatialPolygonsDataFrame with the strata names in the column "StratumName". This is the format used by StoX and WKT files may be read by RstoxBase::DefineStratumPolygon
#' @param fileName the file to write gpx data to
#' @param symbol waypoint symbol to use for waypoints representing strata names.
writeStrataGpx <- function(stratumPolygon, fileName, symbol="square"){
  
  if (!"StratumName" %in% names(stratumPolygon)){
    stop("Column 'StratumName' not present in stratumPolygon")
  }
  
  # fake creator to affect default openCPN labelling behaviour
  creator <- "GPS Visualizer https://www.gpsvisualizer.com/"
  
  poly <- sf::st_transform(sf::st_as_sf(stratumPolygon), "EPSG:4326")
  
  t <- XML::xmlOutputDOM("gpx", attrs = c(creator=creator))
  
  message(paste("Writing strata centroids"))  
  for (i in 1:nrow(poly)){
    
    # get centroid of convex hull.
    # gives odd placement when one strata is embedded in another, but avoids
    # needing strictly valid spherical geometries
    coordinates <- sf::st_coordinates(sf::st_centroid(sf::st_convex_hull(poly[i,])))
    t$addTag("wpt", attrs = c(lat=coordinates[[1,"Y"]], lon=coordinates[[1,"X"]]), close=F)
    t$addTag("name", poly$StratumName[i])
    t$addTag("sym", symbol)
    t$closeTag() #wpt
  }
  
  for (i in 1:nrow(poly)){
    coordinates <- sf::st_coordinates(poly[i,])
    t$addTag("trk", close=F)
    t$addTag("name", poly$StratumName[i])
    t$addTag("trkseg", close=F)
    message(paste("Writing strata", i))
    for (j in 1:nrow(coordinates)){
      t$addTag("trkpt", attrs = c(lat=coordinates[[j,"Y"]], lon=coordinates[[j,"X"]]))
    }
    t$closeTag() #trkseg
    t$closeTag() #trk
  }
  
  t$closeTag() #gpx
  
  XML::saveXML(t$value(), file=fileName)
}

#read strata
vintertoktStrata <- RstoxBase::DefineStratumPolygon(NULL, F, "ResourceFile", "~/workspace/stox/reference/stratum/vintertokt_barentshavny.txt")

#change strata names for clarity
vintertoktStrata$StratumName <- paste("Strata:", vintertoktStrata$StratumName)

#write as gpx track
writeStrataGpx(vintertoktStrata, "~/hi_sync/tokt_og_felt/2022_vintertoktet/rute/strataTrack.gpx")

#some standard area codes are included in RstoxFDA and may be used directly from there
writeStrataGpx(RstoxFDA::mainareaFdir2018, "~/temp/test.gpx")