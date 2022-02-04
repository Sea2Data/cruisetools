API_ADRESS <- "http://toktlogger-bonnevie.hi.no/api/"

#' @noRd
buildurl <- function(address, path, options){
  
  urlParts <- httr::parse_url(address)
  urlParts$path <- paste(urlParts$path, path, sep="/")
  urlParts$query <- options
  url <- httr::build_url(urlParts)
  return(url)
}

#' Extract biomass from toktlogger
#' @description
#'  If toktlogger is set up to report total biomass from the echosounder
#'  this function allows extraction of biomass and depth record from the last minutes.
#' @param minutes The number of minutes to extract
#' @param biomass_mapping The mapping name set up for echosounder biomass in the toktlogger NMEA data mapping
#' @param depth_mapping The mapping name set up for depth in the toktlogger NMEA data mapping
#' @param apiAdress The address to the toklogger to request data from
#' @return data table with columns:
#'  \describe{
#'   \item{timestamp}{Time stamp for observations}
#'   \item{biomass}{The biomass value as returned from toktlogger}
#'   \item{depth}{The depth as returned from toktlogger}
#'  }
extract_biomass <- function(minutes=10, biomass_mapping="biomass_density_SA_38", depth_mapping="depth", apiAdress=API_ADRESS){
  
  now <- as.POSIXct(Sys.time())
  start <- now - minutes*60
    
  getS <- buildurl(apiAdress, "instrumentData/inPeriod", list(after=as.character(start), before=as.character(now), mappingIds=paste(depth_mapping, biomass_mapping, sep=","), format="csv"))
  extract <- data.table::fread(getS)
  
  biomass <- extract[extract$mappingName == biomass_mapping,c("timestamp", "stringValue")]
  names(biomass) <- c("timestamp", "biomass")
  depth <- extract[extract$mappingName == depth_mapping,c("timestamp", "stringValue")]
  names(depth) <- c("timestamp", "depth")
  
  return(merge(depth, biomass))
}

#' Plots integrator line and depth
#' @param biomass a data table as returned by ~\code{\link{extract_biomass}}
plot_biomass <- function(biomass){
  
  depthscale <- sum(biomass$biomass) / max(biomass$depth)
  biomass$scaleddepth <- sum(biomass$biomass) - biomass$depth*depthscale
  
  biomass$integ <- cumsum(biomass$biomass)
  ggplot2::ggplot(biomass) + 
    ggplot2::geom_line(ggplot2::aes(timestamp, integ, col="biomass")) +
    ggplot2::geom_line(ggplot2::aes(timestamp, scaleddepth, col="bottom depth")) +
    ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~ (./ depthscale ) - max(biomass$depth), name="depth",),
                                
                                ) +
    ggplot2::ylab("Integrated biomass")
}

#' Example extracting last 20 minutes and plotting
example <- function(){
  plot_biomass(extract_biomass(20))
}
