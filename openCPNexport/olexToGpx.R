library(data.table)
library(XML)

#' @noRd
pop <- function(lines){
  if (length(lines)<=1){
    return(c())
  }
  else{
    return(lines[2:length(lines)])
  }
}

#' @noRd
parseWaypoint <- function(lines){
  waypoint <- list()
  waypoint$lat <- as.numeric(NA)
  waypoint$lon <- as.numeric(NA)
  waypoint$time <- as.POSIXct(NA)
  waypoint$symbol <- as.character(NA)
  waypoint$name <- as.character(NA)
  waypoint$descr <- as.character(NA)
  waypoint$comment <- as.character(NA)
  
  coordstring <- lines[1]
  coordlist <- strsplit(coordstring, " ")[[1]]
  lat <- as.numeric(coordlist[1])/60
  lon <- as.numeric(coordlist[2])/60
  
  if (!is.na(coordlist[3])){
    time <- as.POSIXct(as.integer(coordlist[3]), origin="1970-01-01")   
  }
  else{
    time <- NA
  }

  if (!is.na(coordlist[4])){
    symbol <- coordlist[4]    
  }
  else{
    symbol <- NA
  }

  
  waypoint$lat <- lat
  waypoint$lon <- lon
  waypoint$time <- time
  waypoint$symbol <- symbol
  
  lines <- pop(lines)
  if (substr(lines[1], 1,4)=="Navn"){
    waypoint$name <- trimws(substr(lines[1],5, nchar(lines[1])))
    lines <- pop(lines)
  }
  while (length(lines) > 0 && substr(lines[1], 1,6)=="MTekst"){
    if (is.na(waypoint$comment)){
      waypoint$comment <- ""
    }
    
    waypoint$comment <- paste(waypoint$comment, substr(lines[1],7, nchar(lines[1])), " ")
    lines <- pop(lines)
  }
  
  output <- list()
  output$waypoint <- waypoint
  output$lines <- lines

  return(output)
}

#' @noRd
parseOlexRoute <- function(lines){
  route <- list()
  stopifnot(substr(lines[1],1,4)=="Rute")
  route$routename <- trimws(substr(lines[1],5,nchar(lines[1])))
  
  lines <- pop(lines)
  if(substr(lines[1],1,8)=="Rutetype"){
    route$routetype <- trimws(substr(lines[1],9,nchar(lines[1])))    
    lines <- pop(lines)
  }
  else{
    route$routetype <- NA
  }

  if(substr(lines[1],1,10)=="Linjefarge"){
    route$linecolor <- trimws(substr(lines[1],11,nchar(lines[1]))) 
    lines <- pop(lines)
  }
  else{
    route$linecolor <- NA
  }
  
  if(substr(lines[1],1,9)=="Plottsett"){
    route$plotsett <- trimws(substr(lines[1],10,nchar(lines[1])))  
    lines <- pop(lines)
  }
  else{
    route$plotsett <- NA
  }
  if(substr(lines[1],1,7)=="Fikspos"){
    route$fixpos <- trimws(substr(lines[1],8,nchar(lines[1])))  
    lines <- pop(lines)
  }
  else{
    route$fixpos <- NA
  }
  
  wpn <- 1
  while(length(lines) > 0 && trimws(lines[1])!=""){
    pp <- parseWaypoint(lines)
    lines <- pp$lines
    route$waypoints[[wpn]] <- pp$waypoint
    wpn <- wpn + 1
  }
  stopifnot(trimws(lines[1])=="")
  lines <- pop(lines)
  
  output <- list()
  output$lines <- lines
  output$route <- route
  
  return(output)
  
}

#' waypoint
#' 
#' @description list with members
#'  \describe{
#'   \item{lat}{latitude in decimal degrees (WGS84)}
#'   \item{lon}{longitude in decimal degrees (WGS84)}
#'   \item{time}{PSOIXct time}
#'   \item{symbol}{code for symbol, specific to plotters}
#'   \item{name}{name of waypoint}
#'   \item{descr}{description of waypoint}
#'   \item{comment}{comments for waypoint}
#'  }
#' 
#' 
#' @name waypoint
#' 
"waypoint"

#' route
#' 
#' @description
#' a list with members:
#' \descibe{
#'  \item{routename}{name of route}
#'  \item{routetype}{type of route, used for visualization, derived from Olex, not supperted by gpx}
#'  \item{linecolor}{linecolor, used for visualization, derived from Olex, not supperted by gpx}
#'  \item{plotsett}{dont know, probably used for visualization, derived from Olex, not supperted by gpx}
#'  \item{fixpos}{dont know, probably used for visualization, derived from Olex, not supperted by gpx}
#'  \item{waypoints}{a list of \code{\link{waypoint}}}
#' }
#' 

#' nestedRoutes
#' 
#' @description
#' a list of \code{\link{route}}
#' @name nestedRoutes
"nestedRoutes"

#' plotterData
#' 
#' @description
#' list of plotterdata contents. May have a member 'routes' which is an \code{\link{nestedRoutes}} object
#' 
#' @name plotterData
"plotterData"

#' Read routes from OLEX - file
#' 
#' @details 
#'  Implementation is not based on proper format description. May crash if untested Olex-elements are introduced.
#' @param olexfile file to be read. May be .gz compressed.
#' @param encoding encoding used in file
#' @return \code{\link{plotterData}} with routes from the file.
parseOlex <- function(olexfile, encoding="latin1"){
  lines <- readLines(olexfile, encoding=encoding)
  output <- list()
  output$routes <- list()

  while(nchar(lines[1])==0){
    lines <- pop(lines)
  }
  
  routenr <- 1
  while (length(lines) > 0){
    head <- lines[1]
    if (substr(head,1,4) == "Rute"){
      pp <- parseOlexRoute(lines)
      lines <- pp$lines
      output$routes[[routenr]] <- pp$route
      routenr <- routenr + 1
    }
    else if (is.na(lines[1])){
      lines <- pop(lines)
    }
    else{
      stop("Did not recognize: ", head)
    }
  }
  return(output)
}

#' extract routes as a date table
#' @param routes nestedRoute
#' @return \code{\link[data.table]{data.table}} of waypoints.
flattenRoute <- function(routes){
  flatr <- NULL
  for (r in routes){
    rframe <- data.frame(r[1:4])
    for (wp in r$waypoints){
      wpframe <- data.frame(wp, stringsAsFactors = F)
      flatr <- rbind(flatr, cbind(rframe, wpframe))
    }
  }
  return(data.table::as.data.table(flatr))
}

#' Writes route as gpx
#' @param filename filename to write to
#' @param nestedRoute \code{\link{nestedRoute}} object to write
#' @param symbolMap list mapping names of waypoint symbols in 'nestedRoute' to replacement names understood by target plotter
#' @param creator value for the 'creatur' attribute in the gpx file. Defaults to openCPN.
writeGpxRoute <- function(filename, nestedRoute, symbolMap=list(Brunsirkel="Circle", Rødramme="Square", Empty="empty"), creator="OpenCPN"){
  t <- XML::xmlOutputDOM("gpx", attrs = c(creator=creator))
  for (r in nestedRoute){
    t$addTag("rte", close=F)
    t$addTag("name", r$routename)
    for (wp in r$waypoints){
      t$addTag("rtept", attrs = c(lat=wp$lat, lon=wp$lon), close=F)
      if (!is.na(wp$time)){
        t$addTag("time", wp$time)        
      }
      if (!is.na(wp$name)){
        t$addTag("name", wp$name)        
      }
      if (!is.na(wp$comment)){
        t$addTag("cmt", wp$comment)        
      }
      if (!is.na(wp$descr)){
        t$addTag("desc", wp$descr)  
      }
      if (!is.na(wp$symbol)){
        t$addTag("sym", symbolMap[[wp$symbol]])        
      }
      t$addTag("type", "WPT")
      t$closeTag() #wpt
    }
    t$closeTag() #rte
  }
  saveXML(t$value(), file=filename)
}

#' Changes symbols for nameless waypoints
#' 
#' @details 
#'  Useful for instance for distinguising transect endpoints from planned fishing stations.
#' 
#' @param nestedRoute \code{\link{nestedRoute}} object to modify
#' @param symbol name for symbol that should be set on unnamed waypoints.
#' @param \code{\link{nestedRoute}} modified nestedRoute.
set_symbols_nonames <- function(nestedRoute, symbol="Empty"){
  for (i in 1:length(nestedRoute)){
    for (j in 1:length(nestedRoute[[i]]$waypoints)){
      if (is.na(nestedRoute[[i]]$waypoints[[j]]$name)){
        nestedRoute[[i]]$waypoints[[j]]$symbol <- symbol
      }
    }
  }
  return(nestedRoute)
}

#' Write waypoints as gpx
#' @param filename filename to write
#' @param nestedRoute \code{\link{nestedRoute}} object to extract waypoints from
#' @param symbolMap list mapping names of waypoint symbols in 'nestedRoute' to replacement names understood by target plotter
writeGpxWaypoints <- function(filename, waypoints, symbolMap=list(Brunsirkel="circle", Rødramme="square", Empty="empty")){
  t <- XML::xmlOutputDOM("gpx")
  for (wp in waypoints){
    t$addTag("wpt", attrs = c(lat=wp$lat, lon=wp$lon), close=F)
    t$addTag("time", wp$time)
    t$addTag("name", wp$name)
    t$addTag("cmt", wp$comment)
    t$addTag("desc", wp$descr)
    t$addTag("sym", symbolMap[[wp$symbol]])
    t$addTag("type", "WPT")
    t$closeTag() #wpt
  }
  saveXML(t$value(), file=filename)
}

# example of use conversion
ex_conversion <- function(olexfile="~/hi_sync/tokt_og_felt/2021_skreitokt/rute/Vestfjorden.Ruter", outfile="~/hi_sync/tokt_og_felt/2021_skreitokt/rute/Vestfjorden.gpx"){
  cont<-parseOlex(olexfile)
  cont$routes <- set_symbols_nonames(cont$routes)
  writeGpxRoute(outfile, cont$routes)  
}

#example fo use importing routes
ex_import <- function(){
  cont <- parseOlex("~/hi_sync/tokt_og_felt/kysttokt_okt_2020/kurser/olexplot-jh20f.gz")
  dt <- flattenRoute(cont$routes)
  stations <- dt[!is.na(dt$name),]
  
}