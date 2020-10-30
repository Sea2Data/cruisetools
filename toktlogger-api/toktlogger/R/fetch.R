API_ADRESS <- "http://toktlogger-jhjort.hi.no/api/"
ACTIVITYGROUP_DEFAULT = "Trål"

#' Set default Activity Main Group Name
#' @param activityGroup default Activity Main Group Name
#' @export
setActvityGroupDefault <- function(activityGroup){
  ACTIVITYGROUP_DEFAULT <<- activityGroup
}

#' Set default API adress
#' @param address address to toktlogger API
#' @export
setApiAdress <- function(address){
  API_ADRESS <<- address
}

#' Parses time as formatted by toktlogger
#' @details
#'  Example toktloggertime: 2020-10-26T20:05:39.144Z
#' @param toktloggerTime character() time in toktlogger format
#' @return POSIXct
#' @noRd
getPosixFromToktLogger <- function(toktloggerTime){
  return(as.POSIXct(substr(toktloggerTime,1,19), format="%Y-%m-%dT%H:%M:%S"))
}

#' @noRd
buildurl <- function(address, path, options){

  urlParts <- httr::parse_url(address)
  urlParts$path <- paste(urlParts$path, path, sep="/")
  urlParts$query <- options
  url <- httr::build_url(urlParts)
  return(url)
}

#' Fetches activities from currect cruise
#' @param apiAdress address to toktlogger API
#' @return table of activities
#' @export
fetchActivities <- function(apiAdress=API_ADRESS){
  getS <- buildurl(apiAdress, "activities/inCurrentCruise", list(format="csv"))
  return(data.table::fread(getS))
}

#' Fetches one activity from current cruise
#' @param localStationNumber local station number for activity
#' @param activityMainGroupName activity group, see examples for complete list
#' @param apiAdress address to toktlogger API
#' @return activity, one row formatted as the return from \code{\link[toktlogger]{fetchActivities}}
#' @examples
#'  #find activity type names
#'  activities <- fetchActivities()
#'  activitygroups <- unique(activities$activityMainGroupName)
#'
#'  #find valid local station numbers
#'  locStations <- activities$localstationNumber[activities$activityMainGroupName==activitygroups[1]]
#'
#'  #find the with activity type
#'  fetchActivity(locStations[1], activitygroups[1])
#'
#' @export
fetchActivity <- function(localStationNumber, activitygroup=ACTIVITYGROUP_DEFAULT, apiAdress=API_ADRESS){
  activities <- fetchActivities()

  if (!(activitygroup %in% activities$activityMainGroupName)){
    stop("Actvitity type name ", activitygroup, " is invalid for current cruise.")
  }

  activity <- activities[activities$activityMainGroupName==activitygroup & activities$localstationNumber == localStationNumber,]
  if (nrow(activity) > 1){
    stop("Activity not uniquely identified")
  }
  if (nrow(activity) < 1){
    stop("Activity not found")
  }
  return(activity)
}

#' Fetches all NMEA telegrams at activity
#' @param localStationNumber local station number for activity
#' @param activityMainGroupName activity group, see examples for complete list
#' @param apiAdress address to toktlogger API
#' @return all NMEA sentences at activity
#' @examples
#'  #for an example of output
#'  data("NMEAexample")
#' @export
fetchNmeaAtActivity <- function(localStationNumber, activitygroup=ACTIVITYGROUP_DEFAULT, apiAdress=API_ADRESS){
  activity <- fetchActivity(localStationNumber, activitygroup, apiAdress)
  starttime <- activity$startTime[1]
  stoptime<- activity$endTime[1]

  getS <- buildurl(apiAdress, "nmeaSentences/inPeriod", list(after=starttime, before=stoptime, format="csv"))
  return(data.table::fread(getS))
}
