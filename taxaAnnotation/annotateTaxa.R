library(worms)

#' Makes table of higher order taxonomic classifications
#' @description 
#'  Dowloads taxonomic classifications from WORMS (http://www.marinespecies.org).
#' @details 
#'  Taxonomic records are accessed by WORMS web-services.
#'  Internet connection is therefore necessary for this function to work.
#'  It is also polite to be gentle with the server, and avoid uncessary web-acess. 
#'  Make your scripts download once or seldom and re-use tables rather than re-downloading.
#' @param aphiaids vector of aphia IDs, identifies scientific name in WORMS.
#' @param levels higher taxonomic levels to include in table.
#' @return data.frame with columns: AphiaID, scientificname, status, rank and the requested levels.
#'  AphiaID is a unique identifier in the WORMS database. Identifying a specific scientific name and taxonomic description (citation)
#'  status indicates whether the scientific name is accepted for the taxonomic description by the WORMS concortium
#'  rank specifies the rank of the taxonomic classification, e.g. species.
makeTaxaTable <- function(aphiaids, levels=c("genus", "family", "order", "class", "phylum")){
  
  aphiaids <- unique(as.integer(aphiaids))
  
  if (any(is.na(aphiaids))){
    stop("Argument 'aphiaids' contains NAs")
  }
  
  columns <- c("AphiaID", "scientificname", "status", "rank", levels)
  
  taxaTable <- worms::wormsbyid(aphiaids)
  taxaTable$AphiaID <- as.character(taxaTable$AphiaID)
  
  if (!all(columns %in% names(taxaTable))){
    missing <- columns[!(columns %in% names(taxaTable))]
    stop(paste("Some levels are not valid for selected taxa:", paste(missing, collapse=", ")))
  }
  
  if (any(taxaTable$status != "accepted")){
    warning("Some provided Aphia IDs encode non-accepted scientific names.")
  }
  
  return(taxaTable[,columns])
}

#' example for annotating biotic data with family for recorded species.
example <- function(){
  
  #load example data (ecosystem Helmer Hanssen 2018)
  eco_hh <- readRDS("./exampledata.rds")
  
  #make table of higher taxonomic classifications for all aphia codes in data
  taxaTable <- makeTaxaTable(eco_hh$aphia[!is.na(eco_hh$aphia)])
  
  #extract genus and aphia id
  genusTable <- taxaTable[,c("AphiaID", "rank", "family")]
  
  #make sure column names are not already used
  stopifnot(!all(names(genusTable) %in% names(eco_hh)))
  
  #annotate data with higher taxonomic classifications
  #force value on missing aphia ids to avoid rows getting lost in merge
  eco_hh[is.na(eco_hh$aphia), "aphia"] <- "<NA>"
  eco_hh_annotated <- merge(eco_hh, genusTable, by.x="aphia", by.y="AphiaID", all.x=T)
  eco_hh[eco_hh$aphia=="<NA>", "aphia"] <- NA
  eco_hh_annotated[eco_hh_annotated$aphia=="<NA>", "aphia"] <- NA
  
  stopifnot(nrow(eco_hh) == nrow(eco_hh_annotated))
  
  #
  # inspect data
  #
  
  # all data without commonname are missing specimenid (hauls with no individuals sampled)
  print("specimenid for rows missing commonname")
  print(eco_hh_annotated[is.na(eco_hh_annotated$commonname),"specimenid"])
  
  # all data without aphia codes are wither missing commonname, or is coded unkown or foregin object.
  print(paste("commonname for rows missing aphiaid (", sum(is.na(eco_hh_annotated$aphia)), " cases)", sep=""))
  print(eco_hh_annotated[is.na(eco_hh_annotated$aphia),c("commonname")])
  
  #all data with missing family annotation are of higher rank
  print("Rank for rows missing family annotation")
  print(table(eco_hh_annotated[is.na(eco_hh_annotated$family),c("rank")], useNA = "ifany"))
  
  eco_hh_annotated[is.na(eco_hh_annotated$family), "family"] <- "<NA>"
  
  hauls_by_family <- aggregate(list(hauls=eco_hh_annotated$serialnumber), by=list(family=eco_hh_annotated$family), FUN=function(x){length(unique(x))})
  hauls_by_family <- hauls_by_family[order(hauls_by_family$hauls, decreasing = T),]
  print("Number of hauls with family present:")
  print(hauls_by_family)
}