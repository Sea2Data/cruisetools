
#' Adds selected columns from the catchsample to the individual table.
#' Defaults to taxa identifier and aphia id
AddCatchColumnsIndividual <- function(catchsample, individual, columnNames=c("catchcategory", "aphia")){
  mergeKeysCatch <- c("m.missiontype", "m.startyear", "m.platform", "m.missionnumber","f.serialnumber", "catchsampleid")
  mergeKeysIndividual <- c("m.missiontype", "m.startyear", "m.platform", "m.missionnumber","f.serialnumber", "c.catchsampleid")
  
  if (!all(columnNames %in% names(catchsample))){
    missing <- columnNames[!(columnNames %in% names(catchsample))]
    stop(paste("Some requested columns not found in catchsample:", paste(missing, collapse=",")))
  }
  individual$orderCol <- 1:nrow(individual)
  individual <- merge(catchsample[,unique(c(mergeKeysCatch, columnNames))], individual, all.y=T, by.x=mergeKeysCatch, by.y=mergeKeysIndividual)
  individual <- individual[order(individual$orderCol),]
  individual$orderCol <- NULL
  
  return(individual)
  
}

#' Adds selected information from the taxanames table to a table with tsn (imrTsn) column
AddTaxaNames <- function(table, taxanamestable, speciesIdColumn="catchcategory", languages=c("Norwegian", "Scientific")){
  if (!(speciesIdColumn %in% names(table))){
    stop(paste("Column", speciesIdColumn, "not found in table."))
  }
  if (!("Tsn" %in% names(taxanamestable))){
    stop("Column Tsn not found in taxanamestable")
  }
  if (!("Language" %in% names(taxanamestable))){
    stop("Column Language not found in taxanamestable")
  }
  if (!all(languages %in% taxanamestable$Language)){
    missing <- languages[!(languages %in% taxanamestable$Language)]
    stop("Not all requested languages found in taxanamestable. Missing: ", paste(missing, collapse=","))
  }
  newCols <- paste0(languages, ".taxa")
  colOrder <- c(newCols, names(table))
  if (any(newCols %in% names(table))){
    exists <- newCols[newCols %in% names(table)]
    stop("Some requested columns already exist in table:", paste(exists, sep=","))
  }
  
  # add requested species names to table
  table$orderCol <- 1:nrow(table)
  for (i in 1:length(languages)){
    nametab <- taxanamestable[taxanamestable$Language == languages[i],c("Tsn", "Name")]
    nametab <- nametab[!duplicated(nametab$Tsn),]
    names(nametab) <- c("Tsn", newCols[i])
    table <- merge(table, nametab, all.x=T, by.x=speciesIdColumn, by.y="Tsn")
  }
  table <- table[order(table$orderCol),]
  table$orderCol <- NULL
  
  return(table[,colOrder])
}

#' Read csv file from bioticEditor as data.frame
ReadEditorCsv <- function(fileName, encoding){
  return(read.csv(fileName, sep="\t", stringsAsFactors = F, check.names = F, encoding=encoding))
}
#' Read csv file for taxa names as data.frame
ReadTaxaNamesCsv <- function(fileName, encoding){
  return(read.csv(fileName, sep=";", stringsAsFactors = F, encoding=encoding))
}

#' Reads in csv files exported from BioticEditor
#' Adds species code from the catchsample-table to the individual-table
#' Adds norwegian name and scientific name from from the taxaname table to the individual table and the preytable
#' Writes annotated versions of individual table and preytable
FixSpeciesNames <- function(preyfile, individualfile, catchsamplefile, taxafilename, newpreyfile=NULL, newindividualfile=NULL, overwrite=F, encoding){
  
  # read input
  individual <- ReadEditorCsv(individualfile, encoding)
  prey <- ReadEditorCsv(preyfile, encoding)
  catch <- ReadEditorCsv(catchsamplefile, encoding)
  taxa <- ReadTaxaNamesCsv(taxafilename, encoding)
  
  nrowind <- nrow(individual)
  nrowprey <- nrow(prey)
  
  # add columns
  individual <- AddCatchColumnsIndividual(catch, individual)
  individual <- AddTaxaNames(individual, taxa, "catchcategory")
  prey <- AddTaxaNames(prey, taxa, "preycategory")
  
  stopifnot(nrow(individual) == nrowind)
  stopifnot(nrow(prey) == nrowprey)
  
  # use a default filename if output file names are not given
  if (is.null(newpreyfile)){
    newpreyfile <- gsub(".csv", "", preyfile)
    newpreyfile <- paste0(newpreyfile, "_annotated.csv")
  }
  if (is.null(newindividualfile)){
    newindividualfile <- gsub(".csv", "", individualfile)
    newindividualfile <- paste0(newindividualfile, "_annotated.csv")
  }
  
  # stop if file aready exist
  if (file.exists(newpreyfile) & !overwrite){
    stop(paste("File: ", newpreyfile, "already exists. Use option 'overwrite', to overwrite."))
  }
  if (file.exists(newindividualfile) & !overwrite){
    stop(paste("File: ", newindividualfile, "already exists. Use option 'overwrite', to overwrite."))
  }
  
  # write annotated tables
  write.table(individual, sep="\t", file=newindividualfile, na = "", quote = F, row.names = F)
  write.table(prey, sep="\t", file=newpreyfile, na = "", quote = F, row.names = F)
  
}

# Add species names for example files
FixSpeciesNames(preyfile="exampleData/catchprey.csv", 
                individualfile="exampleData/individual.csv", 
                catchsamplefile="exampleData/catchsample.csv", 
                taxafilename="exampleData/taxanames_enc.csv", overwrite = T,
                encoding="UTF-8")
