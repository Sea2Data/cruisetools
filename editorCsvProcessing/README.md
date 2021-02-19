# BioticEditor csv processing

BioticEditor supports export of data in several csv files (fishstation.csv, catchsample.csv ...). The format of that export has changed in recent releases (BioticEditor 3.0 and 3.1), and those changes has had unintended consequences for some users. Scripts in this directory supports post-processing of these csv-files to add some columns in order to adapt data to workflows dependent on the old csv-output.

## Add species names to individual and prey
In particular species names (scientifc and norwegian) can be added to the prey-table and the individual table. Run the example script processCsv.R, and change the arguments to 'FixSpeciesNames' to the files you want to change (last lines in script).

## Update reference file
In addition to csv-file from editor, the script depends on a taxa table that relates species codes to species names. An example is included in exampleData, and updates to this can be downloaded from referenceeditor on HI-network (on site or VPN). Download the excel-file and export the sheet 'taxanames' as csv: https://referenceeditor.hi.no/apps/referenceeditor/v2/request/download/taxa/xlsx

## Other solutions
For many users a better approach would be to use tools that extract the same information from the xml-files. The script in this directory is provided as an option that does not require additional R-packages to be installed.

Users may also consider obtaining a similar extract from the xml files instead. Consider for instance bioticexplorer:

http://astarte.imr.no/shiny or https://github.com/MikkoVihtakari/BioticExplorer
