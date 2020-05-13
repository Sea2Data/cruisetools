# This is a simple script to plot interpreted luf20 files from NMDechosounder

#
# Install environment
# 

install.packages("dplyr")
install.packages("ggplot2")
install.packages("devtools")
install.packages("viridis")
library(devtools)

# Check that you have make
Sys.which("make")
# If not, follow this tutorial:
# https://cran.r-project.org/bin/windows/Rtools/
# Note that the path is "usr/bin" and not only "bin"

# Install Rstox & dependencies
# https://github.com/Sea2Data/Rstox
dep.pck <- c("data.table", "ggplot2", "pbapply", "rgdal", "rgeos", "rJava", "sp", "XML")
install.packages(dep.pck, repos="http://cran.us.r-project.org", type="binary")
devtools::install_github("Sea2Data/Rstox", ref="develop")

# Install Mikko's fancy package
# https://mikkovihtakari.github.io/ggOceanMaps/articles/ggOceanMaps.html
devtools::install_github("MikkoVihtakari/ggOceanMapsData")
devtools::install_github("MikkoVihtakari/ggOceanMaps")

# Parameters
acocat = 27 # The acoustic category to be plotted
cruise = 2019847 # The cruise number

# Load environments
library(ggOceanMaps)
library(Rstox)
library(ggplot2)
library(dplyr)
library(viridis)

# downlaod data from NMD (Only needed once, or point to your file)
project <- getNMDdata(cruise = cruise)

# Read and reformat acosutic data
acdat <- readXMLfiles(paste0(project,'/input/acoustic'))
NASC  <- acdat$ReadAcousticXML_AcousticData_NASC.txt
disttab <- acdat$ReadAcousticXML_AcousticData_DistanceFrequency.txt
# Filter the NASC values to one acocat
NASC <- NASC[NASC$acocat==acocat,]

# sum sa for each unique start_time
L <- aggregate(NASC$sa, by=list(start_time=NASC$start_time), FUN=sum)
tab <- inner_join(disttab, L, by = 'start_time')
# Sort the ddata for better viz
tab2 <- arrange(tab, x)
basemap(limits = c(min(tab2$lon_start), max(tab2$lon_start), min(tab2$lat_start), max(tab2$lat_start))) + 
  geom_spatial_point(data=tab2, aes(x=lon_start, y=lat_start, size=x, color=log(x))) +
  scale_color_viridis(trans="log")



