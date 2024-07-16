#Recourse Links
#https://search.r-project.org/CRAN/refmans/geodata/html/worldclim.html
#https://github.com/bpbond/cosore/blob/master/gcb/gcb_ms.Rmd 

#Download data (attempt #1)
#Error Message: cannot open URL
#Warning Message: getData will be removed in a future version of raster. Please use the geodata package instead
library(raster)
library(sp)
tmean <- getData("worldclim", path = "wc10/", var = "tmean", res = 10, download = !file.exists("wc10/wc10/tmean1.hdr"))

#Download data (attempt #2)
#Error Message: The geodata server seems to be off-line
#This does get a wc10 folder in files but it is essentially empty
library(geodata)
tavg <- worldclim_global("tavg", res=10, path="wc10/", version="2.1", lon=-77, lat=39)