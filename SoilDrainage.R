##https://bolin.su.se/data/hugelius-2020-peatland-1?n=hugelius-2020
##https://www.pnas.org/doi/10.1073/pnas.1916387117#data-availability-1 above is found from this email link
#percent of permafrost peatlands (Histels) in each gridcell
histel_frac <- terra::rast("Histel_fraction.tif")
testHist <- terra::extract(histel_frac, test[2:3])#all NA but might make sense
srdbhl_hist <- terra::extract(histel_frac, srdb_hl[3:2])#all NA
plot(histel_frac) #this works
#depth of peatlands
depth <- terra::rast("Northern_peatland_maps_Hugeliusetal2020/Hugelius_etal_2020_PNAS_supplement_grids_10km_Int_WAED/Mean_potential_peat_depth_cm.tif")
testDepth <- terra::extract(depth, test[2:3])#all NA but might make sense
srdbhl_depth <- terra::extract(depth, srdb[15:14])#all NA
plot(depth)#this works
#try with just one point
pls <- terra::extract(depth, 68.62000, -149.3300)
#doesn't work error message
#xp <- SpatialPoints(srdb_hl[3:2], proj4string = CRS("+proj=longlat + datum=WGS84"))
#xp <- spTransform(xp, projection(depth))
#srdb_hl$HERE <- raster::extract(depth, xp, buffer = 1000, fun = mean)
#the extent is  -10983883, 12086117, 2711334 unlike the other data which have normal ranges of lats and longs I think using the lat and long to get data isn't getting the right place