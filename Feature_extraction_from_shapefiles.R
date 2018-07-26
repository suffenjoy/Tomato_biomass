#Extract reflectance and temperature for each shapefile

##Read shapefiles
path_shp <- "F:/Tomato biomass/Processed_results/shapefiles"
row_north <- shapefile(file.path(path_shp,"north_rows_061618.shp"))
row_south <- shapefile(file.path(path_shp,"south_rows_061618.shp"))

##Read NDVI rasters
path_outputs <- "F:/Tomato biomass/Processed_results/rasters"
##NDVI for a smaller region
tomato_ndvi_061618 <- raster(file.path(path_outputs,"tomato_061618_ndvi.tif"))
#Remove values == -10000, which are actually the NAs
tomato_ndvi_061618[tomato_ndvi_061618==-10000] <- NA
##Mask out soil pixels 
hist(tomato_ndvi_061618)
##Only select pixels that NDVI>0.4
veg_tomato_061618 <- calc(tomato_ndvi_061618, function(x){x[x<0.4] <- NA; return(x)})
tomato_ndvi_061618_masked <- raster::mask(tomato_ndvi_061618, veg_tomato_061618)
plot(tomato_ndvi_061618_masked)

##Extract data from raster
##row average ndvi 
north_ndvi_061618 <- raster::extract(tomato_ndvi_061618_masked, row_north, fun = mean, na.rm = TRUE, df = TRUE)
south_ndvi_061618 <- raster::extract(tomato_ndvi_061618_masked, row_south, fun = mean, na.rm = TRUE, df = TRUE)

#add block name to 
north_ndvi_061618$block <- row_north_polys_df@data$block
south_ndvi_061618$block <- row_south_polys_df@data$block
#combine south and north
row_ndvi_061618 <- rbind(north_ndvi_061618, south_ndvi_061618)
names(row_ndvi_061618)[2] <- "NDVI"
#block average
by_block <- group_by(row_ndvi_061618, block)
block_ndvi_061618 <- as.data.frame(summarise(by_block, mean_NDVI = mean(NDVI)))
#remove the buffer 
block_ndvi_061618 <- subset(block_ndvi_061618, block_ndvi_061618$block!="buffer")
#order depend on NDVI value
block_ndvi_061618[order(block_ndvi_061618$mean_NDVI),]

#-------------------------------------------------------------------------#
#Read other rasters
path_raster <- "F:/Tomato biomass/Processed_UAV_images/20180616"
path_code <- "F:/Tomato biomass/Code"
source(file.path(path_code,"Read_uav.r"))

uav_061618 <- read_uav(path = path_raster, 
                       blue = "tomato_061618_thermalrededgemerge_2_transparent_reflectance_blue.tif",
                       green = "tomato_061618_thermalrededgemerge_2_transparent_reflectance_green.tif",
                       red = "tomato_061618_thermalrededgemerge_2_transparent_reflectance_red.tif",
                       rededge = "tomato_061618_thermalrededgemerge_2_transparent_reflectance_rededge.tif",
                       nir = "tomato_061618_thermalrededgemerge_2_transparent_reflectance_nir.tif",
                       ndvi = "tomato_061618_thermalrededgemerge_2_index_ndvi.tif",
                       thermal = "tomato_061618_thermalrededgemerge_2_transparent_reflectance_thermal ir.tif",
                       dsm = "tomato_061618_thermalrededgemerge_2_dsm.tif",
            
                       crop = "tomato_061618_ndvi.tif")
#thermal = "tomato_061618_thermalrededgemerge_2_transparent_reflectance_thermal ir.tif", 
#----------------------------------------------------------------------------#
#Extract temperature
##Mask out the nonveg pixels
hist(uav_061618$ndvi)
##Only select pixels that NDVI>0.4
veg_tomato_061618 <- calc(uav_061618$ndvi, function(x){x[x<0.4] <- NA; return(x)})
uav_061618_masked <- raster::mask(uav_061618, veg_tomato_061618)
#extract data
north_uav_061618 <- raster::extract(uav_061618_masked, row_north, fun = mean, na.rm = TRUE, df = TRUE)
south_uav_061618 <- raster::extract(uav_061618_masked, row_south, fun = mean, na.rm = TRUE, df = TRUE)
#add block name
north_uav_061618$block <- as.factor(row_north@data$block)
south_uav_061618$block <- as.factor(row_south@data$block)
#combine south and north 
rowmean_uav_061618 <- rbind(north_uav_061618, south_uav_061618)
#calculate block average 
blockmean_uav_061618 <- aggregate(rowmean_uav_061618[2:9], list(rowmean_uav_061618$block), mean)
names(blockmean_uav_061618)[1] <- "block"

#-----------------------------------------------------------#
#add more indices 
source(file.path(path_code, "Vegetation_indices.r"))
source(file.path(path_code, "atmosphere_correction.r"))
#NDRE
blockmean_uav_061618$ndre <- ndre(blockmean_uav_061618)
#GNDVI
blockmean_uav_061618$gndvi <- gndvi(blockmean_uav_061618)
#Temperature
blockmean_uav_061618$temp <- T_obj(blockmean_uav_061618$therm, 0.98, 35, 35, 0.5, 60)

#Output 
path_df <- "F:/Tomato biomass/Processed_results/dataframes"
write.csv(blockmean_uav_061618, file.path(path_df, "blockmean_uav_061618.csv"), row.names = FALSE)


#--------------------------------------------------------------------------------#



#-----------------------------------------------------------------------------#

#thermal data extraction 
therm_061618 <- raster(file.path(path_raster, "tomato_061618_thermalrededgemerge_2_transparent_reflectance_thermal ir.tif"))
#crop to smaller extent
therm_061618_crop <- crop(therm_061618, extent(veg_tomato_061618))
#mask out non-veg pixels
therm_061618_masked <- raster::mask(therm_061618_crop, resample(veg_tomato_061618, therm_061618_crop))
#extract function 
north_therm_061618 <- raster::extract(therm_061618_masked, row_north, fun = mean, na.rm = TRUE, df  = TRUE)
south_therm_061618 <- raster::extract(therm_061618_masked, row_south, fun = mean, na.rm = TRUE, df  = TRUE)
#add block name  
north_therm_061618$block <- row_north@data$block
south_therm_061618$block <- row_south@data$block
#combine south and north
row_therm_061618 <- rbind(north_therm_061618, south_therm_061618)
names(row_therm_061618)[2] <- "therm"
#block average
by_block <- group_by(row_therm_061618, block)
block_therm_061618 <- as.data.frame(summarise(by_block, mean_therm = mean(therm)))
#remove the buffer 
block_therm_061618 <- subset(block_therm_061618, block_therm_061618$block!="buffer")
#order depend on therm value
block_therm_061618[order(block_therm_061618$mean_therm),]


#--------------------------------------------------------------------------#
#temperature correction 
source(file.path(path_code, "atmosphere_correction.r"))
north_therm_061618$temp <- T_obj(north_therm_061618$therm, 0.98, 35, 35, 0.5, 60)
row_therm_061618$temp <- T_obj(DN = row_therm_061618$therm, 0.98, 35, 35, 0.5, 60)

