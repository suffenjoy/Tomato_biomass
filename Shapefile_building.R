library(raster)
library(dplyr)

#Read raster file of NDVI
path_outputs <- "F:/Tomato biomass/Processed_results/rasters"
tomato_ndvi_061618 <- raster(file.path(path_outputs,"tomato_061618_ndvi.tif"))

#Remove values == -10000, which are actually the NAs
tomato_ndvi_061618[tomato_ndvi_061618==-10000] <- NA

#Read shapefile of the first block
path_shapefiles <- "F:/Tomato biomass/Processed_results/shapefiles"
block_1 <- shapefile(file.path(path_shapefiles, "061618_block1.shp"))
row_1 <- shapefile(file.path(path_shapefiles, "061618_row1_north.shp"))

#plot the raster and the shapefile
plot(tomato_ndvi_061618, axes = FALSE, box = FALSE, legend = FALSE)
plot(row_1, add = TRUE)
plot(block_1, add = TRUE)

#north 
row_north_list <- list()
for(i in 1:162){
  #move the first row 
  ##For the horizontal direction, 162 rows, 246.2553m, so the distance between each row is about 1.52-1.53m
  ##For the vertical direction, 162 rows, 7.67
  xym_north <- cbind(geom(block_1)[,5]+1.525*(i-1), geom(block_1)[,6]-0.048*(i-1))
  
  polygon_north <- Polygon(xym_north)
  
  row_north_list[[i]] <- Polygons(list(polygon_north), ID = paste("row",i, sep = "_"))
}
row_north_polys <- SpatialPolygons(row_north_list, proj4string = crs(block_1))
row_north_polys_df <- SpatialPolygonsDataFrame(row_north_polys, data.frame(id = 1:162, row.names = names(row_polys), NS = "north"))

#south part
row_south_list <- list()
for(i in 1:162){
  #move the first row 
  ##For the horizontal direction, 162 rows, 246.2553m, so the distance between each row is about 1.52-1.53m
  ##For the vertical direction, 162 rows, 7.67
  xym_south <- cbind(geom(block_1)[,5]+1.525*(i-1)-1.05, geom(block_1)[,6]-84.99-0.048*(i-1))
  
  polygon_south <- Polygon(xym_south)
  
  row_south_list[[i]] <- Polygons(list(polygon_south), ID = paste("row",i, sep = "_"))
}
row_south_polys <- SpatialPolygons(row_south_list, proj4string = crs(block_1))
row_south_polys_df <- SpatialPolygonsDataFrame(row_south_polys, data.frame(id = 1:162, row.names = names(row_polys), NS = "south"))



plot(tomato_ndvi_061618, axes = FALSE, box = FALSE, legend = FALSE)
plot(row_north_polys_df, add = TRUE)
plot(row_south_polys_df, add = TRUE)


#Correspond the row name with the plot number
##For south part
for (i in 1:40){
  row_south_polys_df@data$block[1:2+4*(i-1)] <- "buffer"
  row_south_polys_df@data$block[3:4+4*(i-1)] <- i
  row_south_polys_df@data$block[161:162] <- "buffer"
  row_south_polys_df@data$block[6:7] <- 2
  row_south_polys_df@data$block[8] <- "buffer"
}
##For north part
for (i in 1:40){
  row_north_polys_df@data$block[1:2+4*(i-1)] <- "buffer"
  row_north_polys_df@data$block[3:4+4*(i-1)] <- 81-i
  row_north_polys_df@data$block[161:162] <- "buffer"
}

#write the shapefiles
shapefile(row_north_polys_df, file.path(path_shapefiles, "north_rows_061618.shp"), overwrite = TRUE)
shapefile(row_south_polys_df, file.path(path_shapefiles, "south_rows_061618.shp"), overwrite = TRUE)
