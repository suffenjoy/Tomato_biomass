library(raster)

path_shp <- "E:/tomato_analysis_2019/tomato_GCPs"
all <- shapefile(file.path(path_shp, "tomato_GCPs_2019.shp"))

#different types 
type <- unlist(lapply(strsplit(all$Comment, split = " "),'[',1))


all$type = type

#divide the shapefiles with different types
##gcp and pipes
gcps <- all[all$type=="pipe" | all$type == "gcp", ]
##flags 
flags <- all[all$type == "flag",]
##sensors
sensors <- all[all$type == "sensor",]


#add number of block 
path_map <- "E:/tomato_analysis_2019/map"
flags_info <- read.csv(file.path(path_map, "flags_tomato_2019.csv"))
names(flags_info) <- c("Side", "Block", "Row", "Plot", "Treatment")
unique(flags_info$Treatment)

flags$id <- as.numeric(sapply(strsplit(flags$Comment, " "), '[', 2))

flags$id <- 120:1
flags <- flags[order(flags$id), ]

flags$Treatment <- rep(flags_info$Treatment, each = 8)
flags$Side <- rep(flags_info$Side, each = 8)
flags$Block <- rep(flags_info$Block, each = 8)
flags$Row <- rep(flags_info$Row, each = 8)
flags$Plot <- rep(flags_info$Plot, each  = 8)
flags$gcp_flag <- NULL
flags$flag <- NULL
flags$gcp <- NULL
flags$sensor <- NULL

plot(flags[flags$Treatment == "CONTROL DDD",]) #3 in the west, #2 in the east
plot(flags[flags$Treatment == "CONTROL WW",]) #2 in the west, #3 in the east
plot(flags[flags$Treatment == "BAYER 16 (SOIL) WW",]) #3 in the west, #2 in the east


#export to multiple shapefiles
shapefile(flags,file.path(path_shp, "flags_tomato_2019.shp"), overwrite = TRUE)

shapefile(gcps,file.path(path_shp, "gcps_tomato_2019.shp"))
shapefile(sensors,file.path(path_shp, "sensors_tomato_2019.shp"))


#export the gcp file to the Pix4D GCP format

#-----------------------------------------------------------------------------#

#create shapefiles of the rows 
##read the first row
#path_row <- "E:/tomato_analysis_2019/shapefiles"

##read the row design map
rows_design_2019 <- read.csv(file.path(path_map, "rows_tomato_2019.csv"))
rows_design_west <- rows_design_2019[,1:4]
rows_design_east <- rows_design_2019[,5:8]
names(rows_design_east) <- c("Row", "Block", "Treatment", "Side")


##west 
row_west_list <- list()
for(i in 1:207){
  xym_west <- cbind(geom(row1_west_center_rect)[,5]-0.012*(i-1), geom(row1_west_center_rect)[,6]-1.5225*(i-1))
  polygon_west <- Polygon(xym_west)
  row_west_list[[i]] <- Polygons(list(polygon_west), ID  = paste("row", i, sep = "_"))
  
}

row_west_polys <- SpatialPolygons(row_west_list, proj4string = crs(row1_west_center_rect))
row_west_polys_df <- SpatialPolygonsDataFrame(row_west_polys, data.frame(Row  = 1:207, row.names = names(row_west_polys), Side = "West"))
#merge the treatment and block information 
row_west_polys_df <- merge(row_west_polys_df, rows_design_west, by = c("Row", "Side"))


#Write the shapefiles
shapefile(row_west_polys_df, file.path(path_row, "west_rows_2019.shp"), overwrite = TRUE)


##east 
row_east_list <- list()
for(i in 1:207){
  xym_east <- cbind(geom(row1_east_center_rect)[,5]-0.012*(i-1), geom(row1_east_center_rect)[,6]-1.5229*(i-1))
  polygon_east <- Polygon(xym_east)
  row_east_list[[i]] <- Polygons(list(polygon_east), ID  = paste("row", i, sep = "_"))
  
}
#convert to spatialpolygondataframe
row_east_polys <- SpatialPolygons(row_east_list, proj4string = crs(row1_east_center_rect))
row_east_polys_df <- SpatialPolygonsDataFrame(row_east_polys, data.frame(Row  = 1:207, row.names = names(row_east_polys), Side = "East"))
#merge the treatment and block information 
row_east_polys_df <- merge(row_east_polys_df, rows_design_east, by = c("Row", "Side"))

#Write the shapefiles
shapefile(row_east_polys_df, file.path(path_row, "east_rows_2019.shp"), overwrite = TRUE)


#combine east and west rows together
row_polys_df <- bind(row_east_polys_df, row_west_polys_df)
plot(row_polys_df)
##write the shapefiles
shapefile(row_polys_df, file.path(path_row, "tomato_rows_2019.shp"), overwrite = TRUE)


#-----------------------------------------------------------------
#Create 1ft width polygons 
path_row <- "Z:/CC_UCD_atRR/data2/UAV_MeeraeTomato/shapefiles"
row1_west_center_rect <- shapefile(file.path(path_row, "row1_west_center_rectangle.shp"))
row1_east_center_rect <- shapefile(file.path(path_row, "row1_east_center_rectangle.shp"))
tomato_rows_2019 <- shapefile(file.path(path_row, "tomato_rows_2019.shp"))

## a for loop 
tomato_rows_2019_cen1ft <- tomato_rows_2019
tomato_rows_2019_cen2ft <- tomato_rows_2019
tomato_rows_2019_cen3ft <- tomato_rows_2019
for(i in 1:length(tomato_rows_2019)){
  #center 1ft 
  extent_1ft <- tomato_rows_2019[i,]@bbox
  extent_1ft[2,1] <- extent_1ft[2,1]+0.3436947
  extent_1ft[2,2] <- extent_1ft[2,2]-0.3436947
  tomato_rows_2019_cen1ft <- rbind(tomato_rows_2019_cen1ft, raster::crop(tomato_rows_2019[i,], extent_1ft))
  
  #center 2ft
  extent_2ft <- tomato_rows_2019[i,]@bbox
  extent_2ft[2,1] <- extent_2ft[2,1]+0.1912947
  extent_2ft[2,2] <- extent_2ft[2,2]-0.1912947
  tomato_rows_2019_cen2ft <- rbind(tomato_rows_2019_cen2ft, raster::crop(tomato_rows_2019[i,], extent_2ft))
  
  #center 3ft
  extent_3ft <- tomato_rows_2019[i,]@bbox
  extent_3ft[2,1] <- extent_3ft[2,1]+0.03889475
  extent_3ft[2,2] <- extent_3ft[2,2]-0.03889475
  tomato_rows_2019_cen3ft <- rbind(tomato_rows_2019_cen3ft, raster::crop(tomato_rows_2019[i,], extent_3ft))
}
#remove the original ones
tomato_rows_2019_cen1ft <- tomato_rows_2019_cen1ft[415:828,]
tomato_rows_2019_cen2ft <- tomato_rows_2019_cen2ft[415:828,]
tomato_rows_2019_cen3ft <- tomato_rows_2019_cen3ft[415:828,]
#output
shapefile(tomato_rows_2019_cen1ft, file.path(path_row, "tomato_rows_2019_cen1ft.shp"))
shapefile(tomato_rows_2019_cen2ft, file.path(path_row, "tomato_rows_2019_cen2ft.shp"))
shapefile(tomato_rows_2019_cen3ft, file.path(path_row, "tomato_rows_2019_cen3ft.shp"))


