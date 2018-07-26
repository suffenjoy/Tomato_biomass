#stack 
read_uav <- function(path, red, green, blue, rededge, nir, ndvi, thermal, dsm, crop){
  #extent
  uav_crop <- raster(file.path(path,crop))
  #read raster layer
  uav_red <- crop(raster(file.path(path, red)), uav_crop)
  uav_green <- crop(raster(file.path(path, green)),uav_crop)
  uav_blue <- crop(raster(file.path(path, blue)), uav_crop)
  uav_rededge <- crop(raster(file.path(path, rededge)), uav_crop)
  uav_nir <- crop(raster(file.path(path, nir)), uav_crop)
  uav_ndvi <- crop(raster(file.path(path, ndvi)), uav_crop)
  uav_thermal <- crop(raster(file.path(path, thermal)), uav_crop)
  uav_dsm <- crop(raster(file.path(path, dsm)), uav_crop)
  #uav_dtm <- crop(raster(file.path(path,dtm)), uav_crop)
  
  
  
  #brick all rasters
  uav_brick <- brick(uav_blue, uav_green, uav_red, uav_rededge, uav_nir, uav_ndvi, uav_thermal, uav_dsm)
  
  #rename the bands
  names(uav_brick) <- c("blue","green","red","rededge","nir","ndvi","therm","dsm")
  
  return(uav_brick)
}