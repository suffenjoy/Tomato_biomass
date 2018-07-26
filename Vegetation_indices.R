ndvi <- function(uav){
  red <- uav$red
  nir <- uav$nir
  ndvi <- (nir-red)/(nir+red)
  return(ndvi)
}

ndre <- function(uav){
  nir <- uav$nir
  rededge <- uav$rededge
  ndre <- (nir-rededge)/(nir+rededge)
  return(ndre)
}

gndvi <- function(uav){
  green <- uav$green
  nir <- uav$nir
  gndvi <- (nir-green)/(nir+green)
  return(gndvi)
}
