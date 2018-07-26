library(spectrolab)


#Set path (the folder that stores .sig files) 
path_svc <- "F:/Tomato biomass/Ground_measurement/BIOSTIMULANT FIELD SCREENING Data/SVC/DATA/0613142018"
#It will read all sig files 
spectra <- read_spectra(path_svc, format = "sig")
#Get the reflectance matrix
reflect <- reflectance(spectra)
#Get the wavelength vector
wavelen <- wavelengths(spectra)
#pri related wavelength
wavelen[135]
wavelen[162]

#Get the names 
name <- names(spectra)
#Create a big data frmae
df <- data.frame("name"= rep(name, each = length(wavelen)), 
                 "wavelength" = rep(wavelen, length(name)),
                 "reflectance" = as.vector(t(reflect)))
#output the big data frame 
write.csv(df, file.path(path_svc,"df_svc.csv"))

#split by the name 
list_name <- split(df, df$name)
#create a new df to store pri data
df_pri <- data.frame(matrix(0,nrow = 160, ncol = 4))
colnames(df_pri) <- c("name","p532","p570","pri")
df_pri$name <- names(list_name)
for(i in 1:length(list_name)){
  df_pri$p532[i] <- list_name[[i]]$reflectance[135]
  df_pri$p570[i] <- list_name[[i]]$reflectance[162]
  df_pri$pri[i] <- with(df_pri, (p532[i]-p570[i])/(p532[i]+p570[i]))
}
