library(ggplot2)
#Set path
path_gs <- "F:/Tomato biomass/Ground_measurement/Ground_sensor"


prod_gs <- function(path_gs, file_gs, prod){
  library(lubridate)
  prod_gs <- read.csv(file.path(path_gs, file_gs), header = FALSE, na.strings = "#N/A", skip = 3)[,c(1,2,3,6,7,10,11,14,15,16,19:24)]
  header_gs <- c("Timestamp","Up532","Up570","Up650","Up810","Down650","Down810","NDVI","Down532","Down570","PRI","Targ_temp","Bod_temp","Soil_moist","Soil_temp","Soil_ec")
  colnames(prod_gs) <- header_gs
  
  #calculate pri index
  pri <- function(u532,u570,d532,d570){
    p570 <- d570/u570
    p532 <- d532/u532
    pri <- (p532-p570)/(p532+p570)
    return(pri)
  }
  prod_gs$PRI <- pri(prod_gs$Up532, prod_gs$Up570, prod_gs$Down532, prod_gs$Down570)
  #calculate ndvi index
  ndvi <- function(u810,u650,d810,d650){
    p650 <- d650/u650
    p810 <- d810/u810
    ndvi <- (p810-p650)/(p810+p650)
    return(ndvi)
  }
  prod_gs$NDVI <- ndvi(prod_gs$Up810, prod_gs$Up650, prod_gs$Down810, prod_gs$Down650)

  #Change timestamp to standard format
  prod_gs$Timestamp <- strptime(as.character(prod_gs$Timestamp), "%m/%d/%Y %H:%M")
  #add date
  prod_gs$Date <- lubridate::date(prod_gs$Timestamp)
  #add time 
  prod_gs$Time <- lubridate::hour(prod_gs$Timestamp)+lubridate::minute(prod_gs$Timestamp)/60
  
  #remove NAs
  prod_gs <- na.omit(prod_gs)
  
  #add the product name 
  prod_gs$prod <- as.factor(prod)
  
  return(prod_gs)
}


#Read files
prod_gs_1 <- prod_gs(path_gs, "block1_css_061318_061618.csv","cal")
prod_gs_2 <- prod_gs(path_gs, "block2_con_061318_061618.csv","con_r")
prod_gs_3 <- prod_gs(path_gs, "block3_wis_061318_061618.csv","wis")
prod_gs_4 <- prod_gs(path_gs, "block4_comer_061318_061618.csv","com_er")
prod_gs_5 <- prod_gs(path_gs, "block5_redox_061318_061618.csv","redox")
prod_gs_6 <- prod_gs(path_gs, "block6_bayer_061318_061618.csv","bayer")
prod_gs_7 <- prod_gs(path_gs, "block7_val_061318_061618.csv","val")
prod_gs_8 <- prod_gs(path_gs, "block8_agr_061318_061618.csv","agr")
prod_gs_9 <- prod_gs(path_gs, "block9_oat10_061318_061618.csv","oat_10")
prod_gs_10 <- prod_gs(path_gs, "block10_cosm_061318_061618.csv","cosm_k")
#combine 10 data frames
prod_gs_061618 <- rbind(prod_gs_1, prod_gs_2, prod_gs_3, prod_gs_4, prod_gs_5, prod_gs_6, prod_gs_7, prod_gs_8, prod_gs_9, prod_gs_10)

#-----------------------------------------------------#
#Read weather data 
weather_gs <- function(path_gs, file_gs){
  library(lubridate)
  #subset some columns 
  weather_gs <- read.csv(file.path(path_gs,file_gs), header = FALSE, na.strings = "#/A", skip = 3)[,c(1:3,7:11,16)]
  #change column names
  colnames(weather_gs) <- c("Timestamp","SolarR","Precip","Wind","Gust","Air_temp","VP","AP","VPD")
  #Change timestamp to standard format
  weather_gs$Timestamp <- strptime(as.character(weather_gs$Timestamp), "%m/%d/%Y %H:%M")
  #add date
  weather_gs$Date <- lubridate::date(weather_gs$Timestamp)
  #add time 
  weather_gs$Time <- lubridate::hour(weather_gs$Timestamp)+lubridate::minute(weather_gs$Timestamp)/60
  #remove NAs
  weather_gs <- na.omit(weather_gs)
  return(weather_gs)
}
weather_061618 <- weather_gs(path_gs, "weather_061318_061618.csv")

#------------------------------------------------#
#Date and time selection function 
time_select <- function(df, begin, end, early, late){
  #df is the data frame that stores the early 
  #begin & end is the begin date and end date
  #early & late is the time range 
  df_date_select <- df[df$Date>=begin & df$Date <= end,]
  df_select <- df_date_select[df_date_select$Time>= early & df_date_select$Time <= late,]
  return(df_select)
}


#--------------------------------------------------#
#time series 
#PRI from 9am to 6pm
##06-13
ggplot(time_select(prod_gs_061618, "2018-06-13","2018-06-13",9,18), aes(Timestamp, PRI, color = prod))+geom_line() + ggtitle("PRI diurnal cycle on 06-13") 
##06-14
ggplot(time_select(prod_gs_061618, "2018-06-14","2018-06-14",9,18), aes(Timestamp, PRI, color = prod))+geom_line() + ggtitle("PRI diurnal cycle on 06-14") 
##06-15
ggplot(time_select(prod_gs_061618, "2018-06-15","2018-06-15",9,18), aes(Timestamp, PRI, color = prod))+geom_line() + ggtitle("PRI diurnal cycle on 06-15") 
##06-16
ggplot(time_select(prod_gs_061618, "2018-06-16","2018-06-16",9,18), aes(Timestamp, PRI, color = prod))+geom_line() + ggtitle("PRI diurnal cycle on 06-16") 

ggplot(time_select(prod_gs_061618, "2018-06-13","2018-06-13",9,18), aes(Timestamp, NDVI, color = prod))+geom_line() + ggtitle("NDVI diurnal cycle on 06-13") 


#-----------------------------------------------------------------#
#calculate daily average
gs_dailyave <- function(df, early, late){
  ##subset the time 
  df_select <- df[df$Time>=early & df$Time<= late,]
  #aggregate with product name and date 
  df_aggregate <- aggregate(x = df_select[,c("PRI","NDVI","Targ_temp","Soil_moist","Soil_temp","Soil_ec")], by = list(Date = df_select$Date, prod = df_select$prod), FUN = mean)
  return(df_aggregate)
}

gs_dailyave_061618 <- gs_dailyave(df = prod_gs_061618, early = 11, late = 15)
ggplot(gs_dailyave_061618, aes(Date, NDVI, color = prod)) + geom_line() + geom_point() + ggtitle("NDVI daily average between 06-13 to 06-16")
ggplot(gs_dailyave_061618, aes(Date, PRI, color = prod)) + geom_line() + geom_point() + ggtitle("PRI daily average between 06-13 to 06-16")
ggplot(gs_dailyave_061618, aes(Date, Targ_temp, color = prod)) + geom_line() + geom_point() + ggtitle("Canopy temperature daily average between 06-13 to 06-16")


#---------------------------------------------------------------------#
#Combine weather data to the ground sensor data 
weather_dailyave <- function(df, early, late){
  df_select <- df[df$Time>=early & df$Time<= late,]
  df_aggregate <- aggregate(x = df_select[,2:9], by = list(Date = df_select$Date), FUN = mean)
  return(df_aggregate)
}
weather_dailyave_061618 <- weather_dailyave(df = weather_061618, early = 12, late = 14)
weather_dailyave_061618

#merge weather data and sensor data
gs_061618 <- merge(gs_dailyave_061618, weather_dailyave_061618, by = "Date")
#merge weather data, sensor data, available ground measurement and aerial image data 
gs_gmuav_061618 <- merge(subset(gs_061618, gs_061618$Date == "2018-06-16"), prod_gmuav_061618, by = "prod")
#change name 