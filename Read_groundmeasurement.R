library(dplyr)
library(Hmisc)

#Read ground measurement data 
path_gm <- "F:/Tomato biomass/Ground_measurement/BIOSTIMULANT FIELD SCREENING Data"
gm_061318 <- read.csv(file.path(path_gm, "061318.csv"))

unique(gm_061318$Product)
as.numeric(gm_061318$Product)

#assign new name
#gm_061318$prod <- c("con_r","com_el","com_res","cal","redox","com_er","wis","con_nr","cosm_k","oat_05","agr","bayer","oat_06","val","cosm_x","oat_10")
gm_061318_noncon <- gm_061318[c(-1,-8),]

#Read uav data
path_df <- "F:/Tomato biomass/Processed_results/dataframes"
blockmean_uav_061618 <- read.csv(file.path(path_df, "blockmean_uav_061618.csv"))
#remove buffers
blockmean_uav_061618_nonbuf <- subset(blockmean_uav_061618, blockmean_uav_061618$block!="buffer")
blockmean_uav_061618_nonbuf$block

#read the prod_block file, which correspond product abbreviation with block number
prod_block <- read.csv(file.path(path_df, "prod_block.csv"))
str(prod_block)
prod_block_noncon <- subset(prod_block,prod_block$prod!="con")

#merge two data frames
prodmean_uav_061618 <- merge(blockmean_uav_061618_nonbuf, prod_block, by = "block")
prodmean_uav_061618 <- merge(blockmean_uav_061618_nonbuf, prod_block_noncon, by = "block")
#calculate product average
prodmean_uav_061618 <- aggregate(prodmean_uav_061618[2:12], list(prodmean_uav_061618$prod), mean)
names(prodmean_uav_061618)[1] <- "prod"
#merge with the ground measurement 
prod_gmuav_061618 <- merge(prodmean_uav_061618, gm_061318, by = "prod")
prod_gmuav_061618[c("Product","Block")] <- NULL
str(prod_gmuav_061618)


#----------------------------------------------#
##calculate some correlation with Biomass
plot(prod_gmuav_061618$DW, prod_gmuav_061618)
cor(prod_gmuav_061618$DW, prod_gmuav_061618$ndvi)
rcorr(as.matrix(prod_gmuav_061618[,2:20]))
rcorr(as.matrix(prod_gmuav_061618[,c(7,10,11,13:20)]))


