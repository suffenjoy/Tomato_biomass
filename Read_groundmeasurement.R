library(dplyr)
library(Hmisc)

#Read ground measurement data 
path_gm <- "F:/Tomato biomass/Ground_measurement/BIOSTIMULANT FIELD SCREENING Data"
gm_061318 <- read.csv(file.path(path_gm, "061318.csv"))

unique(gm_061318$Product)
as.numeric(gm_061318$Product)

#assign new name
gm_061318$prod <- as.factor(c("con_r","com_el","com_res","cal","redox","com_er","wis","con_nr","cosm_k","oat_05","agr","bayer","oat_06","val","cosm_x","oat_10"))

#Read uav data
path_df <- "F:/Tomato biomass/Processed_results/dataframes"
blockmean_uav_061618 <- read.csv(file.path(path_df, "blockmean_uav_061618.csv"))
#remove buffers
blockmean_uav_061618_nonbuf <- subset(blockmean_uav_061618, blockmean_uav_061618$block!="buffer")
blockmean_uav_061618_nonbuf$block

#read the prod_block file, which correspond product abbreviation with block number
prod_block <- read.csv(file.path(path_df, "prod_block.csv"))
str(prod_block)

#merge two data frames
prodmean_uav_061618 <- merge(blockmean_uav_061618_nonbuf, prod_block, by = "block")
#calculate product average
prodmean_uav_061618 <- aggregate(prodmean_uav_061618[2:12], list(prod=prodmean_uav_061618$prod), mean)
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

plot(x = prod_gmuav_061618$ndre, y = prod_gmuav_061618$DW, xlab = "NDRE", ylab = "Dry weight")
summary(lm(DW~ndre + gndvi, data= prod_gmuav_061618))
summary(lm(DW~ndre , data= prod_gmuav_061618))

