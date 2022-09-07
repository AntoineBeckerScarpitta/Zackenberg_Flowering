##############################################################################
#
#                           Greenland Flowering project
#                                 Guillaume script
#
##############################################################################
# Antoine Becker-Scarpitta & Guillaume Blanchet
# June 2022



# NOTE:
# Dans cette version de modél, la forme 'lag' est en fait la densité de lannée
# précédente. Pour la calculwe jai donc fait: 
# lag_Flow_m2=lag(Flow_m2, order_by = Year)
# Si on veut faire une forme plus formelle type autoregressive, jimagine
# que les données qui doivent être utilisé sont les données 'brutes': log_flow



# CLean space
rm(list=ls())

# lib
library(sjPlot)
library(lme4)


#------------------------------------------------
#SUMMARY
# 1 - load and rund mod for the high Arctic (Zackenberg)
# 2 - load and rund mod for the low Arctic (Nuuk)
# 3 - Table model output
# 4 - Graphs Forest plot + Species level 
#------------------------------------------------


# 1 - HIGH ARCTIC - Zackenberg
# load data
flow_snow_clim_z <- read.csv("Guillaume/V2/flow_snow_clim_z.csv", 
                             stringsAsFactors=FALSE, header=TRUE,  sep=";", 
                             dec=',', strip.white = T,na.strings = c("","NA"))

# Reorganise data
datZ <- flow_snow_clim_z

# Scale explanatory variable except for lag log_flow
datZ$snowmelt_DOY <- scale(datZ$snowmelt_DOY)
datZ$Temp_summer <- scale(datZ$Temp_summer)
datZ$lag_Temp_fall <- scale(datZ$lag_Temp_fall)


#  MODEL EQ3 - Interaction model
mod_full_z <- lmer(log_flow ~  0 +
                               Species : Temp_summer +
                               Species : lag_Temp_fall +
                               Species : snowmelt_DOY +
                               Species : log_lag_flow + offset(1 * log_lag_flow) +
                               (1|Plot:Year),
                             data=datZ,
                             REML=TRUE, 
                             na.action=na.omit)

#------------------------------------------------




# 2 - LOW ARCTIC - Nuuk
# load data
flow_snow_clim_n <- read.csv("Guillaume/V2/flow_snow_clim_n.csv", 
         stringsAsFactors=FALSE, header=TRUE,  sep=";", 
         dec=',', strip.white = T,na.strings = c("","NA"))

# Reorganise data a little
datN <- flow_snow_clim_n

# Scale explanatory variable except for lag log_flow
datN$snowmelt_DOY <- scale(datN$snowmelt_DOY)
datN$Temp_summer <- scale(datN$Temp_summer)
datN$lag_Temp_fall <- scale(datN$lag_Temp_fall)


#  MODEL EQ3 -  Only interactions
mod_full_n <- lmer(log_flow ~   0 + 
                               Species : Temp_summer + 
                               Species : lag_Temp_fall +
                               Species : snowmelt_DOY + 
                               Species : log_lag_flow + offset(1 * log_lag_flow) + 
                               (1|Plot:Year),
                             data=datN, 
                             REML=T, 
                             na.action=na.omit)
#------------------------------------------------




# 3 - Table model output
# full mod Zack + Nuuk
tab_model(mod_full_z, mod_full_n,
          p.val = "kr", 
          show.df = TRUE, 
          dv.labels = c("Full int Zack", "Full int Nuuk"))


# 1 - corrected value of the Offset
# extract value
intZ <- summary(mod_full_z)$coefficients
intN <- summary(mod_full_n)$coefficients

#correct offet: -beta
intZ[grep("log_lag_flow",rownames(intZ)),1] <- -intZ[grep("log_lag_flow",rownames(intZ)),1]
intN[grep("log_lag_flow",rownames(intN)),1] <- -intN[grep("log_lag_flow",rownames(intN)),1]


# 2- Pvalue and distribution offset
#NUUK
paramFix_n <- summary(mod_full_n)$coefficients
pointerLag_n <- grep("log_lag_flow", rownames(paramFix_n))

as.data.frame(pt(q = abs((1 - paramFix_n[pointerLag_n,1])/paramFix_n[pointerLag_n,2]), 
                 df =  paramFix_n[pointerLag_n,3], lower.tail = FALSE) * 2)

#ZACK
paramFix_z <- summary(mod_full_z)$coefficients
pointerLag_z <- grep("log_lag_flow", rownames(paramFix_z))

as.data.frame(pt(q = abs((1 - paramFix_z[pointerLag_z,1])/paramFix_z[pointerLag_z,2]), 
                 df =  paramFix_z[pointerLag_z,3], lower.tail = FALSE) * 2)
##----------------------------------------------------------------------------

