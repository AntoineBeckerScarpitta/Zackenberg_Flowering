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
mod_full_z_cross_int <- lmer(log_flow ~  0 +
                               Species : Temp_summer +
                               Species : lag_Temp_fall + 
                               Species : snowmelt_DOY +
                               Species : log_lag_flow + #forme autoregressive
                               (1|Plot) + (1|Plot:Year),
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
mod_full_n_cross_int <- lmer(log_flow ~   0 + 
                               Species : Temp_summer + 
                               Species : lag_Temp_fall +
                               Species : snowmelt_DOY + 
                               Species : log_lag_flow + #forme autoregressive
                               (1|Plot) + (1|Plot:Year),
                             data=datN, 
                             REML=T, 
                             na.action=na.omit)
#------------------------------------------------




# 3 - Table model output
# full mod Zack + Nuuk
tab_model(mod_full_z_cross_int, mod_full_n_cross_int,
          p.val = "kr", 
          show.df = TRUE, 
          dv.labels = c("Full int Zack", "Full int Nuuk"))
#------------------------------------------------



# 4 - Graphs Forest plot 
# FOREST PLOT 
# High Arctic Zackenberg
FP_zackInt <- sjPlot::plot_model(mod_full_z_cross_int, 
                                 type = "est",
                                 show.values=T, 
                                 show.intercept = FALSE,
                                 show.p = TRUE,
                                 vline.color="grey", 
                                 value.offset=-0.3, 
                                 title = "Forest plot Zackenberg sans 0") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10, face="bold"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", color="black")) 

# Low Arctic - Nuuk
FP_NuukInt <- sjPlot::plot_model(mod_full_n_cross_int, 
                                 type = "est",
                                 show.values=T, 
                                 show.intercept = FALSE,
                                 show.p = TRUE,
                                 vline.color="grey", 
                                 value.offset=-0.3, 
                                 title = "Forest plot Nuuk - sans 0") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10, face="bold"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", color="black"))

#panel
gridExtra::grid.arrange(FP_zackInt, FP_NuukInt, ncol=2)





