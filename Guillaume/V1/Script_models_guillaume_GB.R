
rm(list=ls())

library(sjPlot)
library(ggplot2)
library(lme4)

#SUMMARY
# 1 - load and rund mod for the high Arctic (Zackenberg)
# 2 - load and rund mod for the low Arctic (Nuuk)
# 3 - Table model output
# 4 - Forest plot on model estimates


# HIGH ARCTIC - Zackenberg
# load data
flow_snow_clim_z <- read.csv("Guillaume/flow_snow_clim_z.csv", 
                             stringsAsFactors=FALSE, header=TRUE,  sep=";", 
                             dec=',', strip.white = T,na.strings = c("","NA"))

# Reorganise data a little
datZ <- flow_snow_clim_z

# Make all 0s count of flowers NAs
datZ$log_flow[which(datZ$log_flow == min(datZ$log_flow))] <- NA

# Scale explanatory variable except for lag log_flow
datZ$snowmelt_DOY <- scale(datZ$snowmelt_DOY)
datZ$Temp_summer <- scale(datZ$Temp_summer)
datZ$lag_Temp_fall <- scale(datZ$lag_Temp_fall)

# Make all 0s count of flowers NAs
datZ$log_lag_flow[which(datZ$log_lag_flow == min(datZ$log_lag_flow))] <- NA

## ___________ Code to remove later
# Check where the 0s are
for(i in 1:length(unique(datZ$Plot))){
  coco <- datZ[datZ$Plot == unique(datZ$Plot)[i], c(3,6)]
  print(c(i , coco$Year[which(is.na(coco$log_flow))]))
}
##____________


#  MODEL EQ3 - Full model
mod_full_z_cross <- lmer(log_flow ~  0 +
                           Species * Temp_summer +
                           Species * lag_Temp_fall +
                           Species * snowmelt_DOY +
                           Species * log_lag_flow +
                           (1|Plot) + (1|Plot:Year),
                         data=datZ,
                         REML=TRUE, 
                         na.action=na.omit)

#  MODEL EQ3 - Interaction model
mod_full_z_cross_int <- lmer(log_flow ~  0 +
                           Species : Temp_summer +
                           Species : lag_Temp_fall +
                           Species : snowmelt_DOY +
                           Species : log_lag_flow +
                           (1|Plot) + (1|Plot:Year),
                         data=datZ,
                         REML=TRUE, 
                         na.action=na.omit)


# FOREST PLOT 
# High Arctic Zackenberg

# Full model
FP_zack <- sjPlot::plot_model(mod_full_z_cross, 
                              type = "est",
                              show.values=T, 
                              show.intercept = FALSE,
                              show.p = TRUE,
                              vline.color="grey", 
                              value.offset=-0.4, 
                              title = "Zack Full") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10, face="bold"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", color="black")) 

# Interaction model
FP_zackInt <- sjPlot::plot_model(mod_full_z_cross_int, 
                                 type = "est",
                                 show.values=T, 
                                 show.intercept = FALSE,
                                 show.p = TRUE,
                                 vline.color="grey", 
                                 value.offset=-0.4, 
                                 title = "Zack Interaction") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10, face="bold"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", color="black")) 

gridExtra::grid.arrange(FP_zack, FP_zackInt, ncol=2)


# summary(mod_full_z_cross)
##----------------------------------------------------------------------------





# LOW ARCTIC - Nuuk
# load data
flow_snow_clim_n <- read.csv("Guillaume/flow_snow_clim_n.csv", 
                             stringsAsFactors=FALSE, header=TRUE,  sep=";", 
                             dec=",", strip.white = T,na.strings = c("","NA"))

# Reorganise data a little
datN <- flow_snow_clim_n

# Make all 0s count of flowers NAs
datN$log_flow[which(datN$log_flow == min(datN$log_flow))] <- NA

# Scale explanatory variable except for lag log_flow
datN$snowmelt_DOY <- scale(datN$snowmelt_DOY)
datN$Temp_summer <- scale(datN$Temp_summer)
datN$lag_Temp_fall <- scale(datN$lag_Temp_fall)

# Make all 0s count of flowers NAs
datN$log_lag_flow[which(datN$log_lag_flow == min(datN$log_lag_flow))] <- NA

## ___________ Code to remove later
# Check where the 0s are
for(i in 1:length(unique(datZ$Plot))){
  coco <- datZ[datZ$Plot == unique(datZ$Plot)[i], c(3,6)]
  print(c(i , coco$Year[which(is.na(coco$log_flow))]))
}
##____________


#  MODEL EQ3 - Full model
mod_full_n_cross <- lmer(log_flow ~ 0 +
                           Species * Temp_summer +
                           Species * lag_Temp_fall +
                           Species * snowmelt_DOY +
                           Species * log_lag_flow +
                           (1|Plot) + (1|Plot:Year),
                         data=datN,
                         REML=T, 
                         na.action=na.omit)

#  MODEL EQ3 -  Only interactions
mod_full_n_cross_int <- lmer(log_flow ~   0 + 
                               Species : Temp_summer + 
                               Species : lag_Temp_fall +
                               Species : snowmelt_DOY + 
                               Species : log_lag_flow + 
                               (1|Plot) + (1|Plot:Year),
                             data=datN, 
                             REML=T, 
                             na.action=na.omit)


# FOREST PLOT 
# Low Arctic Nuuk

# Full model
FP_Nuuk <- sjPlot::plot_model(mod_full_n_cross, 
                              type = "est",
                              show.values=T, 
                              show.intercept = FALSE,
                              show.p = TRUE,
                              vline.color="grey", 
                              value.offset=-0.4, 
                              title = "Nuuk Full") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10, face="bold"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", color="black")) 

# Interaction model
FP_NuukInt <- sjPlot::plot_model(mod_full_n_cross_int, 
                                 # type = "est",
                                 show.values=T, 
                                 show.intercept = FALSE,
                                 show.p = TRUE,
                                 vline.color="grey", 
                                 value.offset=-0.4, 
                                 title = "Nuuk Interaction") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10, face="bold"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", color="black")) 

gridExtra::grid.arrange(FP_Nuuk, FP_NuukInt, ncol=2)


# summary(mod_full_n_cross)
##----------------------------------------------------------------------------








# MODEL TABLES
#Tabl model output
tab_model(mod_full_z_cross_int, mod_full_n_cross_int,
          p.val = "kr", 
          show.df = TRUE, 
          dv.labels = c("Full cross Zack", "Full cross Nuuk"))


# FOREST PLOT 
# High Arctic Zackenberg

FP_zackInt <- sjPlot::plot_model(mod_full_z_cross_int, 
                              type = "est",
                              show.values=T, 
                              show.intercept = FALSE,
                              show.p = TRUE,
                              vline.color="grey", 
                              value.offset=-0.4, 
                              title = "Forest plot Zackenberg") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10, face="bold"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", color="black")) 

gridExtra::grid.arrange(FP_zack, FP_nuuk, ncol=2)

# Low Arctic - Nuuk
FP_NuukInt <- sjPlot::plot_model(mod_full_n_cross_int, 
                              type = "est",
                              show.values=T, 
                              show.intercept = FALSE,
                              show.p = TRUE,
                              vline.color="grey", 
                              value.offset=-0.4, 
                              title = "Forest plot Nuuk") +
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
