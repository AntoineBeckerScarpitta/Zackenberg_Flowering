
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
flow_snow_clim_z <- read.csv("data/datasets/flow_snow_clim_z.csv", 
                             stringsAsFactors=FALSE, header=TRUE,  sep=";", 
                             dec=',', strip.white = T,na.strings = c("","NA"))

#  MODEL EQ3 - Full model
mod_full_z_cross <- lmer(log_flow ~  0 +
                           Species * Temp_summer +
                           Species * lag_Temp_fall +
                           Species * snowmelt_DOY +
                           Species * log_lag_flow +
                           (1|Plot) + (1|Plot:Year),
                         data=flow_snow_clim_z,
                         REML=T, na.action=na.omit)

# MODEL EQ3 - Only interactions
mod_full_z_cross_int <- lmer(log_flow ~ 0 + 
                               Species : Temp_summer + 
                               Species : lag_Temp_fall +
                               Species : snowmelt_DOY + 
                               Species : log_lag_flow + 
                               (1|Plot) + (1|Year),
                             data=flow_snow_clim_z, 
                             REML=T, na.action=na.omit)
# summary(mod_full_z_cross)
##----------------------------------------------------------------------------



# LOW ARCTIC - Nuuk
# load data
flow_snow_clim_n <- read.csv("data/datasets/flow_snow_clim_n.csv", 
                             stringsAsFactors=FALSE, header=TRUE,  sep=";", 
                             dec=",", strip.white = T,na.strings = c("","NA"))

#  MODEL EQ3 - Full model
mod_full_n_cross <- lmer(log_flow ~ 0 +
                           Species * Temp_summer +
                           Species * lag_Temp_fall +
                           Species * snowmelt_DOY +
                           Species * log_lag_flow +
                           (1|Plot) + (1|Year),
                         data=flow_snow_clim_n,
                         REML=T, na.action=na.omit)

#  MODEL EQ3 -  Only interactions
mod_full_n_cross_int <- lmer(log_flow ~   0 + 
                               Species : Temp_summer + 
                               Species : lag_Temp_fall +
                               Species : snowmelt_DOY + 
                               Species : log_lag_flow + 
                               (1|Plot) + (1|Year),
                             data=flow_snow_clim_n, 
                             REML=T, na.action=na.omit)
# summary(mod_full_n_cross)
##----------------------------------------------------------------------------


# MODEL TABLE
#Tabl model output
tab_model(mod_full_z_cross_int, mod_full_n_cross_int,
          p.val = "kr", 
          show.df = TRUE, 
          dv.labels = c("Full cross Zack", "Full cross Nuuk"))


# FOREST PLOT 
# High Arctic Zackenberg
FP_zack <- sjPlot::plot_model(mod_full_z_cross, 
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


# Low Arctic - Nuuk
FP_nuuk <- sjPlot::plot_model(mod_full_n_cross, 
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
gridExtra::grid.arrange(FP_zack, FP_nuuk, ncol=2)
