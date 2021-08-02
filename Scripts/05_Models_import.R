#######################################################################################
#
#                           Greenland Flowering project
#                            05 - Models Exploration
#
#######################################################################################
# Antoine Becker-Scarpitta & Laura Antao
# Febuary 2021

#  clean R work-space
rm(list=ls())


# In this script you have no data, ONLY models outputs
# script to run models are: 04_Analysis_Zackenberg & 04_Analysis_Nuuk
# in these scripts (04_), you will find code line to explore plot effects



# 1 - MODELS STRUCTURE FOR A-ZACKENBERG ; B-NUUK
# A - ZACKENBERG
# mod1_z <- lmer(log(trans_Flow_m2) ~ Species * as.numeric(Year) + lag_trans_Flow_m2 +
#                  (1|Plot),
#                data= flow_snow_clim_z,
#                REML=T, na.action=na.omit)
mod_basic_z <- readRDS('results/Models/mod_basic_z.rds')

# mod2_z <- lmer(log(trans_Flow_m2) ~ Species * Temp_summer + Species * lag_Temp_fall +
#                  Species * snowmelt_DOY  + lag_trans_Flow_m2 + (1|Plot),
#                data=flow_snow_clim_z, 
#                REML=T, na.action=na.omit)
mod_full_z <- readRDS('results/Models/mod_full_z.rds')

# mod3_z <- lmer(log(trans_Flow_m2) ~ Species * Temp_summer + Species * lag_Temp_fall +
#                  Species * snowmelt_DOY  + (1|Plot/Year),
#                data=flow_snow_clim_z, 
#                REML=T, na.action=na.omit)
mod_full_2_z <- readRDS('results/Models/mod_full_ranef_plot_year_z.rds')
# 
# 
# B - NUUK
# mod1_n <- lmer(log(trans_Flow_m2) ~ Species * Year + lag_trans_Flow_m2 +
#                  (1|Plot),
#                data= flow_snow_clim_n,
#                REML=T, na.action=na.omit)
mod_basic_n <- readRDS('results/Models/mod_basic_n.rds')

# #  MODEL 2: 
# mod2_n <- lmer(log(trans_Flow_m2) ~ Species * Temp_summer + 
#                  Species * lag_Temp_fall +
#                  lag_trans_Flow_m2 + (1|Plot),
#                data=flow_snow_clim_n, 
#                REML=T, na.action=na.omit)
mod_full_n <- readRDS('results/Models/mod_full_n.rds')

# #  MODEL 3: ranef plot structured by year
# mod3_n <- lmer(log(trans_Flow_m2) ~ Species * Temp_summer + 
#                  Species * lag_Temp_fall +
#                  (1|Plot/Year),
#                data=flow_snow_clim_n, 
#                REML=T, na.action=na.omit)
mod_full_2_n <- readRDS('results/Models/mod_full_ranef_plot_year_n.rds')




