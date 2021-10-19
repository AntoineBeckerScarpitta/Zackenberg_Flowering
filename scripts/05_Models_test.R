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

# Library
source("Scripts/00_Load_libraries.r")

#SCRIPT SUMMARY
# In this script no data, ONLY models outputs
# script to run models: 04_Analysis_Zackenberg & 04_Analysis_Nuuk

#1 - LOAD MODELS
#   1.A - Zackenberg
#   1.B - Nuuk
#2 - MODELS TABS
#   2.1 - basic mod Zack + Nuuk
#   2.2 - full mod Zack + Nuuk
#   2.3 - ICC
#3 - MODELS PLOTS
#   3.A - Zackenberg
#   3.B - Nuuk


# 1 - LOAD MODELS -------------------------------------------------------------------
# Run mods
source("Scripts/04_Analysis_Zack.r")
source("Scripts/04_Analysis_Nuuk.r")

# clean R object 
remove("clim_season_year", "temp_clim_z", "temp_clim_n", "flow_snow_z", 
       "flow_snow_n", "snow", "flow")


# # 1.A - Zackenberg
# mod_basic_z <- readRDS('results/Models/mod_basic_z.rds')
# mod_full_z_cross <- readRDS('results/Models/mod_full_z_cross.rds')
# # mod_full_z_nest <- readRDS('results/Models/mod_full_z_nest.rds')
# 
# # 1.B - Nuuk
# mod_basic_n <- readRDS('results/Models/mod_basic_n.rds')
# mod_full_n_cross <- readRDS('results/Models/mod_full_n_cross.rds')
# # mod_full_n_nest <- readRDS('results/Models/mod_full_n_nest.rds')
##-----------------------------------------------------------------------------------





# 2 - MODELS TABS -------------------------------------------------------------------
# 2.1 - basic mod Zack + Nuuk
tab_model(mod_basic_z, mod_basic_n,
          p.val = "kr", 
          show.df = TRUE, 
          dv.labels = c("Basic Zack", "Basic Nuuk"))

# 2.2 - full mod Zack + Nuuk
tab_model(mod_full_z_cross, mod_full_n_cross,
          p.val = "kr", 
          show.df = TRUE, 
          dv.labels = c("Full cross Zack", "Full cross Nuuk"))


# 2.3 - ICC: InterClass Correlations
# ICC can be interpreted as “the proportion of the variance explained 
# by the grouping structure in the population”. 
# ICC “can also be interpreted as the expected correlation between two 
# randomly drawn units that are in the same group
# 0<ICC<1 (1=all obs in the group. struct. are equal)
performance::icc(mod_full_z_cross, by_group = TRUE)
performance::icc(mod_full_n_cross, by_group = TRUE)
##-----------------------------------------------------------------------------------




# 3 - MODELS PLOTS and  POSTHOC TEST-------------------------------------------------
# 3.A - Zackenberg
# R2c,m
MuMIn::r.squaredGLMM(mod_basic_z)
MuMIn::r.squaredGLMM(mod_full_z_cross)

# Value per Sp and Sp paire-wise difference
emmeans(mod_full_z_cross, list(pairwise ~ Species), adjust = "tukey")

## R2 marginal et conditionnels des effets fixes
## Methods nsj = Nakagawa and Schielzeth for lmer
r2glmm::r2beta(mod_full_z_cross, method = 'nsj', data=flow_snow_clim_z)

## Graph of models residuals (criticism plots)
## graph 1= density of the model residuals
## graph 2= quantile-quantile plot, standardized residuals vs theoretical quantiles
## graph 3= fitted values vs the standardized residuals
LMERConvenienceFunctions::mcp.fnc(mod_full_z_cross, trim = 2.5)

## Plot of the fixed effects
plot(Effect(c("Species", "Temp_summer"), mod_full_z_cross, residuals=TRUE)) 
plot(Effect(c("Species", "lag_Temp_fall"), mod_full_z_cross, residuals=TRUE))
plot(Effect(c("Species", "snowmelt_DOY"), mod_full_z_cross, residuals=TRUE))
plot(Effect(c("Species", "lag_trans_Flow_m2"), mod_full_z_cross, residuals=TRUE))

## Forest plot of all fixed effects
## vertical grey line = level 1 of each fixed effects
sjPlot::plot_model(mod_full_z_cross, 
                   show.values=T, 
                   show.intercept = TRUE,
                   show.p = TRUE,
                   vline.color="grey", 
                   value.offset=-0.3, 
                   title = "Full Zack nested")


## Detailed fixed effect plots
## show.data=T to show points
## type = "eff" to show real effect
## type ="pred" to show models predictions

# SPECIES
sjPlot::plot_model(mod_full_z_cross, type="eff", 
                   terms=c("Species"), show.data=T) + theme_bw()

# SPECIES * SUMMER TEMPERATURE
sjPlot::plot_model(mod_full_z_cross, type="eff", show.data=F, 
                   terms=c("Species", "Temp_summer")) + theme_bw()

# SPECIES * LAG FALL TEMP (previous automn temperature)
sjPlot::plot_model(mod_full_z_cross, type="eff", show.data=F, 
                   terms=c("Species", "lag_Temp_fall")) + theme_bw()

# SPECIES * LAG FLOW DENSITY (density dependence)
sjPlot::plot_model(mod_full_z_cross, type="eff", show.data=F, 
                   terms=c("Species", "lag_trans_Flow_m2")) + theme_bw()



##------------------------------------
##------------------------------------



# 3.B - Nuuk
# R2c,m
MuMIn::r.squaredGLMM(mod_basic_n)
MuMIn::r.squaredGLMM(mod_full_n_cross)

# Value per Sp and Sp paire-wise difference
emmeans(mod_full_n_cross, list(pairwise ~ Species), adjust = "tukey", data=flow_snow_clim_n)

## R2 marginal et conditionnels des effets fixes
## Methods nsj = Nakagawa and Schielzeth for lmer
r2glmm::r2beta(mod_full_n_cross, method = 'nsj', data=flow_snow_clim_n)

## Graph of models residuals (criticism plots)
## graph 1= density of the model residuals
## graph 2= quantile-quantile plot, standardized residuals vs theoretical quantiles
## graph 3= fitted values vs the standardized residuals
LMERConvenienceFunctions::mcp.fnc(mod_full_n_cross, trim = 2.5)

## Plot of the fixed effects
plot(Effect(c("Species", "Temp_summer"), mod_full_n_cross, residuals=TRUE)) 
plot(Effect(c("Species", "lag_Temp_fall"), mod_full_n_cross, residuals=TRUE))
plot(Effect(c("Species", "snowmelt_DOY"), mod_full_n_cross, residuals=TRUE))
plot(Effect(c("Species", "lag_trans_Flow_m2"), mod_full_n_cross, residuals=TRUE))

## Forest plot of all fixed effects
## vertical grey line = level 1 of each fixed effects
sjPlot::plot_model(mod_full_n_cross, 
                   show.values=T, 
                   show.intercept = TRUE,
                   show.p = TRUE,
                   vline.color="grey", 
                   value.offset=-0.3, 
                   title = "Full Nuuk nested")


## Detailed fixed effect plots
## show.data=T to show points
## type = "eff" to show real effect
## type ="pred" to show models predictions

# SPECIES
sjPlot::plot_model(mod_full_n_cross, type="eff", 
                   terms=c("Species"), show.data=T) + theme_bw()

# SPECIES * SUMMER TEMPERATURE
sjPlot::plot_model(mod_full_n_cross, type="eff", show.data=F, 
                   terms=c("Species", "Temp_summer")) + theme_bw()

# SPECIES * LAG FALL TEMP (previous automn temperature)
sjPlot::plot_model(mod_full_n_cross, type="eff", show.data=F, 
                   terms=c("Species", "lag_Temp_fall")) + theme_bw()

# SPECIES * LAG FLOW DENSITY (density dependence)
sjPlot::plot_model(mod_full_n_cross, type="eff", show.data=F, 
                   terms=c("Species", "lag_trans_Flow_m2")) + theme_bw()
          