#######################################################################################
#
#                           Greenland Flowering project
#                            05 - Variable selection
#
#######################################################################################
# Antoine Becker-Scarpitta & Laura Antao
# Febuary 2021

#  clean R work-space
rm(list=ls())


# # Load data and models
source("Scripts/04_Analysis_Nuuk.R")
source("Scripts/04_Analysis_Zack.R")


#Documentation for sjPLOT 
# https://cloud.r-project.org/web/packages/sjPlot/index.html
# http://www.strengejacke.de/sjPlot/reference/plot_model.html
# https://cloud.r-project.org/web/packages/sjPlot/vignettes/plot_interactions.html
# https://cloud.r-project.org/web/packages/sjPlot/vignettes/plot_marginal_effects.html
# https://cloud.r-project.org/web/packages/sjPlot/vignettes/plot_model_estimates.html
# https://cloud.r-project.org/web/packages/sjPlot/vignettes/tab_mixed.html



################# ZACKENBERG
#### Variables backward selection ----------------------------------------------------
# creat a dB without NA (same used for lmer)
flow_snow_clim_z <- flow_snow_clim_z[complete.cases(flow_snow_clim_z),]

#variable selection (get rid of snowmelt)
lmerTest::step(mod_full_2_z, direction = "backward", trace=FALSE ) 

#get model after selection
mod_bw_sel_z <- get_model(lmerTest::step(mod_full_2_z, 
                                         direction="backward", 
                                         trace=FALSE ) )
summary(mod_bw_sel_z)
MuMIn::r.squaredGLMM(mod_bw_sel_z)

#All tab mod together
tab_model(mod_basic_z, mod_full_z, mod_full_2_z, mod_bw_sel_z,
          p.val = "kr", 
          show.df = TRUE, 
          dv.labels = c("Basic Zack", "Full Zack (1|plot)", 
                        "Full2 (1|plot/year)", "Mod BW sel"))

# tab for final model
tab_model(mod_bw_sel_z,
          p.val = "kr", 
          show.df = TRUE, 
          dv.labels = "Final model Zackenberg")


# tab for basic model ZACK NUUK
tab_model(mod_basic_z, mod_basic_n,
          p.val = "kr", 
          show.df = TRUE, 
          dv.labels = c("Zackenberg", "Nuuk"))
#END----------------------------------------------------------------------------------






################# NUUK
#### Variables backward selection ----------------------------------------------------
# creat a dB without NA (same used for lmer)
flow_snow_clim_n <- flow_snow_clim_n[complete.cases(flow_snow_clim_n),]

#variable selection (get rid of snowmelt)
lmerTest::step(mod_full_2_n, direction = "backward", trace=FALSE ) 

#get model after selection
# mod_bw_sel_n <- get_model(lmerTest::step(mod_full_2_n, 
#                                               direction="backward", 
#                                               trace=FALSE ) )

# write our own model sicne we remove lag flower density
mod_bw_sel_n <- lmer(log(trans_Flow_m2) ~ Species + Temp_summer + 
                       lag_Temp_fall+ (1 | Plot:Year), 
                     data=flow_snow_clim_n, 
                     REML=T, na.action=na.omit)

summary(mod_bw_sel_n)
MuMIn::r.squaredGLMM(mod_bw_sel_n)

# tab for final model
tab_model(mod_bw_sel_n,
          p.val = "kr", 
          show.df = TRUE, 
          dv.labels = "Final model Nuuk")


#All tab mod together
tab_model(mod_basic_n, mod_full_n, mod_full_2_n, mod_bw_sel_n,
          p.val = "kr", 
          show.df = TRUE, 
          dv.labels = c("Basic Nuuk", "Full Nuuk (1|plot)", 
                        "Full2 (1|plot/year)", "Full2 BW sel"))
#END----------------------------------------------------------------------------------









