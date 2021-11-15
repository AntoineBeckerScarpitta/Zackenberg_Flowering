#######################################################################################
#
#                           Greenland Flowering project
#                            04 - Analysis Zackenberg
#
#######################################################################################
# Antoine Becker-Scarpitta
# January 2021

#  clean R work-space
# rm(list=ls())

# # Load 02 - Creation database (+ scripts 00 and 01)
# source("Scripts/00_Load_libraries.r")
# source("Scripts/01_Import_DB.r")
source("Scripts/02_Creation_DB.r")
source("Scripts/C1_Load_climatic_covariates.R")
source("Scripts/C2_Build_all_snow_covariates.R")

remove("clim", "clim_year")



#Documentation for sjPLOT 
# https://cloud.r-project.org/web/packages/sjPlot/index.html
# http://www.strengejacke.de/sjPlot/reference/plot_model.html
# https://cloud.r-project.org/web/packages/sjPlot/vignettes/plot_interactions.html
# https://cloud.r-project.org/web/packages/sjPlot/vignettes/plot_marginal_effects.html
# https://cloud.r-project.org/web/packages/sjPlot/vignettes/plot_model_estimates.html
# https://cloud.r-project.org/web/packages/sjPlot/vignettes/tab_mixed.html




# 1 - DATA MANAGMENT FOR MODEL -  ZACKENBERG ----------------------------------------
# log(density) !=0 (Flow_m2[abundace==0] <- 0.001 in 02_Creation_DB, line 210)
# add snow covariate into flow
flow_snow_z <- left_join(droplevels(flow %>% filter(Site=="Zackenberg")), 
                         droplevels(snow%>% filter(Site=="Zackenberg"))
                         , by=c("Year", "Plot", "Site")) 


#reshape climatic data
temp_clim_z <- droplevels(clim_season_year %>% filter(Site=='Zackenberg') %>%
  pivot_wider(id_cols=c(Site, Year, Season), names_from=Variable,
                                 values_from=Value, values_fill=0) %>%
  dplyr::select(!c('Humidity_%', 'Precipitation_mm')) %>% 
  filter(Season!="winter") %>%
  pivot_wider(id_cols=c(Site, Year), names_from=Season,
              values_from=Temperature_C, values_fill=0))
#rename cols
colnames(temp_clim_z) <- c('Site', 'Year', 'Temp_fall', 'Temp_summer')

#create the lag of temp of fall
temp_clim_z$lag_Temp_fall <- lag(temp_clim_z$Temp_fall, k = 1)


# FINAL DATASET:
#add clim data in flow
flow_snow_clim_z <- left_join(flow_snow_z, temp_clim_z, by=c('Site', 'Year'))

# add a lag form of trans_flow_m2
flow_snow_clim_z <- flow_snow_clim_z %>% 
  arrange(., Year, Plot) %>% 
  group_by(Plot, Species) %>%
  mutate(., lag_trans_Flow_m2=lag(trans_Flow_m2, order_by = Year))
#---END


## PLOT SnowMelt DOY Zackenberg
temp_graph_z <- ggplot(flow_snow_clim_z , aes(x=Year, y=snowmelt_DOY, 
                              group=Species, 
                              color=Species)) +
  geom_point(size=2) +
  geom_smooth(method='lm', se=F) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16,face="bold"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

# #modif
# 
# # temporal autocorrelation function
# par(mfrow=c(1,3))
# acf(na.omit(flow$TotalFlower), plot = T, lag.max = 22, 
#     main = "Sample Autocorrelation for Total flowering number")
# 
# acf(na.omit(flow$Flow_m2), plot = T, lag.max =22, 
#     main = "Sample Autocorrelation for Flowering density")
# 
# acf(na.omit(log(flow$trans_Flow_m2)), plot = T, lag.max = 22, 
#     main = "Sample Autocorrelation for Log(Flowering density)")
# par(mfrow=c(1,1))
##-----------------------------------------------------------------------------------




## 2 - MODELS -----------------------------------------------------------------------
#  MODEL 1: EQ1 - temporal trends in flowering density for each sp?
# flow(t) ~ Sp * Year + ranef(plot)
mod_basic_z <- lmer(log(trans_Flow_m2) ~ Species * Year + (1|Plot),
             data= flow_snow_clim_z,
             REML=T, na.action=na.omit)
# summary(mod_basic_z)
# saveRDS(mod_basic_z, "results/models/mod_basic_z.rds")


# #  MODEL 2a: EQ2 NESTED - What climatic variables and density dep. drive the trends?
# # The design is crossed, since all plot are sampled every year :
# # every year all plot are sample, then repetition of plot withtin year
mod_full_z_nest <- lmer(log(trans_Flow_m2) ~  Species * Temp_summer +
                                              Species * lag_Temp_fall +
                                              Species * snowmelt_DOY +
                                              Species * log(lag_trans_Flow_m2) +
                                              (1|Plot) + (1|Plot:Year),
             data=flow_snow_clim_z,
             REML=T, na.action=na.omit)
# summary(mod_full_z_nest)
# # saveRDS(mod_full_z_nest, "results/models/mod_full_z_nest.rds")



#  MODEL 2b: EQ2 CROSSED - 
# The valid structure is crossed
mod_full_z_cross <- lmer(log(trans_Flow_m2) ~  Species * Temp_summer + 
                                          Species * lag_Temp_fall +
                                          Species * snowmelt_DOY + 
                                          Species * log(lag_trans_Flow_m2) + 
                                          (1|Plot) + (1|Year),
                    data=flow_snow_clim_z, 
                    REML=T, na.action=na.omit)
# summary(mod_full_z_cross)
# saveRDS(mod_full_z_cross, "results/models/mod_full_z_cross.rds")
#-----------------------------------------------------------------------------------




# #### Variables backward selection ----------------------------------------------------
# # NOT USEFUL - BEST MODEL = mod_full_z_cross
# # create a dB without NA (same used in lmer, Mod1 & Mod2)
# flow_snow_clim_z <- flow_snow_clim_z[complete.cases(flow_snow_clim_z),]
# 
# # Backward variable selection on full model EQ2 
# lmerTest::step(mod_full_z_cross, direction = "backward", trace=FALSE ) 
# 
# # get model after selection
# mod_bw_sel_z <- get_model(lmerTest::step(mod_full_z_cross, 
#                                               direction="backward", 
#                                               trace=FALSE ) )
# 
# # saveRDS(mod_bw_sel_z, "results/models/mod_bw_sel_z.rds")
# summary(mod_bw_sel_z)
#------------------------------------------------------------------------------------


# #### Results tables -----------------------------------------------------------------
# #All tab mod together
# tab_model(mod_basic_z, mod_full_z_nest, mod_full_z_cross,
#           p.val = "kr", 
#           show.df = TRUE, 
#           dv.labels = c("Basic Zack", "Full Zack nested", 
#                         "Full Zack crossed"))
# 
# # tab for final model
# tab_model(mod_full_z_cross,
#           p.val = "kr", 
#           show.df = TRUE, 
#           dv.labels = "Final model Zackenberg")
# #END----------------------------------------------------------------------------------



