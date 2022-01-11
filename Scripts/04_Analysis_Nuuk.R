#######################################################################################
#
#                           Greenland Flowering project
#                             04 - Analysis Nuuk
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


# 1 - DATA MANAGMENT FOR MODEL -  ZACKENBERG ----------------------------------------
# log(density) !=0 (Flow_m2[abundace==0] <- 0.001 in 02_Creation_DB, line 210)

# add snow covariate into flow _ NO DATA FOR NUUK YET
flow_snow_n <- left_join(droplevels(flow %>% filter(Site=="Nuuk")),
                         droplevels(snow %>% filter(Site=="Nuuk")), 
                       by=c("Year", "Plot", "Site"))

#reshape climatic data
temp_clim_n <- droplevels(clim_season_year %>% filter(Site=='Nuuk') %>%
                            pivot_wider(id_cols=c(Site, Year, Season), names_from=Variable,
                                        values_from=Value, values_fill=0) %>%
                            dplyr::select(!c('Humidity_%', 'Precipitation_mm')) %>% 
                            filter(Season!="winter") %>%
                            pivot_wider(id_cols=c(Site, Year), names_from=Season,
                                        values_from=Temperature_C, values_fill=0))
#rename cols
colnames(temp_clim_n) <- c('Site', 'Year', 'Temp_fall', 'Temp_summer')

#create the lag of temp of fall
temp_clim_n$lag_Temp_fall <- lag(temp_clim_n$Temp_fall, k = 1)


# FINAL DATASET:
#add clim data in flow
flow_snow_clim_n <- left_join(flow_snow_n,temp_clim_n, 
                              by=c('Site', 'Year'))

# add a lag form of trans_flow_m2
flow_snow_clim_n <- flow_snow_clim_n %>% 
  arrange(., Year, Plot) %>% 
  group_by(Plot, Species) %>%
  mutate(., lag_trans_Flow_m2=lag(trans_Flow_m2, order_by = Year))
###---END



# # SnowMelt DOY 
temp_graph_n <- ggplot(flow_snow_clim_n , aes(x=Year, y=snowmelt_DOY, group=Species, color=Species)) +
  geom_point(size=2) +
  geom_smooth(method='lm', se=F) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16,face="bold"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
#-----------------------------------------------------------------------------------=




## 2 - MODELS -----------------------------------------------------------------------
#  MODEL 1: EQ1 - temporal trends in flowering density for each sp?
mod_basic_n <- lmer(log(trans_Flow_m2) ~ Species * Year + (1|Plot),
               data= flow_snow_clim_n,
               REML=T, na.action=na.omit)
# summary(mod_basic_n)
# saveRDS(mod_basic_n, "results/models/mod_basic_n.rds")



#  MODEL 2a: EQ2 NESTED - What climatic variables and density dep. drive the trends?
# The design is crossed, since all plot are sampled every year :
# every year all plot are sample, then repetition of plot withtin year == Crossed
mod_full_n_nest <- lmer(log(trans_Flow_m2) ~ Species * Temp_summer + 
                          Species * lag_Temp_fall +
                          Species * snowmelt_DOY + 
                          Species * log(lag_trans_Flow_m2) + 
                          (1|Plot) + (1|Plot:Year),
                        data=flow_snow_clim_n, 
                        REML=T, na.action=na.omit)
# summary(mod_full_n_nest)
# saveRDS(mod_full_n_nest, "results/models/mod_full_n_nest.rds")




#  MODEL 2b: EQ2 CROSSED - VALIDE STRUCTURE
# The valide structure is crossed
mod_full_n_cross <- lmer(log(trans_Flow_m2) ~  Species * Temp_summer + 
                           Species * lag_Temp_fall +
                           Species * snowmelt_DOY + 
                           Species * log(lag_trans_Flow_m2) + 
                           (1|Plot) + (1|Year),
                         data=flow_snow_clim_n, 
                         REML=T, na.action=na.omit)
# summary(mod_full_n_cross)
# saveRDS(mod_full_n_cross, "results/models/mod_full_n_cross.rds")
#------------------------------------------------------------------------------------




#### Variables backward selection ----------------------------------------------------
# # NOT USEFUL - BEST MODEL = mod_full_z_cross
# # create a dB without NA (same used in lmer, Mod1 & Mod2)
# flow_snow_clim_n <- flow_snow_clim_n[complete.cases(flow_snow_clim_n),]
# 
# # Backward variable selection on full model EQ2 
# lmerTest::step(mod_full_n_cross, direction = "backward", trace=FALSE ) 
# 
# # get model after selection
# mod_bw_sel_n <- get_model(lmerTest::step(mod_full_n_cross, 
#                                          direction="backward", 
#                                          trace=FALSE ) )
# 
# # saveRDS(mod_bw_sel_n, "results/models/mod_bw_sel_n.rds")
# summary(mod_bw_sel_n)
#------------------------------------------------------------------------------------



# #### Results tables -----------------------------------------------------------------
# #All tab mod together
# tab_model(mod_basic_n, mod_full_n_nest, mod_full_n_cross,
#           p.val = "kr", 
#           show.df = TRUE, 
#           dv.labels = c("Basic Nuuk", "Full Nuuk nested", 
#                         "Full Nuuk crossed", "Full Nuuk sel"))
# 
# # tab for final model
# tab_model(mod_full_n_nest,
#           p.val = "kr", 
#           show.df = TRUE, 
#           dv.labels = "Final model Nuuk")
# #END----------------------------------------------------------------------------------




