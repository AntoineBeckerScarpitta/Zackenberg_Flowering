###############################################################################
#
#                           Greenland Flowering project
#                            04 - Analysis Zackenberg
#
###############################################################################
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


##-- DATA FOR TOMAS --##
# flow_snow_zack_TR <- flow %>%
#   dplyr::select(-trans_Flow_m2) %>%
#   dplyr::filter(Site=='Zackenberg') %>%
#   droplevels(.$Species) %>%
#   left_join(., droplevels(snow %>% filter(Site=="Zackenberg"))
#             , by=c("Year", "Plot", "Site")) 
# table(flow_zack_TR$Year, flow_zack_TR$Species)
 # write.csv(flow_snow_zack_TR, 'data/data_TR_pheno/flow_zack_TR.csv')
##-----------------------



# 1 - DATA MANAGMENT FOR MODEL -  ZACKENBERG ----------------------------------
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
  mutate(., lag_trans_Flow_m2=lag(trans_Flow_m2, order_by = Year)) %>%
  mutate(., log_flow=log(trans_Flow_m2), 
         log_lag_flow=log(lag_trans_Flow_m2)) %>%
  dplyr::select(Site, Year,  Plot, Species, log_flow, snowmelt_DOY, 
                lag_Temp_fall, Temp_summer, log_lag_flow)
##----------------------------------------------------------------------------

##full dataset for mod
# write.csv2(flow_snow_clim_z, "flow_snow_clim_z.csv")



##----------------------------------------------------------------------------
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
##----------------------------------------------------------------------------




## 2 - MODELS -----------------------------------------------------------------
#  MODEL 1: EQ1 - temporal trends in flowering density for each sp?
# flow(t) ~ Sp * Year + ranef(plot)
mod_basic_z <- lmer(log_flow ~ 0 + Species * Year + (1|Plot),
             data= flow_snow_clim_z,
             REML=T, 
             na.action=na.omit)


# Reorganise data
datZ <- flow_snow_clim_z

# Make all 0s count of flowers NAs
datZ$log_flow[which(datZ$log_flow == min(datZ$log_flow))] <- NA

# Scale explanatory variable except for lag log_flow
datZ$snowmelt_DOY <- scale(datZ$snowmelt_DOY)
datZ$Temp_summer <- scale(datZ$Temp_summer)
datZ$lag_Temp_fall <- scale(datZ$lag_Temp_fall)

# Make all 0s count of flowers NAs
datZ$log_lag_flow[which(datZ$log_lag_flow == min(datZ$log_lag_flow))] <- NA


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
#------------------------------------------------------------------------------




# #### Variables backward selection -------------------------------------------
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
#------------------------------------------------------------------------------




