###############################################################################
#
#                           Greenland Flowering project
#                             04 - Analysis Nuuk
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


# 1 - DATA MANAGMENT FOR MODEL -  NUUK ----------------------------------
# log(density) !=0 (Flow_m2[abundace==0] <- 0.001 in 02_Creation_DB, line 210)
# will be replace by NA later for full mod

# add snow covariate into flow _ NO DATA FOR NUUK YET
flow_snow_n <- left_join(droplevels(flow %>% filter(Site=="Nuuk")),
                         droplevels(snow %>% filter(Site=="Nuuk")), 
                       by=c("Year", "Plot", "Site"))

#reshape climatic data
temp_clim_n <- droplevels(clim_season_year %>% filter(Site=='Nuuk') %>%
                            pivot_wider(id_cols=c(Site, Year, Season), 
                                        names_from=Variable,
                                        values_from=Value, 
                                        values_fill=0) %>%
                            dplyr::select(!c('Humidity_%', 'Precipitation_mm')) %>% 
                            filter(Season!="winter") %>%
                            pivot_wider(id_cols=c(Site, Year), 
                                        names_from=Season,
                                        values_from=Temperature_C, 
                                        values_fill=0))
#rename cols
colnames(temp_clim_n) <- c('Site', 'Year', 'Temp_fall', 'Temp_summer')

#create the lag of temp of fall
temp_clim_n$lag_Temp_fall <- lag(temp_clim_n$Temp_fall, k = 1)


# FINAL DATASET:
#add clim data in flow
flow_snow_clim_n <- left_join(flow_snow_n, temp_clim_n, 
                              by=c('Site', 'Year'))

# add a lag form of trans_flow_m2
flow_snow_clim_n <- flow_snow_clim_n %>% 
  arrange(., Year, Plot) %>% 
  group_by(Plot, Species) %>%
  mutate(., lag_trans_Flow_m2=lag(trans_Flow_m2, order_by = Year)) %>%
  mutate(., log_flow=log(trans_Flow_m2), 
          log_lag_flow=log(lag_trans_Flow_m2)) %>%
  dplyr::select(Site, Year,  Plot, Species, log_flow, snowmelt_DOY, 
         lag_Temp_fall, Temp_summer, log_lag_flow)
#-----------------------------------------------------------------------------=

##full dataset for mod
# write.csv2(flow_snow_clim_n, "flow_snow_clim_n.csv")





#-----------------------------------------------------------------------------=
## PLOT SnowMelt DOY Nuuk
temp_graph_n <- ggplot(flow_snow_clim_n,
                       aes(x=Year, 
                           y=snowmelt_DOY, 
                           group=Species, 
                           color=Species)) +
  geom_point(size=2) +
  geom_smooth(method='lm', se=F) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16,face="bold"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))
#-----------------------------------------------------------------------------=




## 2 - MODELS -----------------------------------------------------------------
#  MODEL 1: EQ1 - temporal trends in flowering density for each sp?
mod_basic_n <- lmer(log_flow ~ 0 + Species * Year + (1|Plot),
               data= flow_snow_clim_n,
               REML=T, na.action=na.omit)



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
#------------------------------------------------------------------------------

