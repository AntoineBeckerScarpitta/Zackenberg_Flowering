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
                         droplevels(snow %>% filter(Site=="Zackenberg"))
                         , by=c("Year", "Plot", "Site")) 

#reshape climatic data
temp_clim_z <- droplevels(clim_season_year %>% filter(Site=='High_Arctic') %>%
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

#Rename Site names
flow_snow_z <- flow_snow_z %>%
  dplyr::mutate(Site=
                  dplyr::recode(Site,
                                "Nuuk"="Low_Arctic",
                                "Zackenberg"="High_Arctic"))


# FINAL DATASET:
#add clim data in flow
flow_snow_clim_z <- left_join(flow_snow_z, temp_clim_z, by=c('Site', 'Year'))

# add a lag form of flow_m2
flow_snow_clim_z <- flow_snow_clim_z %>% 
  arrange(., Year, Plot) %>% 
  group_by(Plot, Species) %>%
  mutate(., lag_Flow_m2=lag(Flow_m2, order_by = Year)) %>%
  mutate(., log_flow=log1p(Flow_m2), 
         log_lag_flow=log1p(lag_Flow_m2)) %>%
  dplyr::select(Site, Year,  Plot, Species, log_flow, snowmelt_DOY, 
                lag_Temp_fall, Temp_summer, log_lag_flow)
##----------------------------------------------------------------------------

##full dataset for mod
# write.csv2(flow_snow_clim_z, "Guillaume/V2/flow_snow_clim_z.csv")



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
mod_basic_z <- lmer(log_flow ~ 0 + 
                      Species * Year +
                      (1|Plot),
                    data= flow_snow_clim_z,
                    REML=T, 
                    na.action=na.omit)


#  MODEL EQ3 - Full model
# Reorganise data
datZ <- flow_snow_clim_z

# Scale explanatory variable except for lag log_flow
datZ$snowmelt_DOY <- scale(datZ$snowmelt_DOY)
datZ$Temp_summer <- scale(datZ$Temp_summer)
datZ$lag_Temp_fall <- scale(datZ$lag_Temp_fall)


#  MODEL EQ3 - Full model
# mod_full_z_cross <- lmer(log_flow ~  0 +
#                            Species * Temp_summer +
#                            Species * lag_Temp_fall +
#                            Species * snowmelt_DOY +
#                            Species * log_lag_flow +
#                            (1|Plot) + (1|Plot:Year),
#                          data=datZ,
#                          REML=TRUE, 
#                          na.action=na.omit)

#  MODEL EQ3 - Interaction model
mod_full_z_cross_int <- lmer(log_flow ~  0 +
                               Species : Temp_summer +
                               Species : lag_Temp_fall +
                               Species : snowmelt_DOY +
                               Species : log_lag_flow + offset(1 * log_lag_flow) +
                               (1|Plot:Year),
                             data=datZ,
                             REML=TRUE, 
                             na.action=na.omit)

# mod_full_z_cross_int <- lmer(log_flow ~  0 +
#                                Species : Temp_summer +
#                                Species : lag_Temp_fall +
#                                Species : snowmelt_DOY +
#                                Species : offset(1 * log_lag_flow) +
#                                (1|Plot) + (1|Plot:Year),
#                              data=datZ,
#                              REML=TRUE, 
#                              na.action=na.omit)

#------------------------------------------------------------------------------


#abbe plot Flo t ~ t-1
# ggplot(datZ , aes(y=log_flow, x=log_lag_flow)) + 
#   xlim(0,7) + 
#   ylim(0,7) + 
#   geom_abline(intercept = 0, slope = 1, color="black", size=1) + 
#   geom_point(aes(color=Species)) +
#   labs(title="t~t-1 High Arctic") + 
#   xlab('t-1') + 
#   ylab('t') +
#   theme_classic()  +
#   theme(text = element_text(size = 20))
