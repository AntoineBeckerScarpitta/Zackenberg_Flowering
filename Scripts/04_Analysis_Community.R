################################################################################
#
#                           Greenland Flowering project
#                            04 - Analysis Community 
#
################################################################################
# Antoine Becker-Scarpitta
# January 2021

#  clean R work-space
# rm(list=ls())

# source("Scripts/00_Load_libraries.r")
# source("Scripts/01_Import_DB.r")
source("Scripts/02_Creation_DB.r")
source("Scripts/C1_Load_climatic_covariates.R")
source("Scripts/C2_Build_all_snow_covariates.R")
remove("clim")


# 1 - DATA MANAGMENT FOR MODEL -  COMMUNITY ---999------------------------------
#reshape climat season scale
clim_seas_com <- droplevels(clim_season_year  %>%
                            pivot_wider(id_cols=c(Site, Year, Season), 
                                        names_from=Variable,
                                        values_from=Value, 
                                        values_fill=0) %>%
                            dplyr::select(!c('Humidity_%')) %>% 
                            pivot_wider(id_cols=c(Site, Year), 
                                        names_from=Season,
                                        values_from=c(Temperature_C, Precipitation_mm), 
                                        values_fill=0))

#reshape climat year scale
clim_year_com <- droplevels(clim_year  %>% pivot_wider(id_cols=c(Site, Year), 
                                           names_from=Variable,
                                           values_from=Value, 
                                           values_fill=0))
#rename cols
colnames(clim_year_com) <- c('Site', 'Year', "Hum_year", 'Prec_year', 'Temp_year')                    

#join community data AND Cimatic data
flow_clim_com <- left_join(flow_com, clim_year_com, by=c('Site', 'Year'))
                              





## 2 - MODELS ------------------------------------------------------------------
#  MODEL 1- temporal trends in flowering density for each site
# Comflow(t) ~  Year 
com_z <- lm(ComFlow_m2 ~ Year,
           data=flow_clim_com[flow_clim_com$Site=="Zackenberg", ],
                na.action=na.omit)

com_n <- lm(ComFlow_m2 ~ Year ,
           data=flow_clim_com[flow_clim_com$Site=="Nuuk", ],
           na.action=na.omit)

#mod results
tab_model(com_z, com_n,
          p.val = "kr", 
          show.df = TRUE, 
          dv.labels = c("Community Zack", "Community Nuuk"))



#  MODEL 2 - temporal trends in flowering density for each sp?
# Comlow(t) ~ Year + Climat
com_z2 <- lm(ComFlow_m2 ~ Year + Hum_year + Prec_year + Temp_year,
              data=flow_clim_com[flow_clim_com$Site=="Zackenberg", ],
              na.action=na.omit)

com_n2 <- lm(ComFlow_m2 ~ Year + Hum_year + Prec_year + Temp_year,
           data=flow_clim_com[flow_clim_com$Site=="Nuuk", ],
           na.action=na.omit)

#mod results
tab_model(com_z2, com_n2,
          p.val = "kr", 
          show.df = TRUE, 
          dv.labels = c("Community Zack - Clim", "Community Nuuk - Clim"))




















