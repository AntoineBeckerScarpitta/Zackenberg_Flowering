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
source("Scripts/C2_Climatic_covariates.R")
source("Scripts/C2_Snow_covariates.R")



# 1 - DATA MANAGMENT FOR MODEL -  ZACKENBERG ----------------------------------------
# log(density) !=0 (Flow_m2[abundace==0] <- 0.001 in 02_Creation_DB, line 210)

# add snow covariate into flow _ NO DATA FOR NUUK YET
# flow_snow <- left_join(droplevels(flow %>% filter(Site=="Nuuk")), 
#                        snow, by=c("Year", "Plot")) 

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
flow_snow_clim_n <- left_join(droplevels(flow %>% filter(Site=="Nuuk")),
                              temp_clim_n, by=c('Site', 'Year'))

# add a lag form of trans_flow_m2
flow_snow_clim_n <- flow_snow_clim_n %>% 
  arrange(., Year, Plot) %>% 
  group_by(Plot, Species) %>%
  mutate(., lag_trans_Flow_m2=lag(trans_Flow_m2, order_by = Year))
###---END


# # SnowMelt DOY - NO DATA FOR NUUK YET
# ggplot(flow_snow_clim_n , aes(x=Year, y=snowmelt_DOY, group=Species, color=Species)) + 
#   geom_point(size=2) +  
#   geom_smooth(method='lm', se=F) +
#   theme(axis.text=element_text(size=15),
#         axis.title=element_text(size=16,face="bold"), 
#         panel.background = element_blank(), 
#         axis.line = element_line(colour = "black")) 
#
#modif

# # temporal autocorrelation function
# par(mfrow=c(1,3))
# acf(na.omit(flow_snow_clim_n$TotalFlower), plot = T, lag.max = 12, 
#     main = "Sample Autocorrelation for Total flowering number")
# 
# acf(na.omit(flow_snow_clim_n$Flow_m2), plot = T, lag.max =12, 
#     main = "Sample Autocorrelation for Flowering density")
# 
# acf(na.omit(log(flow_snow_clim_n$trans_Flow_m2)), plot = T, lag.max = 12, 
#     main = "Sample Autocorrelation for Log(Flowering density)")
# par(mfrow=c(1,1))
# #



## 2 - MODELS -----------------------------------------------------------------------
#  MODEL 1: flow(t) ~ Sp * Year + flow(t-1) + ranef(plot)
mod_basic_n <- lmer(log(trans_Flow_m2) ~ Species * Year + lag_trans_Flow_m2 +
                                    (1|Plot),
               data= flow_snow_clim_n,
               REML=T, na.action=na.omit)
summary(mod_basic_n)
saveRDS(mod_basic_n, "results/models/mod_basic_n.rds")


#  MODEL 2: 
# flow(t)~ Sp*clim(summer) + Sp*clim(fall-1) + Sp*SnowMelt + flow(t-1) + ranef(plot)
mod_full_n <- lmer(log(trans_Flow_m2) ~ Species * Temp_summer + 
                                    Species * lag_Temp_fall +
                                    lag_trans_Flow_m2 + (1|Plot),
               data=flow_snow_clim_n, 
               REML=T, na.action=na.omit)
summary(mod_full_n)
saveRDS(mod_full_n, "results/models/mod_full_n.rds")


#  MODEL 3: ranef plot structured by year
# flow(t)~ Sp*clim(summer) + Sp*clim(fall-1) + Sp*SnowMelt + ranef(plot/year)
mod_full_2_n <- lmer(log(trans_Flow_m2) ~ Species * Temp_summer + 
                                    Species * lag_Temp_fall +
                                    (1|Plot/Year),
               data=flow_snow_clim_n, 
               REML=T, na.action=na.omit)
summary(mod_full_2_n)
saveRDS(mod_full_2_n, "results/models/mod_full_ranef_plot_year_n.rds")
#-----------------------------------------------------------------------------------=

# models used different data, can't compare them
# anova(mod1, mod2)


# R2c, m
MuMIn::r.squaredGLMM(mod_basic_n)
MuMIn::r.squaredGLMM(mod_full_n)
MuMIn::r.squaredGLMM(mod_full_2_n)


# POSTHOC TEST ON mod_full_n (full model)
# # posthoc test
# emmeans(mod_full_n, list(pairwise ~ Species), adjust = "tukey")
# 
# 
# # # r2 marginal et conditionnels des effets fixes
# r2glmm::r2beta(mod_full_n, method = 'nsj') 
# # check les methodes, ?a peux faire une diff?rence...
# 
# # # plot les "graph criticism plots"
# LMERConvenienceFunctions::mcp.fnc(mod_full_n)$rstand
# 
# 
# # # plot rapide des effects fixes significatif sous forme de graph
# plot(effects::allEffects(mod_full_n),multiline=T,rug=F,ci.style = "line",show.data=T)
# 
# 
# # # plot rapide de tout les effects fixes ( la ligne verticale du 0 ?tant le "niveau 1" de chaque effet fixe)
# sjPlot::plot_model(mod_full_n,show.values = T,vline.color = "grey",value.offset = -0.3)
# 
# 
# # plot des effets fixes en d?tail
# # ,show.data=T si tu veux voir les points
# # ,type = "eff" si tu veux voir les effets "reels" ; ,type ="pred" si tu veux voir les effets pr?dits par le mod?le
# sjPlot::plot_model(mod_full_n, type = "eff", terms = c("Species"),show.data=F)+theme_bw()
# sjPlot::plot_model(mod_full_n, type = "eff", terms = c("Species","lag_trans_Flow_m2"),show.data=F)+theme_bw()
# 
# # sjPlot::plot_model(mod_full_n, type = "eff", terms = c("Species","snowmelt_DOY"),show.data=F)+theme_bw()
# 
# 
# # sjPlot::plot_model(mod_full_n, type = "eff", terms = c("snowmelt_DOY", "Species"),show.data=F)+theme_bw()
# sjPlot::plot_model(mod_full_n, type = "eff", terms = c("lag_trans_Flow_m2", "Species"),show.data=F)+theme_bw()
# sjPlot::plot_model(mod_full_n, type = "eff", terms = c("Temp_summer", "Species"),show.data=F)+theme_bw()
# sjPlot::plot_model(mod_full_n, type = "eff", terms = c("lag_Temp_fall", "Species"),show.data=F)+theme_bw()
