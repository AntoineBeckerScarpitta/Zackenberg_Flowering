##############################################################################
#
#                           Greenland Flowering project
#                            05 - Models Exploration
#
##############################################################################
# Antoine Becker-Scarpitta & Laura Antao
# May 2021

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
#3 - SPECIES LEVEL PLOTS
#   3.A - Zackenberg
#   3.B - Nuuk
#4 - FOREST PLOTS
#   4.A - Zackenberg
#   4.B - Nuuk


#Documentation for sjPLOT 
# https://cloud.r-project.org/web/packages/sjPlot/index.html
# http://www.strengejacke.de/sjPlot/reference/plot_model.html
# https://cloud.r-project.org/web/packages/sjPlot/vignettes/plot_interactions.html
# https://cloud.r-project.org/web/packages/sjPlot/vignettes/plot_marginal_effects.html
# https://cloud.r-project.org/web/packages/sjPlot/vignettes/plot_model_estimates.html
# https://cloud.r-project.org/web/packages/sjPlot/vignettes/tab_mixed.html



# 1.1 - LOAD MODELS ----------------------------------------------------------
#  clean R work-space
rm(list=ls())

# Run mods
source("Scripts/04_Analysis_Zack.r")
source("Scripts/04_Analysis_Nuuk.r")

# clean R object 
remove("clim_season_year", "temp_clim_z", "temp_clim_n", "flow_snow_z",
       "flow_snow_n", "snow", "flow")
##----------------------------------------------------------------------------



# 2 - MODELS TABS BASIC ------------------------------------------------------
# 2.1 - basic mod Zack + Nuuk
tab_model(mod_basic_z, mod_basic_n,
          p.val = "kr", 
          show.df = TRUE, 
          dv.labels = c("Basic Zack", "Basic Nuuk"))


#Zackenberg ANOVA(mod_sp)
mod_z <- anova(mod_basic_z)
# extract values
n_z <- rownames(mod_z)
F_z <- round(mod_z$`F value`, 2)
df_z <- mod_z$NumDF
p_z <- mod_z$`Pr(>F)`
#compile in df
data.frame(n_z,F_z ,df_z, p_z )


#Nuuk ANOVA(mod_sp)
mod_n <- anova(mod_basic_n)
# extract values
n_n <- rownames(mod_n)
F_n <- round(mod_n$`F value`, 2)
df_n <- mod_n$NumDF
p_n <- mod_n$`Pr(>F)`
#compile in df
data.frame(n_n,F_n ,df_n, p_n )
##----------------------------------------------------------------------------



# 3 -  SPECIES LEVEL GRAPH---------------------------------
#ZACKENBERG
#extract the data used in mod_basic_z
data_mod_z <- as.data.frame(mod_basic_z@frame)

#extract the predict from mod_basic_z
data_mod_z$pred <- predict(mod_basic_z, re.form=NA)  ## population level

#graph plot effect + predict sp level
zack <- ggplot(data_mod_z, aes(Year, log_flow, 
                               group=Species, color=Species)) +
  geom_point(size=1) +
  # geom_smooth(method='lm', se=TRUE, aes(fill=Plot), alpha = 0.1) + #plot trends
  geom_smooth(method='loess', se=FALSE, colour="black", 
              aes(y=pred, group=Species)) + # sp prediction
  labs(y='log(flower density)') +
  ggtitle('High Arctic - Zackenberg') +
  facet_grid(Species~., scales="fixed") +
  theme_linedraw() +
  scale_x_continuous(limits = c(1997, 2019),
                     breaks = c(1997, 2002, 2007, 2012, 2017)) +
  theme(legend.position = "none", 
        axis.text=element_text(size=10),
        axis.title=element_text(size=10, face="bold"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", color="black"))


#NUUK
#extract the data used in mod_basic_n
data_mod_n <- as.data.frame(mod_basic_n@frame)

#extract the predict from mod_basic_n
data_mod_n$pred <- predict(mod_basic_n, re.form=NA)  ## population level

#graph plot effect + predict sp level
nuuk <- ggplot(data_mod_n, aes(Year, log_flow, 
                               group=Plot, color=Plot)) +
  geom_point(size=1) +
  geom_smooth(method='lm', se=TRUE, aes(fill=Plot), alpha = 0.1) + # plottrends
  # geom_point(aes(y=pred), color="black", size=1) +
  geom_smooth(method='loess', se=FALSE, colour="black", 
              aes(y=pred, group=Species)) + # sp prediction
  labs(y='log(flower density)') +
  ggtitle('Low Arctic - Nuuk') +
  facet_grid(Species~., scales="fixed")+
  theme_linedraw() +
  scale_x_continuous(limits = c(2009, 2019),
                     breaks = c(2009, 2011, 2013, 2015, 2017, 2019)) +
  theme(legend.position = "none", 
        axis.text=element_text(size=10),
        axis.title=element_text(size=10, face="bold"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", color="black"))

#panel
gridExtra::grid.arrange(zack, nuuk, ncol=2)

#PDF SAVING ------------------------------------------------------------------
# Customizing the output
pdf("results/Models/Sp_level_plot.pdf",         # File name
    width = 9, height = 8, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk",    # Color model (cmyk is required for  publications)
    paper = "A4")          # Paper size

# Creating a plot
gridExtra::grid.arrange(zack, nuuk, ncol=2)

# Closing the graphical device
dev.off() 
##------------------------------------------------------------------------------

# 
# ## sorti unique par site
# # Customizing the output
# pdf("results/Models/Sp_level_plot_NUUK.pdf",         # File name
#     width = 4, height = 8, # Width and height in inches
#     bg = "white",          # Background color
#     colormodel = "cmyk",    # Color model (cmyk is required for  publications)
#     paper = "A4")          # Paper size
# 
# # Creating a plot
# nuuk
# 
# # Closing the graphical device
# dev.off() 
# 
# # Customizing the output
# pdf("results/Models/Sp_level_plot_ZACK.pdf",         # File name
#     width = 4, height = 8, # Width and height in inches
#     bg = "white",          # Background color
#     colormodel = "cmyk",    # Color model (cmyk is required for  publications)
#     paper = "A4")          # Paper size
# 
# # Creating a plot
# zack
# 
# # Closing the graphical device
# dev.off() 



# 3 - MODELS TABS FULL --------------------------------------------------------
# full mod Zack + Nuuk
tab_model(mod_full_z_cross_int, mod_full_n_cross_int,
          p.val = "kr", 
          show.df = TRUE, 
          dv.labels = c("Full int Zack", "Full int Nuuk"))

# 2.3 - ICC: InterClass Correlations
# ICC can be interpreted as “the proportion of the variance explained 
# by the grouping structure in the population”. 
# ICC “can also be interpreted as the expected correlation between two 
# randomly drawn units that are in the same group
# 0<ICC<1 (1=all obs in the group. struct. are equal)
performance::icc(mod_full_z_cross_int, by_group = TRUE)
performance::icc(mod_full_n_cross_int, by_group = TRUE)


#Pvalue and distribution offset
#NUUK
paramFix_n <- summary(mod_full_n_cross_int)$coefficients
pointerLag_n <- grep("log_lag_flow",rownames(paramFix_n))

as.data.frame(pt(q = abs((1 - paramFix_n[pointerLag_n,1])/paramFix_n[pointerLag_n,2]), 
   df =  paramFix_n[pointerLag_n,3], lower.tail = FALSE) * 2)

#ZACK
paramFix_z <- summary(mod_full_z_cross_int)$coefficients
pointerLag_z <- grep("log_lag_flow",rownames(paramFix_z))

as.data.frame(pt(q = abs((1 - paramFix_z[pointerLag_z,1])/paramFix_z[pointerLag_z,2]), 
                 df =  paramFix_z[pointerLag_z,3], lower.tail = FALSE) * 2)
##----------------------------------------------------------------------------




# 4 - Forest PLOT FULL INT MOD -----------------------------------------------
# FOREST PLOT 
# High Arctic Zackenberg
FP_zackInt <- sjPlot::plot_model(mod_full_z_cross_int, 
                                 type = "est",
                                 show.values=T, 
                                 show.intercept = FALSE,
                                 show.p = TRUE,
                                 vline.color="grey", 
                                 value.offset=-0.3, 
                                 title = "Forest plot Zackenberg sans 0") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10, face="bold"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", color="black")) 

# Low Arctic - Nuuk
FP_NuukInt <- sjPlot::plot_model(mod_full_n_cross_int, 
                                 type = "est",
                                 show.values=T, 
                                 show.intercept = FALSE,
                                 show.p = TRUE,
                                 vline.color="grey", 
                                 value.offset=-0.3, 
                                 title = "Forest plot Nuuk - sans 0") +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=10, face="bold"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", color="black"))

#panel
gridExtra::grid.arrange(FP_zackInt, FP_NuukInt, ncol=2)


pdf("results/Models/FP_all_sans_zero.pdf",         # File name
    width = 12, height = 10, # Width and height in inches
    bg = "white",          # Background color
    colormodel = "cmyk")
    # Color model (cmyk is required for  publications)
    # paper = "A4")          # Paper size

# Creating a plot
gridExtra::grid.arrange(FP_zackInt, FP_NuukInt, ncol=2)

# Closing the graphical device
dev.off() 



# 
# #PDF SAVING 
# # Customizing the output
# pdf("results/Models/FP_nuuk.pdf",         # File name
#     width = 9, height = 8, # Width and height in inches
#     bg = "white",          # Background color
#     colormodel = "cmyk",    # Color model (cmyk is required for  publications)
#     paper = "A4")          # Paper size
# 
# # Creating a plot
# FP_NuukInt
# 
# # Closing the graphical device
# dev.off() 
# 
# pdf("results/Models/FP_zack.pdf",         # File name
#     width = 9, height = 8, # Width and height in inches
#     bg = "white",          # Background color
#     colormodel = "cmyk",  
#     paper = "A4") 
# 
# # Creating a plot
# FP_zackInt
# 
# # Closing the graphical device
# dev.off() 
# ##----------------------------------------------------------------------------
# 
# 
# 
# 
# 
# 
# # 1.2 Exploratory graphs DOY ~ FLow density  ---------------------------------
# #Zack
# cor_doy_flow_z <- ggplot(flow_snow_clim_z, 
#                          aes(snowmelt_DOY, log_flow, group=Species, color=Species)) +
#   geom_point() +
#   geom_smooth(method='loess', se=TRUE) +
#   labs(y='Flower density / m2') +
#   ggtitle('Zackenberg') +
#   facet_grid(Species~., scales="free_y") +
#   theme_linedraw() 
# 
# #Nuuk
# cor_doy_flow_n <- ggplot(flow_snow_clim_n, 
#                          aes(snowmelt_DOY, log_flow, group=Species, color=Species)) +
#   geom_point() +
#   geom_smooth(method='loess', se=TRUE) +
#   labs(y='Flower density / m2') +
#   ggtitle('Nuuk') +
#   facet_grid(Species~., scales="free_y") +
#   theme_linedraw() 
# 
# #  graph
# gridExtra::grid.arrange(cor_doy_flow_z, cor_doy_flow_n, ncol=2, 
#                         top = "Snowmelt_DOY ~ flowering density")
# ##----------------------------------------------------------------------------
