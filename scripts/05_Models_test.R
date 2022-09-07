##############################################################################
#
#                           Greenland Flowering project
#                            05 - Models Exploration
#
##############################################################################
# Antoine Becker-Scarpitta 
# July 2022

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
#compile in df
data.frame(rownames(mod_z), 
           round(mod_z$`F value`, 2), 
           mod_z$NumDF, 
           mod_z$`Pr(>F)`)

#Nuuk ANOVA(mod_sp)
mod_n <- anova(mod_basic_n)
#compile in df
data.frame(rownames(mod_n), 
           round(mod_n$`F value`, 2), 
           mod_n$NumDF, 
           mod_n$`Pr(>F)`)
##----------------------------------------------------------------------------


# 3 - MODELS TABS FULL --------------------------------------------------------
# full mod Zack + Nuuk
tab_model(mod_full_z_cross_int, mod_full_n_cross_int,
          p.val = "kr", 
          show.df = TRUE, 
          dv.labels = c("Full int Zack", "Full int Nuuk"))


# 1 - corrected value of the Offset
# extract value
intZ <- summary(mod_full_z)$coefficients
intN <- summary(mod_full_n)$coefficients

#correct offet: -beta
intZ[grep("log_lag_flow",rownames(intZ)),1] <- -intZ[grep("log_lag_flow",rownames(intZ)),1]
intN[grep("log_lag_flow",rownames(intN)),1] <- -intN[grep("log_lag_flow",rownames(intN)),1]


# 2- Pvalue and distribution offset
#NUUK
paramFix_n <- summary(mod_full_n)$coefficients
pointerLag_n <- grep("log_lag_flow", rownames(paramFix_n))

as.data.frame(pt(q = abs((1 - paramFix_n[pointerLag_n,1])/paramFix_n[pointerLag_n,2]), 
                 df =  paramFix_n[pointerLag_n,3], lower.tail = FALSE) * 2)

#ZACK
paramFix_z <- summary(mod_full_z)$coefficients
pointerLag_z <- grep("log_lag_flow", rownames(paramFix_z))

as.data.frame(pt(q = abs((1 - paramFix_z[pointerLag_z,1])/paramFix_z[pointerLag_z,2]), 
                 df =  paramFix_z[pointerLag_z,3], lower.tail = FALSE) * 2)
##----------------------------------------------------------------------------



# # 4 - Forest PLOT FULL INT MOD -----------------------------------------------
# # FOREST PLOT 
# # High Arctic Zackenberg
# FP_zackInt <- sjPlot::plot_model(mod_full_z_cross_int, 
#                                  type = "est",
#                                  show.values=T, 
#                                  show.intercept = FALSE,
#                                  show.p = TRUE,
#                                  vline.color="grey", 
#                                  value.offset=-0.3, 
#                                  title = "Forest plot Zackenberg") +
#   theme(axis.text=element_text(size=10),
#         axis.title=element_text(size=10, face="bold"),
#         panel.background = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.line = element_line(colour = "black"), 
#         strip.background = element_blank(),
#         strip.text = element_text(face = "bold", color="black")) 
# 
# # Low Arctic - Nuuk
# FP_NuukInt <- sjPlot::plot_model(mod_full_n_cross_int, 
#                                  type = "est",
#                                  show.values=T, 
#                                  show.intercept = FALSE,
#                                  show.p = TRUE,
#                                  vline.color="grey", 
#                                  value.offset=-0.3, 
#                                  title = "Forest plot Nuuk - sans 0") +
#   theme(axis.text=element_text(size=10),
#         axis.title=element_text(size=10, face="bold"),
#         panel.background = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.line = element_line(colour = "black"), 
#         strip.background = element_blank(),
#         strip.text = element_text(face = "bold", color="black"))
# 
# #panel
# gridExtra::grid.arrange(FP_zackInt, FP_NuukInt, ncol=2)
# 
# 
# pdf("results/Models/FP_all_sans_zero.pdf",         # File name
#     width = 12, height = 10, # Width and height in inches
#     bg = "white",          # Background color
#     colormodel = "cmyk")
#     # Color model (cmyk is required for  publications)
#     # paper = "A4")          # Paper size
# 
# # Creating a plot
# gridExtra::grid.arrange(FP_zackInt, FP_NuukInt, ncol=2)
# 
# # Closing the graphical device
# dev.off() 
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
##----------------------------------------------------------------------------
