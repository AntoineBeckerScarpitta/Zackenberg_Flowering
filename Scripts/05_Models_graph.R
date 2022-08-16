##############################################################################
#
#                           Greenland Flowering project
#                            05 - Models Graphs
#
##############################################################################
# Antoine Becker-Scarpitta & Laura Antao
# June 2021


# 1.1 - LOAD MODELS ----------------------------------------------------------
#  clean R work-space
rm(list=ls())

library(dplyr)

# Run mods
source("Scripts/04_Analysis_Zack.r")
source("Scripts/04_Analysis_Nuuk.r")
##----------------------------------------------------------------------------


#data per site
#ZACKENBERG
#extract the data used in mod_basic_z
data_mod_z <- as.data.frame(mod_basic_z@frame)
#extract the predict from mod_basic_z
data_mod_z$pred <- predict(mod_basic_z, re.form=NA)  ## population level

#NUUK
#extract the data used in mod_basic_n
data_mod_n <- as.data.frame(mod_basic_n@frame)
#extract the predict from mod_basic_n
data_mod_n$pred <- predict(mod_basic_n, re.form=NA)  ## population level

#data base
data_mod_n$Site <- "Low_Arctic"
data_mod_z$Site <- "High_Arctic"
data_mod_all <- rbind(data_mod_z, data_mod_n)

data_mod_all$Species <- as.factor(data_mod_all$Species)
data_mod_all$Site <- as.factor(data_mod_all$Site)


# #graph
# ggplot(data_mod_all, aes(Year, log_flow, 
#                          group=Species, color=Species)) +
#   geom_point(size=1) +
#   # geom_smooth(method='lm', se=TRUE, aes(fill=Plot), alpha = 0.1) + #plot trends
#   geom_smooth(method='loess', se=FALSE, colour="black", 
#               aes(y=pred, group=Species)) + # sp prediction
#   labs(y='log(flower density)') +
#   ggtitle('High Arctic - Zackenberg') +
#   facet_grid(Species~Site, scales="fixed") +
#   theme_linedraw() +
#   # scale_x_continuous(limits = c(1997, 2019),
#   #                    breaks = c(1997, 2002, 2007, 2012, 2017)) +
#   theme(legend.position = "none", 
#         axis.text=element_text(size=9),
#         axis.title=element_text(size=9, face="bold"),
#         panel.background = element_blank(),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         axis.line = element_line(colour = "black"), 
#         strip.background = element_blank(),
#         strip.text = element_text(face = "bold", color="black"))






#### PER SPECIES
#CAS
HA_CAS <- ggplot(data_mod_all %>% dplyr::filter(Site=="High_Arctic", 
                                      Species=="CAS"),
       aes(Year, log_flow)) +
  geom_point(size=1) +
  # geom_smooth(method='lm', se=TRUE, aes(fill=Plot), alpha = 0.1) + #plot trends
  geom_smooth(method='loess', se=FALSE, colour="black", 
              aes(y=pred)) + # sp prediction
  labs(y='log(flower density)') +
  ggtitle('High Arctic - CAS') +
  theme_linedraw() +
  # scale_x_continuous(limits = c(1997, 2019),
  #                    breaks = c(1997, 2002, 2007, 2012, 2017)) +
  scale_y_continuous(limits = c(-10, 10)) +
  theme(legend.position = "none", 
        axis.text=element_text(size=5),
        axis.title=element_text(size=5),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(color="black"))

#DRY
HA_CAS <- ggplot(data_mod_all %>% dplyr::filter(Site=="High_Arctic", 
                                                Species=="DRY"),
                 aes(Year, log_flow)) +
  geom_point(size=1) +
  geom_smooth(method='loess', se=FALSE, colour="black", 
              aes(y=pred)) + # sp prediction
  ggtitle('High Arctic - DRY') +
  theme_linedraw() +
  # scale_x_continuous(limits = c(1997, 2019),
  #                    breaks = c(1997, 2002, 2007, 2012, 2017)) +
  scale_y_continuous(limits = c(-10, 10)) +
  theme(legend.position = "none", 
        axis.text=element_text(size=5),
        axis.title=element_text(size=5),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(color="black"))


# PAP
HA_PAP <- ggplot(data_mod_all %>% dplyr::filter(Site=="High_Arctic", 
                                                Species=="PAP"),
                 aes(Year, log_flow)) +
  geom_point(size=1) +
  geom_smooth(method='loess', se=FALSE, colour="black", 
              aes(y=pred)) + # sp prediction
  ggtitle('High Arctic - CAS') +
  theme_linedraw() +
  # scale_x_continuous(limits = c(1997, 2019),
  #                    breaks = c(1997, 2002, 2007, 2012, 2017)) +
  scale_y_continuous(limits = c(-10, 10)) +
  theme(legend.position = "none", 
        axis.text=element_text(size=5),
        axis.title=element_text(size=5),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(color="black"))


#SAL_female
HA_SAL_female <- ggplot(data_mod_all %>% dplyr::filter(Site=="High_Arctic", 
                                                Species=="SAL_female"),
                 aes(Year, log_flow)) +
  geom_point(size=1) +
  geom_smooth(method='loess', se=FALSE, colour="black", 
              aes(y=pred)) + # sp prediction
  ggtitle('High Arctic - SAL_female') +
  theme_linedraw() +
  # scale_x_continuous(limits = c(1997, 2019),
  #                    breaks = c(1997, 2002, 2007, 2012, 2017)) +
  scale_y_continuous(limits = c(-10, 10)) +
  theme(legend.position = "none", 
        axis.text=element_text(size=5),
        axis.title=element_text(size=5),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(color="black"))


# SAL_male
HA_SAL_male <- ggplot(data_mod_all %>% dplyr::filter(Site=="High_Arctic", 
                                                       Species=="SAL_male"),
                        aes(Year, log_flow)) +
  geom_point(size=1) +
  geom_smooth(method='loess', se=FALSE, colour="black", 
              aes(y=pred)) + # sp prediction
  ggtitle('High Arctic - SAL_male') +
  theme_linedraw() +
  # scale_x_continuous(limits = c(1997, 2019),
  #                    breaks = c(1997, 2002, 2007, 2012, 2017)) +
  scale_y_continuous(limits = c(-10, 10)) +
  theme(legend.position = "none", 
        axis.text=element_text(size=5),
        axis.title=element_text(size=5),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(color="black"))

# SAX
HA_SAX <- ggplot(data_mod_all %>% dplyr::filter(Site=="High_Arctic", 
                                                     Species=="SAX"),
                      aes(Year, log_flow)) +
  geom_point(size=1) +
  geom_smooth(method='loess', se=FALSE, colour="black", 
              aes(y=pred)) + # sp prediction
  ggtitle('High Arctic - SAX') +
  theme_linedraw() +
  # scale_x_continuous(limits = c(1997, 2019),
  #                    breaks = c(1997, 2002, 2007, 2012, 2017)) +
  scale_y_continuous(limits = c(-10, 10)) +
  theme(legend.position = "none", 
        axis.text=element_text(size=5),
        axis.title=element_text(size=5),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(color="black"))

# SIL
HA_SIL <- ggplot(data_mod_all %>% dplyr::filter(Site=="High_Arctic", 
                                                     Species=="SIL"),
                      aes(Year, log_flow)) +
  geom_point(size=1) +
  geom_smooth(method='loess', se=FALSE, colour="black", 
              aes(y=pred)) + # sp prediction
  ggtitle('High Arctic - SIL') +
  theme_linedraw() +
  # scale_x_continuous(limits = c(1997, 2019),
  #                    breaks = c(1997, 2002, 2007, 2012, 2017)) +
  scale_y_continuous(limits = c(-10, 10)) +
  theme(legend.position = "none", 
        axis.text=element_text(size=5),
        axis.title=element_text(size=5),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(color="black"))



# SAL_female
LA_SAL_female <- ggplot(data_mod_all %>% dplyr::filter(Site=="Low_Arctic", 
                                                     Species=="SAL_female"),
                      aes(Year, log_flow)) +
  geom_point(size=1) +
  geom_smooth(method='loess', se=FALSE, colour="black", 
              aes(y=pred)) + # sp prediction
  ggtitle('Low Arctic - SAL_female') +
  theme_linedraw() +
  # scale_x_continuous(limits = c(1997, 2019),
  #                    breaks = c(1997, 2002, 2007, 2012, 2017)) +
  scale_y_continuous(limits = c(-10, 10)) +
  theme(legend.position = "none", 
        axis.text=element_text(size=5),
        axis.title=element_text(size=5),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(color="black"))

# SAL_male
LA_SAL_male <- ggplot(data_mod_all %>% dplyr::filter(Site=="Low_Arctic", 
                                                       Species=="SAL_male"),
                        aes(Year, log_flow)) +
  geom_point(size=1) +
  geom_smooth(method='loess', se=FALSE, colour="black", 
              aes(y=pred)) + # sp prediction
  ggtitle('Low Arctic - SAL_male') +
  theme_linedraw() +
  # scale_x_continuous(limits = c(1997, 2019),
  #                    breaks = c(1997, 2002, 2007, 2012, 2017)) +
  scale_y_continuous(limits = c(-10, 10)) +
  theme(legend.position = "none", 
        axis.text=element_text(size=5),
        axis.title=element_text(size=5),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(color="black"))

# SIL
LA_SIL <- ggplot(data_mod_all %>% dplyr::filter(Site=="Low_Arctic", 
                                                     Species=="SIL"),
                      aes(Year, log_flow)) +
  geom_point(size=1) +
  geom_smooth(method='loess', se=FALSE, colour="black", 
              aes(y=pred)) + # sp prediction
  ggtitle('Low Arctic - SIL') +
  theme_linedraw() +
  # scale_x_continuous(limits = c(1997, 2019),
  #                    breaks = c(1997, 2002, 2007, 2012, 2017)) +
  scale_y_continuous(limits = c(-10, 10)) +
  theme(legend.position = "none", 
        axis.text=element_text(size=5),
        axis.title=element_text(size=5),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(color="black"))

# ERI
LA_ERI <- ggplot(data_mod_all %>% dplyr::filter(Site=="Low_Arctic", 
                                                Species=="ERI"),
                 aes(Year, log_flow)) +
  geom_point(size=1) +
  geom_smooth(method='loess', se=FALSE, colour="black", 
              aes(y=pred)) + # sp prediction
  ggtitle('Low Arctic - ERI') +
  theme_linedraw() +
  # scale_x_continuous(limits = c(1997, 2019),
  #                    breaks = c(1997, 2002, 2007, 2012, 2017)) +
  scale_y_continuous(limits = c(-10, 10)) +
  theme(legend.position = "none", 
        axis.text=element_text(size=5),
        axis.title=element_text(size=5),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(color="black"))

# LOI
LA_LOI <- ggplot(data_mod_all %>% dplyr::filter(Site=="Low_Arctic", 
                                                Species=="LOI"),
                 aes(Year, log_flow)) +
  geom_point(size=1) +
  geom_smooth(method='loess', se=FALSE, colour="black", 
              aes(y=pred)) + # sp prediction
  ggtitle('Low Arctic - LOI') +
  theme_linedraw() +
  # scale_x_continuous(limits = c(1997, 2019),
  #                    breaks = c(1997, 2002, 2007, 2012, 2017)) +
  scale_y_continuous(limits = c(-10, 10)) +
  theme(legend.position = "none", 
        axis.text=element_text(size=5),
        axis.title=element_text(size=5),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(color="black"))


gridExtra::grid.arrange(HA_CAS, NA, 
                        HA_PAP, NA, 
                        HA_SAL_female, NA, 
                        HA_SAL_male, NA,
                        HA_SAX, NA, 
                        HA_SIL, NA, 
                        LA_ERI, 
                        LA_LOI, 
                        LA_SAL_female, 
                        LA_SAL_male, 
                        LA_SIL,
                        ncol=2)







