##############################################################################
#
#                           Greenland Flowering project
#                            05 - Models Graphs
#
##############################################################################
# Antoine Becker-Scarpitta 
# June 2022 


# 1.1 - LOAD MODELS ----------------------------------------------------------
#  clean R work-space
rm(list=ls())

# Run mods
{source("Scripts/04_Analysis_Zack.r")
source("Scripts/04_Analysis_Nuuk.r")
##---------------------------------------------------------------------------


# ## PLOT SnowMelt DOY 
# #----------------------------------------------------------------------------
# temp_graph_n <- ggplot(flow_snow_clim_n,
#                        aes(x=Year, 
#                            y=snowmelt_DOY, 
#                            group=Species, 
#                            color=Species)) +
#   geom_point(size=1.2) +
#   geom_smooth(method='lm', se=F) +
#   theme(axis.text=element_text(size=15),
#         axis.title=element_text(size=16,face="bold"),
#         panel.background = element_blank(),
#         axis.line = element_line(colour = "black"))
# 
# ## PLOT SnowMelt DOY Zackenberg
# temp_graph_z <- ggplot(flow_snow_clim_z , 
#                        aes(x=Year, y=snowmelt_DOY, 
#                            group=Species, 
#                            color=Species)) +
#   geom_point(size=1.2) +
#   geom_smooth(method='lm', se=F) +
#   theme(axis.text=element_text(size=15),
#         axis.title=element_text(size=16,face="bold"),
#         panel.background = element_blank(),
#         axis.line = element_line(colour = "black"))
##---------------------------------------------------------------------------


# # Species level mod + graphs
# #----------------------------------------------------------------------------
# Zack
{z_cas <- lmer(log_flow ~ Year + (1|Plot),
              data=flow %>%
                filter(Site=="Zackenberg", Species=="CAS") %>%
                mutate(., log_flow=log1p(Flow_m2)))
z_dry <- lmer(log_flow ~ Year + (1|Plot),
              data=flow %>%
                filter(Site=="Zackenberg", Species=="DRY") %>%
                mutate(., log_flow=log1p(Flow_m2)))
z_pap <- lmer(log_flow ~ Year + (1|Plot),
              data=flow %>%
                filter(Site=="Zackenberg", Species=="PAP") %>%
                mutate(., log_flow=log1p(Flow_m2)))
z_salf <- lmer(log_flow ~ Year + (1|Plot),
              data=flow %>%
                filter(Site=="Zackenberg", Species=="SAL_female") %>%
                mutate(., log_flow=log1p(Flow_m2)))
z_salm <- lmer(log_flow ~ Year + (1|Plot),
              data=flow %>%
                filter(Site=="Zackenberg", Species=="SAL_male") %>%
                mutate(., log_flow=log1p(Flow_m2)))
z_sax <- lmer(log_flow ~ Year + (1|Plot),
              data=flow %>%
                filter(Site=="Zackenberg", Species=="SAX") %>%
                mutate(., log_flow=log1p(Flow_m2)))
z_sil <- lmer(log_flow ~ Year + (1|Plot),
              data=flow %>%
                filter(Site=="Zackenberg", Species=="SIL") %>%
                mutate(., log_flow=log1p(Flow_m2)))
# Nuuk
n_eri <- lmer(log_flow ~ Year + (1|Plot),
              data=flow %>%
                filter(Site=="Nuuk", Species=="ERI") %>%
                mutate(., log_flow=log1p(Flow_m2)))
n_loi <- lmer(log_flow ~ Year + (1|Plot),
              data=flow %>%
                filter(Site=="Nuuk", Species=="LOI") %>%
                mutate(., log_flow=log1p(Flow_m2)))

n_salf <- lmer(log_flow ~ Year + (1|Plot),
              data=flow %>%
                filter(Site=="Nuuk", Species=="SAL_female") %>%
                mutate(., log_flow=log1p(Flow_m2)))
n_salm <- lmer(log_flow ~ Year + (1|Plot),
              data=flow %>%
                filter(Site=="Nuuk", Species=="SAL_male") %>%
                mutate(., log_flow=log1p(Flow_m2)))
n_sil <- lmer(log_flow ~ Year + (1|Plot),
              data=flow %>%
                filter(Site=="Nuuk", Species=="SIL") %>%
                mutate(., log_flow=log1p(Flow_m2)))}

summary(z_cas)$coefficient
summary(z_dry)$coefficient
summary(z_pap)$coefficient
summary(z_salf)$coefficient
summary(z_salm)$coefficient
summary(z_sax)$coefficient
summary(z_sil)$coefficient
summary(n_eri)$coefficient
summary(n_loi)$coefficient
summary(n_salf)$coefficient
summary(n_salm)$coefficient
summary(n_sil)$coefficient




#compile data per site
#ZACKENBERG
#extract the data used in mod_basic_z
data_mod_z <- as.data.frame(mod_basic_z@frame)
#extract the predict from mod_basic_z
data_mod_z$pred <- predict(mod_basic_z, re.form=NULL)  ## population level

#NUUK
#extract the data used in mod_basic_n
data_mod_n <- as.data.frame(mod_basic_n@frame)
#extract the predict from mod_basic_n
data_mod_n$pred <- predict(mod_basic_n, re.form=NULL)  ## population level

#compile data base
data_mod_n$Site <- "Low_Arctic"
data_mod_z$Site <- "High_Arctic"
data_mod_all <- rbind(data_mod_z, data_mod_n)

#change str
data_mod_all[,c("Site","Species")]<-lapply(data_mod_all[,c("Site","Species")],as.factor)




#### GRAPH PER SPECIES
#CAS
HA_CAS <- ggplot(data_mod_all %>% 
                   dplyr::filter(Site=="High_Arctic", 
                                 Species=="CAS"),
                 aes(Year, log_flow)) +
  geom_point(size=1.2, color="#db6400") +
  geom_smooth(linetype='dashed', method='lm', se=TRUE, 
              aes(group=Species), color="#db6400", fill="#db6400", 
              alpha = 0.3) + #plot trends
  labs(y='log(flower density)', x=" ") +
  ggtitle('Cassiope tetragona') +
  theme_linedraw() +
  scale_x_continuous(limits = c(1997, 2019),
                     breaks = c(1997, 2002, 2007, 2012, 2017)) +
  theme(legend.position = "none", 
        axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(color="black"),
        plot.title = element_text(size=9))

#DRY
HA_DRY <- ggplot(data_mod_all %>% 
                   dplyr::filter(Site=="High_Arctic", 
                                 Species=="DRY"),
                 aes(Year, log_flow)) +
  geom_point(size=1.2, color="#fda62c#") +
  geom_smooth(linetype='dashed', method='lm', se=TRUE, 
              aes(group=Species), color="#fda62c#", fill="#fda62c#", 
              alpha = 0.3) + #plot trends
  labs(y=' ', x=" ") +
  ggtitle('Dryas intergrifolia/octopetala') +
  theme_linedraw() +
  scale_x_continuous(limits = c(1997, 2019),
                     breaks = c(1997, 2002, 2007, 2012, 2017)) +
  theme(legend.position = "none", 
        axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(color="black"),
        plot.title = element_text(size=9))

# PAP
HA_PAP <- ggplot(data_mod_all %>% 
                   dplyr::filter(Site=="High_Arctic", 
                                 Species=="PAP"),
                 aes(Year, log_flow)) +
  geom_point(size=1.2, color="#bedbbd") +
  geom_smooth(method='lm', se=TRUE, 
              aes(group=Species), color="#bedbbd", fill="#bedbbd", 
              alpha = 0.3) + #plot trends
  labs(y=' ', x=" ") +
  ggtitle('Papaver radicatum') +
  theme_linedraw() +
  scale_x_continuous(limits = c(1997, 2019),
                     breaks = c(1997, 2002, 2007, 2012, 2017)) +
  theme(legend.position = "none", 
        axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(color="black"),
        plot.title = element_text(size=9))

#SAL_female
HA_SAL_female <- ggplot(data_mod_all %>% 
                          dplyr::filter(Site=="High_Arctic", 
                                        Species=="SAL_female"),
                        aes(Year, log_flow)) +
  geom_point(size=1.2, color="#d20b72") +
  geom_smooth(linetype='dashed', method='lm', se=TRUE, 
              aes(group=Species), color="#d20b72", fill="#d20b72", 
              alpha = 0.3) + #plot trends
  labs(y=' ', x=" ") +
  ggtitle('Salix arctica female') +
  theme_linedraw() +
  scale_x_continuous(limits = c(1997, 2019),
                     breaks = c(1997, 2002, 2007, 2012, 2017)) +
  theme(legend.position = "none", 
        axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(color="black"),
        plot.title = element_text(size=9))

# SAL_male
HA_SAL_male <- ggplot(data_mod_all %>% 
                        dplyr::filter(Site=="High_Arctic", 
                                      Species=="SAL_male"),
                      aes(Year, log_flow)) +
  geom_point(size=1.2, color="#e6729f") +
  geom_smooth(linetype='dashed', method='lm', se=TRUE, 
              aes(group=Species), color="#e6729f", fill="#e6729f", 
              alpha = 0.3) + #plot trends
  labs(y=' ', x=" ") +
  ggtitle('Salix arctica male') +
  theme_linedraw() +
  scale_x_continuous(limits = c(1997, 2019),
                     breaks = c(1997, 2002, 2007, 2012, 2017)) +
  theme(legend.position = "none", 
        axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(color="black"),
        plot.title = element_text(size=9))

# SAX
HA_SAX <- ggplot(data_mod_all %>% 
                   dplyr::filter(Site=="High_Arctic", 
                                 Species=="SAX"),
                 aes(Year, log_flow)) +
  geom_point(size=1.2, color="#8ccccb") +
  geom_smooth(method='lm', se=TRUE, 
              aes(group=Species), color="#8ccccb", fill="#8ccccb", 
              alpha = 0.3) + #plot trends
  labs(y=' ') +
  ggtitle('Saxifraga oppositifolia') +
  theme_linedraw() +
  scale_x_continuous(limits = c(1997, 2019),
                     breaks = c(1997, 2002, 2007, 2012, 2017)) +
  theme(legend.position = "none", 
        axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(color="black"),
        plot.title = element_text(size=9))

# SIL
HA_SIL <- ggplot(data_mod_all %>% 
                   dplyr::filter(Site=="High_Arctic", 
                                 Species=="SIL"),
                 aes(Year, log_flow)) +
  geom_point(size=1.2, color="#16697b") +
  geom_smooth(linetype='dashed', method='lm', se=TRUE, 
              aes(group=Species), color="#16697b", fill="#16697b", 
              alpha = 0.3) + #plot trends
  labs(y=' ', x=" ") +
  ggtitle('Silene acaulis') +
  theme_linedraw() +
  scale_x_continuous(limits = c(1997, 2019),
                     breaks = c(1997, 2002, 2007, 2012, 2017)) +
  theme(legend.position = "none", 
        axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(color="black"),
        plot.title = element_text(size=9))

# SAL_female
LA_SAL_female <- ggplot(data_mod_all %>% 
                          dplyr::filter(Site=="Low_Arctic", 
                                        Species=="SAL_female"),
                        aes(Year, log_flow)) +
  geom_point(size=1.2, color="#d20b72") +
  geom_smooth(method='lm', se=TRUE, 
              aes(group=Species), color="#d20b72", fill="#d20b72", 
              alpha = 0.3) + #plot trends
  labs(y=' ', x=" ") +
  ggtitle('Salix glauca female') +
  theme_linedraw() +
  scale_x_continuous(limits = c(2009, 2019),
                     breaks = c(2009, 2011, 2013, 2015, 2017, 2019)) +
  theme(legend.position = "none", 
        axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(color="black"),
        plot.title = element_text(size=9))

# SAL_male
LA_SAL_male <- ggplot(data_mod_all %>% 
                        dplyr::filter(Site=="Low_Arctic", 
                                      Species=="SAL_male"),
                      aes(Year, log_flow)) +
  geom_point(size=1.2, color="#e6729f") +
  geom_smooth(linetype='dashed', method='lm', se=TRUE, 
              aes(group=Species), color="#e6729f", fill="#e6729f", 
              alpha = 0.3) + #plot trends
  labs(y=' ', x=" ") +
  ggtitle('Salix glauca male') +
  theme_linedraw() +
  scale_x_continuous(limits = c(2009, 2019),
                     breaks = c(2009, 2011, 2013, 2015, 2017, 2019)) +
  theme(legend.position = "none", 
        axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(color="black"),
        plot.title = element_text(size=9))

# SIL
LA_SIL <- ggplot(data_mod_all %>% 
                   dplyr::filter(Site=="Low_Arctic", 
                                 Species=="SIL"),
                 aes(Year, log_flow)) +
  geom_point(size=1.2, color="#16697b") +
  geom_smooth(method='lm', se=TRUE, 
              aes(group=Species), color="#16697b", fill="#16697b", 
              alpha = 0.3) + #plot trends
  labs(y=' ', x=" ") +
  ggtitle('Silene acaulis') +
  theme_linedraw() +
  scale_x_continuous(limits = c(2009, 2019),
                     breaks = c(2009, 2011, 2013, 2015, 2017, 2019)) +
  theme(legend.position = "none", 
        axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(color="black"),
        plot.title = element_text(size=9))

# ERI
LA_ERI <-  ggplot(data_mod_all %>% 
                    dplyr::filter(Site=="Low_Arctic", 
                                  Species=="ERI"),
                  aes(Year, log_flow)) +
  geom_point(size=1.2, color="#f05455") +
  geom_smooth(linetype='dashed', method='lm', se=TRUE, 
              aes(group=Species), color="#f05455", fill="#f05455", 
              alpha = 0.3) + #plot trends
  labs(y='log(flower density)', x=" ") +
  ggtitle('Eriophorum angustifolium') +
  theme_linedraw() +
  scale_x_continuous(limits = c(2009, 2019),
                     breaks = c(2009, 2011, 2013, 2015, 2017, 2019)) +
  theme(legend.position = "none", 
        axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(color="black"),
        plot.title = element_text(size=9))

# LOI
LA_LOI <- ggplot(data_mod_all %>% 
                   dplyr::filter(Site=="Low_Arctic", 
                                 Species=="LOI"),
                 aes(Year, log_flow)) +
  geom_point(size=1.2, color="#fecf47") +
  geom_smooth(method='lm', se=TRUE, 
              aes(group=Species), color="#fecf47", fill="#fecf47", 
              alpha = 0.3) + #plot trends
  labs(y=' ') +
  ggtitle('Loiseleuria procumbens') +
  theme_linedraw() +
  scale_x_continuous(limits = c(2009, 2019),
                     breaks = c(2009, 2011, 2013, 2015, 2017, 2019)) +
  theme(legend.position = "none", 
        axis.text=element_text(size=8),
        axis.title=element_text(size=8),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(color="black"),
        plot.title = element_text(size=9))

# plot all graph in layout
gridExtra::grid.arrange(HA_SAL_female, LA_SAL_female,
                        HA_SAL_male, LA_SAL_male, 
                        HA_SIL,LA_SIL,
                        HA_CAS, LA_ERI,
                        HA_PAP, LA_LOI,
                        HA_SAX,
                        ncol=2)}


## GRAPH SIMPLE
# pdf("results/Models/Sp_level_plot_MEP.pdf",         # File name
#     width = 4, height = 10, # Width and height in inches
#     bg = "white",          # Background color
#     colormodel = "cmyk",    # Color model (cmyk is required for  publications)
#     paper = "A4")          # Paper size
# 
# # Creating a plot
# gridExtra::grid.arrange(HA_SAL_female, LA_SAL_female,
#                         HA_SAL_male, LA_SAL_male,
#                         HA_SIL,LA_SIL,
#                         HA_CAS, LA_ERI,
#                         HA_PAP, LA_LOI,
#                         HA_SAX,
#                         ncol=2)
# # Closing the graphical device
# dev.off()

