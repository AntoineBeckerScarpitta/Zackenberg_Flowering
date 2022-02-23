###############################################################################
#
#                           Greenland Flowering project
#                         03 - Basic Explorations Graph
#
###############################################################################
# Antoine Becker-Scarpitta
# September 2020

#  clean R work-space
# rm(list=ls())

# Load 02 - Creation database (load scripts 00 and 01)
source("Scripts/02_Creation_DB.R")
source("Scripts/C1_Load_climatic_covariates.R")
source("Scripts/C2_Build_all_snow_covariates.R")


# Total number of plot per year Nuuk and Zackenberg
# write.csv(table(flow[flow$Site=="Nuuk", "Year"], 
#                 flow[flow$Site=="Nuuk", "Species"]), 
#           "results/Plot_num_year_Nuuk.csv")
# write.csv(table(flow[flow$Site=="Zackenberg", "Year"],
#                 flow[flow$Site=="Zackenberg", "Species"]), 
#           "results/Plot_num_year_Zack.csv")



#  TEMPORAL TRENDS AT PLOT LEVEL 
# 1 - Flower density per m2  ## --------------------------------------------
flow_log_nn <- ggplot(flow, aes(Year, log(trans_Flow_m2), 
                                group=Plot, color=Plot)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE) +
  labs(y='log(Flower density / m2)') +
  ggtitle('Plot-level') +
  facet_grid(Site+Species~., scales="free_y")+
  theme_linedraw() +
  theme(legend.position = "none") 
### END PLOT LEVEL ## ------------------------------------------------------


#  TEMPORAL TRENDS AT SPECIE LEVEL 
# 2 - Total Flower per m2  ## --------------------------------------------
flow_den2_log_nn <- ggplot(flow, aes(Year, log(trans_Flow_m2), 
                                     group=Species, color=Species)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE) +
  labs(y='log(Flower density/m2)') +
  ggtitle('Species-level') +
  facet_grid(Site+Species~., scales="free_y")+
  theme_linedraw() +
  theme(legend.position = "none")

#  graph
gridExtra::grid.arrange(flow_log_nn, flow_den2_log_nn, ncol=2,
          top = "Temporal trends of flowering density")
### END SP LEVEL ## -----------------------------------------------------





# DISTRIBUTION OF FLOWERING VARIABLES 
# DENSITY
distdens <- ggplot(flow, aes(x=Flow_m2, fill=Species)) + 
  geom_density(alpha=.3) +
  labs(y='Density', x='Flowering density/m2') +
  ggtitle('A - Distribution of Flowering density') +
  facet_wrap(.~Site+Species, scales="free") +
  theme_linedraw() +
  theme(legend.position = "none")

# log(density) !=0 (Flow_m2 + 0.0001 in 02_Creation_DB, line 131)
dist_logdens_nn <- ggplot(flow, aes(x=log(trans_Flow_m2), fill=Species)) + 
  geom_density(alpha=.3) +
  labs(y='Density', x='log(Flowering density/m2)') +
  ggtitle('B - Distribution of log(Flowering density) !=0') +
  facet_wrap(Site+Species~ ., scales="free") +
  theme_linedraw() +
  theme(legend.position = "none")

#  graph
gridExtra::grid.arrange(distdens, dist_logdens_nn, ncol=2, 
                        top = "Distribution of flowering variable per species")
### END DISTRI PLOTS  ## -------------------------------------------------


# for models
# data distribution, flo density, log(flow density)
par(mfrow=c(2,2))
hist(flow[flow$Site=="Nuuk", "Flow_m2"], 
     main='A - Nuuk flowering density', xlab="Flower m2")
hist(flow[flow$Site=="Zackenberg", "Flow_m2"], 
     main='B - Zackenberg flowering density', xlab="Flower m2")
hist(log(flow[flow$Site=="Nuuk", "trans_Flow_m2"]), 
     main='C - Nuuk log(flow density)', xlab="log(Flower m2)")
hist(log(flow[flow$Site=="Zackenberg", "trans_Flow_m2"]), 
     main='D - Zackenberg log(flow density)', xlab="log(Flower m2)")
par(mfrow=c(1,1))





# CLIMATIC TRENDS
# Plot Climatic trends    ## ---------------------------------------------
# YEAR TRENDS
ggplot(clim_year, aes(Year, Value, group=Site, color=Site)) +
  scale_color_brewer(palette="Set1")+
  geom_point() +
  geom_smooth(aes(group=Site, color=Site, fill=Site), method='lm', se=TRUE) +
  labs(y='Value') +
  ggtitle('Temporal climatic trends at Nuuk & Zackenberg') +
  facet_grid(Variable~., scales="free") +
  theme_linedraw() 

#  SEASONAL TRENDS
ggplot(clim_season_year, aes(Year, Value, group=Site, color=Season)) +
  scale_color_brewer(palette="Set1")+
  geom_point() +
  geom_smooth(aes(group=Season, color=Season, fill=Season), method='loess', se=TRUE) +
  labs(y='Value') +
  ggtitle('Seasonal climatic trends at Nuuk & Zackenberg') +
  facet_grid(Variable+Site~., scales="free") +
  theme_linedraw() 
### END CLIMATIC PLOTS  ## -----------------------------------------------





# SNOW MELT DAY OF THE YEAR ZACKENBERG
# Plot SnowMelt DOY    ## ------------------------------------------------
# YEAR TRENDS
ggplot(left_join(flow, snow, by=c("Year", "Plot", "Site")), 
       aes(x=Year, y=snowmelt_DOY, group=Species, color=Species)) +
  geom_point(size=2) +
  geom_smooth(aes(group=Species, color=Species), 
              method='lm', se=F) +
  ggtitle('Temporal trend of snowmelt day') +
  theme(axis.text=element_text(size=16, face='bold'),
        axis.title=element_text(size=16, face="bold"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  facet_grid(Site~., scales="free") 
### END SNOW MELT DOY   ## -----------------------------------------------






# COMMUNITY FLOWER DENSITY  ##--------------------------------------------
 ggplot(flow_com, aes(Year, ComFlow_m2, group=Site, color=Site)) +
  scale_color_brewer(palette="Set1")+
  geom_point() +
  geom_smooth(aes(group=Site, color=Site, fill=Site), 
                  method='lm', se=TRUE) +
  labs(y='Flower density / m2') +
  ggtitle('Community Flower density') +
  facet_grid(Site~., scales="free") +
  theme_linedraw()  +
  theme(axis.text=element_text(size=16, face='bold'),
        axis.title=element_text(size=16, face="bold"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))

# summary(lm(ComFlow_m2 ~ Year*Site, data=flow_com))
# summary(lm(ComFlow_m2 ~ Year, data=flow_com %>% filter(Site=="Nuuk")))
# summary(lm(ComFlow_m2 ~ Year, data=flow_com %>% filter(Site=="Zackenberg")))
### END COMMUNITY FLOWER DENSITY   ## ------------------------------------





