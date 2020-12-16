###############################################################################
#
#                           Greenland Flowering project
#                         03 - Basic Explorations Graph
#
###############################################################################
# Antoine Becker-Scarpitta
# September 2020

#  clean R work-space
rm(list=ls())

# Load 02 - Creation database (load scripts 00 and 01)
source("Scripts/00_Load_libraries.r")
source("Scripts/01_Import_DB.r")
source("Scripts/02_Creation_DB.R")
source("Scripts/C2_Climatic_covariates.R")


# Total number of plot per year Nuuk and Zackenberg
# write.csv(table(flow[flow$Site=="Nuuk", "Year"], flow[flow$Site=="Nuuk", "Species"]), "results/Plot_num_year_Nuuk.csv")
# write.csv(table(flow[flow$Site=="Zackenberg", "Year"], flow[flow$Site=="Zackenberg", "Species"]), "results/Plot_num_year_Zack.csv")



#  TEMPORAL TRENDS AT PLOT LEVEL WITH LOG ON DENSITY ONLY
# 1 - Flower density per m2  ## --------------------------------------------
flow_den <- ggplot(flow, aes(Year, Flow_m2, group=Plot, color=Plot)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE) +
  labs(y='Flower density / m2') +
  ggtitle('A - Flower density') +
  facet_grid(Site+Species~., scales="free_y") +
  theme_linedraw() +
  theme(legend.position = "none") 

# 3 - Log Flower density !=0  ## --------------------------------------------
flow_log_nn <- ggplot(flow, aes(Year, TotalFlower, group=Plot, color=Plot)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE) +
  labs(y='log(Flower density / m2)') +
  ggtitle('B - Log(Flower density) !=0') +
  facet_grid(Site+Species~., scales="free_y")+
  theme_linedraw() +
  theme(legend.position = "none") 
#  graph
gridExtra::grid.arrange(flow_den, flow_log_nn, ncol=2, 
                        top = "Plot level temporal trends in flowering")
### END PLOT LEVEL ## ------------------------------------------------------




#  TEMPORAL TRENDS AT SPECIE LEVEL DENSITY ONLY
# 4 - Flower density per m2  ## --------------------------------------------
flow_den2 <- ggplot(flow, aes(Year, Flow_m2, group=Species, color=Species)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE) +
  labs(y='Flower density/m2') +
  ggtitle('A - Flower density') +
  facet_grid(Site+Species ~., scales="free_y") +
  theme_linedraw()+
  theme(legend.position = "none") 

# 6 - Total Flower per plot  ## --------------------------------------------
flow_den2_log_nn <- ggplot(flow, aes(Year, log(trans_Flow_m2), group=Species, color=Species)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE) +
  labs(y='log(Flower density/m2)') +
  ggtitle('B - Log(Flower density) !=0') +
  facet_grid(Site+Species~., scales="free_y")+
  theme_linedraw() +
  theme(legend.position = "none")
#  graph
gridExtra::grid.arrange(flow_den2, flow_den2_log_nn, ncol=2, 
                        top = "Temporal flowering trends at species level in Zackenberg")
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
  geom_smooth(aes(group=Season, color=Season), method='lm', se=FALSE) +
  labs(y='Value') +
  ggtitle('Seasonal climatic trends at Nuuk & Zackenberg') +
  facet_grid(Variable+Site~., scales="free") +
  theme_linedraw() 
### END CLIMATIC PLOTS  ## -----------------------------------------------



