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
source("Scripts/02_Climatic_covariates.R")



table(flow[flow$Site=="Nuuk", "Year"], flow[flow$Site=="Nuuk", "Species"])
table(flow[flow$Site=="Zackenberg", "Year"], flow[flow$Site=="Zackenberg", "Species"])




#  TEMPORAL TRENDS AT PLOT LEVEL 
# 1 - Flower density per m2  ## --------------------------------------------
flow_den <- ggplot(flow, aes(Year, Flow_m2, group=Plot, color=Plot)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE) +
  labs(y='Flower density / m2') +
  ggtitle('Flower density') +
  facet_grid(Site+Species~., scales="free_y") +
  theme_linedraw() +
  theme(legend.position = "none") 

# 2 - Total Flower per plot  ## --------------------------------------------
flow_tot <- ggplot(flow, aes(Year, TotalFlower, group=Plot, color=Plot)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE) +
  labs(y='Total flower number') +
  ggtitle('Total flower nubmer') +
  facet_grid(Site+Species~., scales="free_y")+
  theme_linedraw() +
  theme(legend.position = "none") 

#  graph
gridExtra::grid.arrange(flow_den, flow_tot, ncol=2, 
                        top = "Plot level temporal trends in flowering")
### END PLOT LEVEL ## ------------------------------------------------------





#  TEMPORAL TRENDS AT SPECIE LEVEL 
# 3 - Flower density per m2  ## --------------------------------------------
flow_den2 <- ggplot(flow, aes(Year, Flow_m2, group=Species, color=Species)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE) +
  labs(y='Flower density/m2') +
  ggtitle('Flower density') +
  facet_grid(Site+Species ~., scales="free_y") +
  theme_linedraw()+
  theme(legend.position = "none") +
  scale_color_brewer(palette="Dark2")

# 4 - Total Flower per plot  ## --------------------------------------------
flow_tot2 <- ggplot(flow, aes(Year, TotalFlower, group=Species, color=Species)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE) +
  labs(y='Total flower number') +
  ggtitle('Total flower number') +
  facet_grid(Site+Species~., scales="free_y")+
  theme_linedraw() +
  theme(legend.position = "none") +
  scale_color_brewer(palette="Dark2")

#  graph
gridExtra::grid.arrange(flow_den2, flow_tot2, ncol=2, 
           top = "Temporal flowering trends at species level in Zackenberg")
### END SP LEVEL ## -----------------------------------------------------





# DISTRIBUTION OF FLOWERING VARIABLES 
# DENSITY
distdens <- ggplot(flow, aes(x=Flow_m2, fill=Species)) + 
  geom_density(alpha=.3) +
  labs(y='Density', x='Flowering density/m2 ') +
  ggtitle('Distribution of Flowering density') +
  facet_wrap(.~Site+Species, scales="free") +
  theme_linedraw() +
  theme(legend.position = "none")

# TOTAL
disttot <- ggplot(flow, aes(x=TotalFlower, fill=Species)) + 
  geom_density(alpha=.3) +
  labs(y='Density', x='Total Flowers') +
  ggtitle('Distribution of Total Flowering') +
  facet_wrap(Site+Species~ ., scales="free") +
  theme_linedraw() +
  theme(legend.position = "none")

#  graph
gridExtra::grid.arrange(distdens, disttot, ncol=2, 
                        top = "Distribution of flowering variable per species")
### END DISTRI PLOTS  ## -------------------------------------------------




# CLIMATIC TRENDS
# Plot Climatic trends    ## ---------------------------------------------
# ALL MONTHS
ggplot(clim_month, aes(Year, Value, group=Site, color=Site)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE) +
  labs(y='Value') +
  ggtitle('Climatic trends all months Nuuk & Zackenberg') +
  facet_grid(Variable~., scales="free_y") +
  theme_linedraw() 



# GROWING SEASON June to August
clim_month$Season <- "winter"
clim_month[clim_month$Month=="06", "Season"] <- "summer"
clim_month[clim_month$Month=="07", "Season"] <- "summer"
clim_month[clim_month$Month=="08", "Season"] <- "summer"
clim_month[clim_month$Month=="09", "Season"] <- "summer"

ggplot(clim_month[clim_month$Season=="summer", ], 
       aes(Year, Value, group=Site, color=Site)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE) +
  labs(y='Value') +
  ggtitle('Climatic trends in the growing season (June-Aug) Nuuk & Zackenberg') +
  facet_grid(Variable~., scales="free_y") +
  theme_linedraw() 
### END CLIMATIC PLOTS  ## -----------------------------------------------





