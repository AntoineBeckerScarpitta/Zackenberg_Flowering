#######################################################################################
#
#                           Greenland Flowering project
#                         03 - Basic Explorations Graph
#
#######################################################################################
# Antoine Becker-Scarpitta
# September 2020

#  clean R work-space
rm(list=ls())

# Load 02 - Creation database (load scripts 00 and 01)
source("Scripts/02_Creation_DB.r")




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
  theme(legend.position = "none")

# 4 - Total Flower per plot  ## --------------------------------------------
flow_tot2 <- ggplot(flow, aes(Year, TotalFlower, group=Species, color=Species)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE) +
  labs(y='Total flower number') +
  ggtitle('Total flower number') +
  facet_grid(Site+Species~., scales="free_y")+
  theme_linedraw() +
  theme(legend.position = "none")

#  graph
gridExtra::grid.arrange(flow_den2, flow_tot2, ncol=2, 
                        top = "Temporal flowering trends at species level in Zackenberg")
### END SP LEVEL ## ------------------------------------------------------------------




# DISTRIBUTION OF FLOWERING VARIABLES 
# DENSITY
distdens <- ggplot(flow, aes(x=Flow_m2, fill=Species)) + 
  geom_density(alpha=.3) +
  labs(y='Density', x='Flowering density/m2 ') +
  ggtitle('Distribution of Flowering density') +
  facet_wrap(.~Species, scales="free") +
  theme_linedraw() +
  theme(legend.position = "none")

# TOTAL
disttot <- ggplot(flow, aes(x=TotalFlower, fill=Species)) + 
  geom_density(alpha=.3) +
  labs(y='Density', x='Total Flowers') +
  ggtitle('Distribution of Total Flowering') +
  facet_wrap(Species~ ., scales="free") +
  theme_linedraw() +
  theme(legend.position = "none")

#  graph
gridExtra::grid.arrange(distdens, disttot, ncol=2, 
                        top = "Distribution of flowering variable per species")
### END DISTRI PLOTS  ## ------------------------------------------------------------------

