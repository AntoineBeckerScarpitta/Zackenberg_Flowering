#######################################################################################
#
#                           Greenland Flowering project
#                            04 - Analysis covariates
#
#######################################################################################
# Antoine Becker-Scarpitta
# November 2020

#  clean R work-space
rm(list=ls())

# Load 02 - Creation database (+ scripts 00 and 01)
source("Scripts/00_Load_libraries.r")
source("Scripts/C2_Climatic_covariates.R")



### TEMPORAL TRENDS  #----------------------------------------------------------------
ggplot(clim_year, aes(Year, Value, group=Site, color=Site)) +
  scale_color_brewer(palette="Set1")+
  geom_point() +
  geom_smooth(aes(group=Site, color=Site, fill=Site), method='lm', se=TRUE) +
  labs(y='Value') +
  ggtitle('Temporal climatic trends at Nuuk & Zackenberg') +
  facet_grid(Variable~., scales="free") +
  theme_linedraw() 

# ZACKENBERG TEMPORAL TRENDS
fits_year <- droplevels(clim_year[clim_year$Site=="Zackenberg", ]) %>%
  group_by(Variable) %>%
  do(broom::tidy(lm(Value ~ Year, data=.))) %>%
  filter(term!= "(Intercept)")


### SEASONAL TRENDS  #----------------------------------------------------------------
ggplot(clim_season_year, aes(Year, Value, group=Site, color=Season)) +
  scale_color_brewer(palette="Set1")+
  geom_point() +
  geom_smooth(aes(group=Season, color=Season), method='lm', se=FALSE) +
  labs(y='Value') +
  ggtitle('Seasonal climatic trends at Nuuk & Zackenberg') +
  facet_grid(Variable+Site~., scales="free") +
  theme_linedraw() 

# ZACKENBERG SEASONAL TRENDS
fits_season <- droplevels(clim_season_year[clim_season_year$Site=="Zackenberg", ]) %>%
  group_by(Variable) %>%
  do(broom::tidy(lm(Value ~ Year, data=.))) %>%
  filter(term!= "(Intercept)")
# END-------------------------------------------------------------------------------

