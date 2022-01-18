#######################################################################################
#
#                           Greenland Flowering project
#                            04 - Analysis covariates
#
#######################################################################################
# Antoine Becker-Scarpitta
# November 2020

#  clean R work-space
# rm(list=ls())

# Load 02 - Creation database (+ scripts 00 and 01)
source("Scripts/00_Load_libraries.r")
source("Scripts/02_Creation_DB.r")
source("Scripts/C1_Load_climatic_covariates.R")
source("Scripts/C2_Build_all_snow_covariates.R")


### CLIMATIC TEMPORAL TRENDS  #-------------------------------------------------------
ggplot(clim_year, aes(Year, Value, group=Site, color=Site)) +
  scale_color_brewer(palette="Set1")+
  geom_point() +
  geom_smooth(aes(group=Site, color=Site, fill=Site), method='lm', se=TRUE) +
  labs(y='Value') +
  ggtitle('Temporal climatic trends at Nuuk & Zackenberg') +
  facet_grid(Variable~., scales="free") +
  theme_linedraw() +
  theme(text = element_text(size = 20), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),  axis.line = element_line(colour = "black")) 


# ANNUAL TRENDS
#Zack
fits_year_z <- droplevels(clim_year[clim_year$Site=="Zackenberg", ]) %>%
  group_by(Variable) %>%
  do(broom::tidy(lm(Value ~ Year, data=.))) %>%
  filter(term!= "(Intercept)")
#Nuuk
fits_year_n <- droplevels(clim_year[clim_year$Site=="Nuuk", ]) %>%
  group_by(Variable) %>%
  do(broom::tidy(lm(Value ~ Year, data=.))) %>%
  filter(term!= "(Intercept)")



### CLIMATIC SEASONAL TRENDS  #------------------------------------------------------
ggplot(clim_season_year, aes(Year, Value, group=Site, color=Season)) +
  scale_color_brewer(palette="Set1")+
  geom_point() +
  geom_smooth(aes(group=Season, color=Season), method='lm', se=FALSE) +
  labs(y='Value') +
  ggtitle('Seasonal climatic trends at Nuuk & Zackenberg') +
  facet_grid(Variable+Site~., scales="free") +
  theme_linedraw() 

# SEASONAL TRENDS
#Zack
fits_season_z <- droplevels(clim_season_year[clim_season_year$Site=="Zackenberg", ]) %>%
  group_by(Variable) %>%
  do(broom::tidy(lm(Value ~ Year*Season, data=.))) %>%
  filter(term!= "(Intercept)")
#Nuuk
fits_season_n <- droplevels(clim_season_year[clim_season_year$Site=="Nuuk", ]) %>%
  group_by(Variable) %>%
  do(broom::tidy(lm(Value ~ Year*Season, data=.))) %>%
  filter(term!= "(Intercept)")
# END-------------------------------------------------------------------------------




### SNOW MELT DOY  #----------------------------------------------------------------
# Merge flow data with snow melt DOY
flow_snow <- left_join(flow, snow, by=c("Site", "Year", "Plot")) 

## SnowMelt DOY graphs
#NUUK
SM_Nuuk <- ggplot(flow_snow %>% filter(Site=="Nuuk"),
                  aes(x=Year, y=snowmelt_DOY, 
                              group=Species, 
                              color=Species)) +
  geom_point(size=2) +
  geom_smooth(method='lm', se=F) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16,face="bold"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_x_continuous(limits = c(2009, 2019), 
                     breaks=c(2009,2011,
                              2013,2015,
                              2017,2019)) +
  scale_y_continuous(limits = c(117, 220))

# ZACKENBERG
SM_Zack <- ggplot(flow_snow %>% filter(Site=="Zackenberg"),
                  aes(x=Year, y=snowmelt_DOY, 
                      group=Species, 
                      color=Species)) +
  geom_point(size=2) +
  geom_smooth(method='lm', se=F) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16,face="bold"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))  +
  scale_y_continuous(limits = c(117, 220))

gridExtra::grid.arrange(SM_Nuuk, SM_Zack, ncol=2, 
                        top = "Plot level temporal trends in flowering")
# END-------------------------------------------------------------------------------

