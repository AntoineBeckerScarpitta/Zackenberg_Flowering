###############################################################################
#
#                           Greenland Flowering project
#                            04 - Analysis covariates
#
###############################################################################
# Antoine Becker-Scarpitta
# May 2022

#  clean R work-space
# rm(list=ls())

# Load 02 - Creation database (+ scripts 00 and 01)
# source("Scripts/00_Load_libraries.r")
source("Scripts/02_Creation_DB.r")
source("Scripts/C1_Load_climatic_covariates.R")
source("Scripts/C2_Build_all_snow_covariates.R")




## MAT ET MET  - CLIM
##TEMPERATURE
# calculate the average year temperature
temp_year_site <- clim %>%
  dplyr::mutate(Year=as.numeric(Year), 
                Month=as.numeric(Month)) %>%
  dplyr::filter(Variable=="Temperature_C", 
                Year>2007, 
                Year<=2020) %>%
  dplyr::group_by(Site) %>%
  summarise(mean(Value)) %>%
  ungroup()

# Nuuk & Zack 2007-2019
# Nuuk       Temperature_C          9.52
# Zackenberg Temperature_C          5.89

# calculate the average monthly temperature
temp_month <- clim %>% 
  dplyr::mutate(Year=as.numeric(Year), 
                Month=as.numeric(Month)) %>%
  dplyr::filter(Year>2007,
                Year<=2020,
                Variable=="Temperature_C") %>%
  dplyr::group_by(Site, Month) %>% 
  dplyr::summarise(Mean=round(mean(Value), 3), 
                   Min=min(Value), 
                   Max=max(Value))

# PRECIPITATION
prec_year_site <- clim %>%
  dplyr::mutate(Year=as.numeric(Year)) %>%
  dplyr::filter(Variable=="Precipitation_mm", 
                Year>2007,
                Year<=2020) %>%
  dplyr::group_by(Site, Year) %>%
  dplyr::summarise(Sum_P_Y=sum(Value)) %>%
  dplyr::summarise(Mean_P=mean(Sum_P_Y)) %>%
  ungroup()
# END-------------------------------------------------------------------------





### CLIMATIC TEMPORAL TRENDS  #-----------------------------------------------
ggplot(clim_year %>% dplyr::filter(Variable!="Humidity_%"), 
       aes(Year, Value, group=Site, color=Site)) +
  scale_color_brewer(palette="Set1")+
  geom_point() +
  geom_smooth(aes(group=Site, color=Site, fill=Site), method='lm', se=TRUE) +
  labs(y='Value') +
  ggtitle(' ') +
  facet_grid(Variable~., scales="free") +
  geom_hline(yintercept=0) +
  theme_linedraw() +
  theme(text = element_text(size = 20), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),  
        axis.line = element_line(colour = "black")) 


# ANNUAL TRENDS
#Zack
fits_year_z <- droplevels(clim_year[clim_year$Site=="Zackenberg", ]) %>%
  dplyr::filter(Variable!="Humidity_%") %>% 
  group_by(Variable) %>%
  do(broom::tidy(lm(Value ~ Year, data=.))) %>%
  filter(term!= "(Intercept)")
#Nuuk
fits_year_n <- droplevels(clim_year[clim_year$Site=="Nuuk", ]) %>%
  dplyr::filter(Variable!="Humidity_%") %>% 
  group_by(Variable) %>%
  do(broom::tidy(lm(Value ~ Year, data=.))) %>%
  filter(term!= "(Intercept)")



### CLIMATIC SEASONAL TRENDS  #------------------------------------------------
ggplot(clim_season_year %>% dplyr::filter(Variable!="Humidity_%", 
                                          Season!="winter"), 
       aes(Year, Value, group=Site, color=Season)) +
  scale_color_brewer(palette="Set1")+
  geom_point() +
  geom_smooth(aes(group=Season, color=Season, fill=Season), 
              method='lm', se=TRUE) +
  labs(y='Value') +
  ggtitle('  ') +
  facet_grid(Variable~Site, scales="free") +
  geom_hline(yintercept=0) +
  theme_linedraw() +
  theme(text = element_text(size = 15), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 


# SEASONAL TRENDS
#Zack
# summary(lm(Value ~ 0 + Year:Season, data=
#                       droplevels(
#                         clim_season_year[clim_season_year$Site=="High_Arctic",]) %>%
#   dplyr::filter(Variable=="Precipitation_mm", 
#                 Season!="winter")))

#Zack
fits_season_z <- droplevels(clim_season_year[clim_season_year$Site=="High_Arctic", ]) %>%
  dplyr::filter(Variable!="Humidity_%", 
                Season!="winter") %>% 
  group_by(Variable) %>%
  do(broom::tidy(lm(Value ~ Year:Season, data=.))) 



#Nuuk
fits_season_n <- droplevels(clim_season_year[clim_season_year$Site=="Low_Arctic", ]) %>%
  dplyr::filter(Variable!="Humidity_%", 
                Season!="winter") %>% 
  group_by(Variable) %>%
  do(broom::tidy(lm(Value ~ Year:Season, data=.))) #%>%
  #filter(term!= "(Intercept)")
# END-------------------------------------------------------------------------




### SNOW MELT DOY  #----------------------------------------------------------
# Merge flow data with snow melt DOY
snow$SNOW <-"T1"
flow_snow <- left_join(flow, snow, by=c("Site", "Year", "Plot")) %>%
  dplyr::filter(SNOW=='T1') %>%
  dplyr::mutate(Site=dplyr::recode(Site, 
                                   "Nuuk"="Low_Arctic",
                                   "Zackenberg"="High_Arctic"))

# flow_snow <- left_join(flow, snow, by=c("Site", "Year", "Plot"))


## SnowMelt DOY graphs
#NUUK
SM_Nuuk <- ggplot(flow_snow %>% filter(Site=="Low_Arctic"),
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
  scale_y_continuous(limits = c(117, 200)) +
  ggtitle('Low Arctic') 

# ZACKENBERG
SM_Zack <- ggplot(flow_snow %>% filter(Site=="High_Arctic"),
                  aes(x=Year, y=snowmelt_DOY, 
                      group=Species, 
                      color=Species)) +
  geom_point(size=2) +
  geom_smooth(method='lm', se=F) +
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=16,face="bold"),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"))  +
  scale_y_continuous(limits = c(117, 200)) +
  ggtitle('High Arctic') 

gridExtra::grid.arrange(SM_Nuuk, SM_Zack, ncol=2)


# SNOWMELT TRENDS
#Zack
fits_SM_z <- droplevels(flow_snow[flow_snow$Site=="High_Arctic", ]) %>%
  group_by(Species) %>%
  do(broom::tidy(lm(snowmelt_DOY ~ Year, data=.))) %>%
  filter(term!= "(Intercept)")

# summary(lm(snowmelt_DOY ~ Year*Species, data=droplevels(flow_snow[flow_snow$Site=="High_Arctic", ])))

#Nuuk
fits_SM_n <- droplevels(flow_snow[flow_snow$Site=="Low_Arctic", ]) %>%
  group_by(Species) %>%
  do(broom::tidy(lm(snowmelt_DOY ~ Year, data=.))) %>%
  filter(term!= "(Intercept)")
# END-------------------------------------------------------------------------

