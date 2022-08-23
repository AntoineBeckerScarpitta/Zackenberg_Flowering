# 1.1 - LOAD MODELS ----------------------------------------------------------
#  clean R work-space
rm(list=ls())

# Run mods
source("Scripts/04_Analysis_Zack.r")
source("Scripts/04_Analysis_Nuuk.r")

# clean R object 
remove("clim_season_year", "temp_clim_z", "temp_clim_n", "flow_snow_z",
       "flow_snow_n", "snow")
##----------------------------------------------------------------------------

#ZAck
mod_basic_z <- lmer(log_flow ~ 0 + 
                      Species * Year +
                      (1|Plot),
                    data= flow %>%
                      filter(Site=="Zackenberg") %>%
                      mutate(., log_flow=log1p(Flow_m2)),
                    REML=T, 
                    na.action=na.omit)

# mean predict
#extract the data used in mod_basic_n
data_mod_z <- as.data.frame(mod_basic_z@frame)
#extract the predict from mod_basic_n
data_mod_z$pred <- predict(mod_basic_z, re.form=NA)  ## population level
#compute mean predict
data_mod_mean_z <- data_mod_z %>% group_by(Species, Year) %>%
  summarise(AvPred=mean(pred))


# graph predictions
zack_pred <- ggplot(flow %>%
                      filter(Site=="Zackenberg") %>%
                      mutate(., log_flow=log1p(Flow_m2)), 
                    aes(Year, log_flow, group=Species, color=Species)) +
  geom_point(size=1) +
  # geom_smooth(method='lm', se=TRUE, aes(fill=Plot), alpha = 0.1) + #plot trends
  geom_line(data=data_mod_z, aes(y=pred, x=Year, group=Species)) + # sp prediction
  labs(y='log(flower density)') +
  ggtitle('High Arctic - PRED') +
  facet_grid(Species~., scales="free_y") +
  theme_linedraw() +
  scale_x_continuous(limits = c(1997, 2019),
                     breaks = c(1997, 2002, 2007, 2012, 2017)) +
  theme(legend.position = "none", 
        axis.text=element_text(size=10),
        axis.title=element_text(size=10, face="bold"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", color="black"))

  
# graph ggplot
zack_ggplot <- ggplot(flow %>%
                        filter(Site=="Zackenberg") %>%
                        mutate(., log_flow=log1p(Flow_m2)), 
                      aes(Year, log_flow, group=Species, color=Species)) +
  geom_point(size=1) +
  geom_smooth(method='lm', se=TRUE, aes(fill=Species), alpha = 0.3) + #plot trends
  # geom_smooth(method='lm', se=FALSE, colour="black", 
  #             aes(y=pred)) + # sp prediction  , group=Species
  labs(y='log(flower density)') +
  ggtitle('High Arctic - GGPLOT') +
  facet_grid(Species~., scales="free_y") +
  theme_linedraw() +
  scale_x_continuous(limits = c(1997, 2019),
                     breaks = c(1997, 2002, 2007, 2012, 2017)) +
  theme(legend.position = "none", 
        axis.text=element_text(size=10),
        axis.title=element_text(size=10, face="bold"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", color="black"))

#combine graphs
gridExtra::grid.arrange(zack_pred, zack_ggplot, ncol=2)





### NUUK
#Nuuk
mod_basic_n <- lmer(log_flow ~ 0 + 
                      Species * Year +
                      (1|Plot),
                    data= flow %>%
                      filter(Site=="Nuuk") %>%
                      mutate(., log_flow=log1p(Flow_m2)),
                    REML=T, 
                    na.action=na.omit)

#extract predict
# mean predict
#extract the data used in mod_basic_n
data_mod_n <- as.data.frame(mod_basic_n@frame)
#extract the predict from mod_basic_n
data_mod_n$pred <- predict(mod_basic_n, re.form=NA)  ## population level
#compute mean predict
data_mod_mean_n <- data_mod_n %>% group_by(Species, Year) %>%
  summarise(AvPred=mean(pred))



#graph ggplot
nuuk_ggplot <- ggplot(flow %>%
                      filter(Site=="Nuuk") %>%
                      mutate(., log_flow=log1p(Flow_m2)), 
                      aes(Year, log_flow, group=Species, color=Species)) +
  geom_point(size=1) +
  geom_smooth(method='lm', se=TRUE, aes(fill=Species), alpha = 0.3) + #plot trends
  labs(y='log(flower density)') +
  ggtitle('Nuuk - ggplot') +
  facet_grid(Species~., scales="free_y") +
  theme_linedraw() +
  scale_x_continuous(limits = c(2009, 2019),
                     breaks = c(2009, 2011, 2013, 2015, 2017, 2019)) +
  theme(legend.position = "none", 
        axis.text=element_text(size=10),
        axis.title=element_text(size=10, face="bold"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", color="black"))



# graph predictions
nuuk_pred <- ggplot(flow %>%
                      filter(Site=="Nuuk") %>%
                      mutate(., log_flow=log1p(Flow_m2)), 
                    aes(Year, log_flow, group=Species, color=Species)) +
  geom_point(size=1) +
  geom_line(data=data_mod_n, aes(y=pred, x=Year, group=Species)) + # sp prediction
  labs(y='log(flower density)') +
  ggtitle('Nuuk - Predictions') +
  facet_grid(Species~., scales="free_y") +
  theme_linedraw() +
  scale_x_continuous(limits = c(2009, 2019),
                     breaks = c(2009, 2011, 2013, 2015, 2017, 2019)) +
  theme(legend.position = "none", 
        axis.text=element_text(size=10),
        axis.title=element_text(size=10, face="bold"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"), 
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", color="black"))
  
  #combine graphs
  gridExtra::grid.arrange(nuuk_pred, nuuk_ggplot, ncol=2)
  
  
  
  #all 
  #combine graphs
  gridExtra::grid.arrange(zack_pred, zack_ggplot,nuuk_pred, nuuk_ggplot, ncol=4)
  