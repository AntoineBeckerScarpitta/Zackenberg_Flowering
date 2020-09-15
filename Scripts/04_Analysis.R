#######################################################################################
#
#                           Greenland Flowering project
#                                04 - Analysis
#
#######################################################################################
# Antoine Becker-Scarpitta
# July 2020

#  clean R work-space
rm(list=ls())

# Load 02 - Creation database (wihi load scripts 00 and 01)
source("Scripts/02_Creation_DB.r")






ggplot(flow_tot_plot[flow_tot_plot$Species=="CAS",],
       aes(Year, Flow_m2, group=Plot, color=Plot)) +
  geom_point() +
  geom_smooth(method='lm', se=TRUE) +
  labs(y='Flow density m2') +
  ggtitle('CAS')