#######################################################################################
#
#                           Greenland Flowering project
#                             03 - Analysis
#
#######################################################################################
# Antoine Becker-Scarpitta
# July 2020

#  clean R work-space
rm(list=ls())

# Load 02 - Creation database (wihi load scripts 00 and 01)
source("Scripts/02_Creation_DB.r")


# DID
# delete K, W, plots => DONE
# add SITE in all dataset => DONE
# correct Zack Sil , Si names => DONE
# as.factor(year) => DONE

# TO DO 
# NA in SECTION
# NUUK has a different structure NEED TO FIGURED OUT how to integrated it
# different format of data in SECTION
# cas5, cas6, dry7, dry8 half no data, should delete them


# DATA STRUCTURE FOR ANALYSIS
# select only TotalFlowering line for the total flower per year
flow_sub <- droplevels(flow[flow$Flower_var=="TotalFlowering", ])

#  delete NA in the subset
flow_sub <- flow_sub[complete.cases(flow_sub),]

# calculate the total flower per plot per year (sum of all sections)
flow_tot_plot <- ddply(flow_sub, .(Site, Year, Species, Plot), 
                  summarise, SumFlower=sum(Value))

# calcula sumFlower per species only
flow_tot_sp <- ddply(flow_tot_plot, .(Site, Year, Species), 
                  summarise, SumFlower=sum(SumFlower))
####---


# basic explorations
qplot(data= flow_tot_sp, Year, SumFlower, geom='line') +
  facet_wrap(~Species)








