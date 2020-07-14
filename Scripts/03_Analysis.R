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



# select only TotalFlowering line for the total flower per year
flow_sub <- droplevels(flow[flow$Flower_var=="TotalFlowering", ])

#  delete NA in the subset
flow_sub <- flow_sub[complete.cases(flow_sub),]

# chekc number of obs per plot per year
table(flow_sub$Plot, flow_sub$Year)


# WORK ON IT _ DO NOT WORK
flow_sub[c("Year", "Site", "Plot", "Section", "Species")] <- sapply(flow[c("Year","Site","Plot","Section","Species")], as.factor)

flow_sub$Year <- as.factor(flow_sub$Year)
flow_sub$Site <- as.factor(flow_sub$Site)
flow_sub$Plot <- as.factor(flow_sub$Plot)
flow_sub$Section <- as.factor(flow_sub$Section)
flow_sub$Species <- as.factor(flow_sub$Species)

# calculate the total flower per plot per year (sum of all sections)
flow_tot <- ddply(flow_sub, .(Site, Year, Species, Plot), 
                  summarise, SumFlower=sum(Value))



# basic explorations
qplot(data= flow, Year, Value, geom='line') +
  facet_wrap(~Species)


