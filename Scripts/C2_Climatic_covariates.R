#######################################################################################
#
#                           Greenland Flowering project
#                             01 - Import COVARIATES
#
#######################################################################################
# Antoine Becker-Scarpitta
# November 2020

#  clean R work-space
# rm(list=ls())

# load library
# R, Rstudio need to be updated
# Library need to be installed (see 00_Load_libraries script)
source("Scripts/00_Load_libraries.r")





#  1 LOAD CLIMATE COVARIATES
#  ZACKENBERG
# Temperature
Ztemp <- read.csv("data/datasets/View_ClimateBasis_Zackenberg_Data_Temperature_Air_temperature__200cm__60min_average__DegreesC.csv", 
                  stringsAsFactors=FALSE, header=TRUE,  sep="\t",
                  strip.white = T,na.strings = c("","NA"))
Ztemp$Variable <- "Temperature_C"
Ztemp$Site <- "Zackenberg"
colnames(Ztemp) <- c('Date', "Time", "Value", "Variable", "Site")

# Precipitation
Zprec <- read.csv("data/datasets/View_ClimateBasis_Zackenberg_Data_Precipitation_Precipitation_accumulated_mm.csv", 
                  stringsAsFactors=FALSE, header=TRUE,  sep="\t",
                  strip.white = T,na.strings = c("","NA"))
Zprec$Variable <- "Precipitation_mm"
Zprec$Site <- "Zackenberg"
colnames(Zprec) <- c('Date', "Time", "Value", "Variable", "Site")

# Humidity
Zhum <- read.csv("data/datasets/View_ClimateBasis_Zackenberg_Data_Humidity_Relative_humidity__200cm__60min_average__Percent.csv", 
                 stringsAsFactors=FALSE, header=TRUE,  sep="\t",
                 strip.white = T,na.strings = c("","NA"))
Zhum$Variable <- "Humidity_%"
Zhum$Site <- "Zackenberg"
colnames(Zhum) <- c('Date', "Time", "Value", "Variable", "Site")




#  NUUK
# Temperature
Ntemp <- read.csv("data/datasets/View_ClimateBasis_Nuuk_Data_Temperature_Air_temperature__200_cm__30min_average__DegreesC.csv", 
                  stringsAsFactors=FALSE, header=TRUE,  sep="\t",
                  strip.white = T,na.strings = c("","NA"))
Ntemp$Variable <- "Temperature_C"
Ntemp$Site <- "Nuuk"
colnames(Ntemp) <- c('Date', "Time", "Value", "Variable", 'Site')

# Precicpiation
Nprec <- read.csv("data/datasets/View_ClimateBasis_Nuuk_Data_Precipitation_Precipitation_accumulated_mm.csv", 
                  stringsAsFactors=FALSE, header=TRUE,  sep="\t",
                  strip.white = T,na.strings = c("","NA"))
Nprec$Variable <- "Precipitation_mm"
Nprec$Site <- "Nuuk"
colnames(Nprec) <- c('Date', "Time", "Value", "Variable", "Site")

# Humidity
Nhum <- read.csv("data/datasets/View_ClimateBasis_Nuuk_Data_Humidity_Relative_humidity__200_cm__30min_average__Percent.csv", 
                  stringsAsFactors=FALSE, header=TRUE,  sep="\t",
                  strip.white = T,na.strings = c("","NA"))
Nhum$Variable <- "Humidity_%"
Nhum$Site <- "Nuuk"
colnames(Nhum) <- c('Date', "Time", "Value", "Variable", "Site")
#-------------------------------------------------------------------------------------




# COMPILE COVARIATE DATABASE
# combine All dataset together
clim <- rbind(Ztemp, Zprec, Zhum, Ntemp, Nprec, Nhum)

# Remove -9999, NA
clim[clim$Value==-9999, 'Value'] <- NA
clim <- clim[complete.cases(clim), ]

# Devided Date into Year  Month Day
clim <- cbind(as.data.frame(str_split(clim$Date, "-", simplify=TRUE)), clim)
colnames(clim) <- c("Year", "Month", "Day", 'Date', "Time", "Value", "Variable", "Site")


# create a new col==SEASONAL variable
clim$Season <- "winter"
clim[clim$Month=="06" |
       clim$Month=="07" |
       clim$Month=="08", "Season"] <- "summer"
clim[clim$Month=="09" |
       clim$Month=="10", "Season"] <- "autumn"

# calculate the average variable value for SUMMER, AUTUNM, WINTER per YEAR
clim_season_year <- as.data.frame(clim %>% 
                             dplyr::group_by(Site, Year, Variable, Season) %>% 
                             dplyr::summarise(Value=round(mean(Value), 3))) %>%
                             dplyr::mutate(Site=as.factor(Site),
                                   Year=as.numeric(Year), 
                                   Variable=as.factor(Variable))%>%
                             ungroup()


# calculate the average Value per YEAR (all months)
clim_year <- as.data.frame(clim %>% 
                             dplyr::group_by(Site, Year, Variable) %>% 
                             dplyr::summarise(Value=round(mean(Value), 2))) %>%
                             dplyr::mutate(Site=as.factor(Site),
                                     Year=as.numeric(Year), 
                                     Variable=as.factor(Variable))%>%
                             ungroup()

#END----------------------------------------------------------------------------------


# remove temporary table
remove("Nhum", "Nprec", "Ntemp", "Zhum", "Zprec", "Ztemp")



