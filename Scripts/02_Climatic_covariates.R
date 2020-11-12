#######################################################################################
#
#                           Greenland Flowering project
#                             01 - Import COVARIATES
#
#######################################################################################
# Antoine Becker-Scarpitta
# November 2020

#  clean R work-space
rm(list=ls())

# load library
# R, Rstudio need to be updated
# Library need to be installed (see 00_Load_libraries script)
source("Scripts/00_Load_libraries.r")



#  ZACKENBERG
# CLIMATE COVARIATES
Ztemp <- read.csv("data/datasets/View_ClimateBasis_Zackenberg_Data_Temperature_Air_temperature__200cm__60min_average__DegreesC.csv", 
                  stringsAsFactors=FALSE, header=TRUE,  sep="\t",
                  strip.white = T,na.strings = c("","NA"))
Ztemp$Variable <- "Temperature_C"
Ztemp$Site <- "Zack"
colnames(Ztemp) <- c('Date', "Time", "Value", "Variable", "Site")

Zprec <- read.csv("data/datasets/View_ClimateBasis_Zackenberg_Data_Precipitation_Precipitation_accumulated_mm.csv", 
                  stringsAsFactors=FALSE, header=TRUE,  sep="\t",
                  strip.white = T,na.strings = c("","NA"))
Zprec$Variable <- "Precipitation_mm"
Zprec$Site <- "Zack"
colnames(Zprec) <- c('Date', "Time", "Value", "Variable", "Site")


#  NUUK
Ntemp <- read.csv("data/datasets/View_ClimateBasis_Nuuk_Data_Temperature_Air_temperature__200_cm__30min_average__DegreesC.csv", 
                  stringsAsFactors=FALSE, header=TRUE,  sep="\t",
                  strip.white = T,na.strings = c("","NA"))
Ntemp$Variable <- "Temperature_C"
Ntemp$Site <- "Nuuk"
colnames(Ntemp) <- c('Date', "Time", "Value", "Variable", 'Site')

Nprec <- read.csv("data/datasets/View_ClimateBasis_Nuuk_Data_Precipitation_Precipitation_accumulated_mm.csv", 
                  stringsAsFactors=FALSE, header=TRUE,  sep="\t",
                  strip.white = T,na.strings = c("","NA"))
Nprec$Variable <- "Precipitation_mm"
Nprec$Site <- "Nuuk"
colnames(Nprec) <- c('Date', "Time", "Value", "Variable", "Site")
#-------------------------------------------------------------------------------------

# combine All dataset together
clim <- rbind(Ztemp, Zprec, Ntemp, Nprec)

# Remove -9999, NA
clim[clim$Value==-9999, 'Value'] <- NA
clim <- clim[complete.cases(clim), ]

# Devided Date into Year  Month Day
clim <- cbind(as.data.frame(str_split(clim$Date, "-", simplify=TRUE)), clim)
colnames(clim) <- c("Year", "Month", "Day", 'Date', "Time", "Value", "Variable", "Site")

# calculate the average Value per Month
clim_month <- as.data.frame(clim %>% 
  dplyr::group_by(Site, Year, Month, Variable) %>% 
  dplyr::summarise(Value=round(mean(Value), 2))) %>%
  dplyr::mutate(Site=as.factor(Site),
                Year=as.factor(Year), 
                Month=as.factor(Month),
                Variable=as.factor(Variable))


# Plot Climatic trends
ggplot(clim_month, aes(Year, Value, group=Site, color=Site)) +
         geom_point() +
         geom_smooth(method='lm', se=TRUE) +
         labs(y='Value') +
         ggtitle('Climatic trends in Nuuk and Zackenberg') +
         facet_grid(Variable~., scales="free_y") +
         theme_linedraw() 











