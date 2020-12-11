library(dplyr)
library(stringr)


# Snow cover (remarks on original fields only)
# load file
Zsnow <- read.csv("data/datasets/View_BioBasis_Zackenberg_Data_Abiotics_Snow_and_ice_cover02122020113324881.csv",
                  stringsAsFactors=FALSE, header=TRUE,  sep="\t",
                  strip.white = T,na.strings = c("","NA"))
# create a new col==Variable
Zsnow$Variable <- "Snow_cover"

# create new col==Site
Zsnow$Site <- "Zackenberg"

# remove field and general remarks
Zsnow <- as.data.frame(Zsnow %>% 
                         dplyr::select(Date, Plot, Section, SnowCoverFraction, 
                                       Variable, Site))

# devided the date into year, month, day
Zsnow <- cbind(as.data.frame(str_split(Zsnow$Date, "-", simplify=TRUE)), Zsnow)
colnames(Zsnow) <- c("Year", "Month", "Day", 'Date', "Plot", "Section", 
                     "Value", "Variable", "Site")


# TO DO 
# Create a col DOY=day of the year based on original date
# Select the DOY with 50 of snow_cover (this will be the date of snow melt)
# I think we deal the plot level and not the section, thus:
# ...section marked as A-D, can be renamed ==A
# ...plot==ERI, ART, VEG, can be remove
# ...we need to figured out the doubles plots (e.g. Sax1Sil1, Sax2Sil2...)
# ...correct mistakes Plot==Si4, Sax3Si3 in Plot==Sil4, Sax3Sil3
# we need at the end one table with the DOY when the plot has 50% snow cover of a given  year
# this DOY will be one explainatory variable with Tem, Prec, Humd and Time





