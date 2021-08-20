#######################################################################################
#
#                           Greenland Flowering project
#                             C2 - SNOW COVARIATES
#
#######################################################################################
# Laura Antao & antoine Becker-Scarpitta
# January 2021 ##updated 19-01-2021

#  clean R work-space
rm(list=ls())

library(lubridate)  ##for dealing with dates

# Load Nuuk Snow cover variable
source("scripts/C1_Load_Nuuk_snow_data.R")


# 1 - ZACKENBERG ----------------------------------------
# Snow cover (remarks on original fields only)
# load file
Zsnow <- read.csv("data/datasets/View_BioBasis_Zackenberg_Data_Abiotics_Snow_and_ice_cover02122020113324881.csv",
                  stringsAsFactors=FALSE, header=TRUE,  sep="\t",
                  strip.white = T,na.strings = c("","NA"))

# create a new col==Variable
Zsnow <- as.data.frame(Zsnow %>%
  mutate(Variable = "Snow_cover", 
         Site = "Zackenberg") %>% 
    rename(Snow=SnowCoverFraction) %>%
    dplyr::select(Date, Plot, Section, Snow, 
                  Site, Variable, Site)) %>%
  #remove plots==ERI, ART, VEG
    filter(!Plot %in% c("Art1","Art2","Art3","Art4","Art5",
                        "Art6","Art7","Eri1","Eri2","Eri3",
                        "Eri4", "Veg1"))

##renaming plots and sections
Zsnow$Plot[Zsnow$Plot== "Sax1Si1"] <- "Sax1Sil1"
Zsnow$Plot[Zsnow$Plot== "Sax2Si2"] <- "Sax2Sil2"
Zsnow$Plot[Zsnow$Plot== "Sax3Si3"] <- "Sax3Sil3"
Zsnow$Plot[Zsnow$Plot== "Si4"] <- "Sil4"
Zsnow$Plot[Zsnow$Plot== "Sax1Si2"] <- "Sax1Sil2"

Zsnow$Section[Zsnow$Section== "A-D"] <- "A"


# seperate the double names  #e.g. "Dry2Sal7"
Zsnow_temp <- Zsnow %>% 
  separate(Plot, sep = 4, into= c("subPlot1", "subPlot2"))
  
Zsnow <- rbind(Zsnow_temp %>% dplyr::select(-subPlot2) %>%
                rename(Plot=subPlot1),
               
               Zsnow_temp %>% filter(!subPlot2=="") %>%
                dplyr::select(-subPlot1) %>%
                rename(Plot=subPlot2))

remove("Zsnow_temp")
#-------------------------------------------------------------------------------------




# 2 - BIND NUUK + ZACKENBERG ---------------------------------------------------------
All_snow <- rbind(Zsnow, Nsnow)

# divided the date into year, month, day
All_snow <- cbind(as.data.frame(str_split(All_snow$Date, "-", simplify=TRUE)), All_snow)
colnames(All_snow) <- c("Year", "Month", "Day", 'Date', "Plot", "Section", 
                     "Snow", "Site",  "Variable")

# Add DOY col AND remove NA
All_snow <- All_snow %>%
  # Create new col DOY=day of the year based on original date
  mutate(DOY=yday(Date))  %>%
  ##adding concatenation of Year and Plot (as these are the units to look at)
  mutate(Year_Plot = as.factor(paste(Year, Plot, sep = "-"))) %>% 
  ##exclude late-season records (>late July)
  filter(!Snow %in% c(-9999, NA),
         DOY<200)

# how many plots
# n_distinct(All_snow$Plot) #44
# n_distinct(All_snow$Year_Plot) #693 
#(was 732 before all selection (<200, NA)
#(was 717 after removing NA only)
#-------------------------------------------------------------------------------------



# 3 - LM to PREDICT the DOY with 50% snow cover --------------------------------------
##1. use lm's for all Year_Plot to get estimated DOY with 50% snow cover using predict()

est_DOY <- All_snow %>%
  group_by(Year_Plot, Site) %>% 
  do(lm(DOY ~ Snow, data = .) %>% 
       predict(., data.frame(Snow = 50)) %>%
       tibble(DOY = .)) %>%
  ungroup() %>%
  ##to deal with negative, small or very large estimates (was doing this before)
  ##adding new variable to indicate if estimated DOY is < 30 April
  ##and also if estimated DOY is > mid July
  # mutate(DOY1= ifelse(DOY<120, "NA", 
  #                     ifelse(DOY>200, "NA", DOY))) %>%  ##might not be necessary anymore
  separate(., Year_Plot, into= c("Year", "Plot"), sep="-", remove=F)
        

##2. identify which plots never reach 50 value
aux2 <- All_snow  %>%
  group_by(Year_Plot, Site) %>%
  summarize(max_cover = max(Snow)) %>%
  filter(max_cover<50)  ##212 Year_Plot that only have values<50

##for all of these, set a NA, keeping the estimated lm value for the others
est_DOY <- est_DOY %>%
  mutate(snowmelt_DOY= ifelse(Year_Plot %in% aux2$Year_Plot, NA, DOY))



##3. check and decide what to do with estimated DOYs too small or too large that still remain
##3.1 - 4 plots with super high estimated values (larger than 365!) + NA
snow <- est_DOY %>% filter(snowmelt_DOY<365)

# all 2018 ---> assign NA
# est_DOY$snowmelt_DOY[est_DOY$Year_Plot=="2018_Pap2Sal5"] <- NA
# est_DOY$snowmelt_DOY[est_DOY$Year_Plot=="2018_Pap3"] <- NA
# est_DOY$snowmelt_DOY[est_DOY$Year_Plot=="2018_Pap1"] <- NA
# est_DOY$snowmelt_DOY[est_DOY$Year_Plot=="2018_Cas4"] <- NA
# est_DOY$snowmelt_DOY[est_DOY$Year_Plot=="2018_Cas3"] <- NA  ##similar to the other 2018 decision
# est_DOY$snowmelt_DOY[est_DOY$Year_Plot=="2002_Dry4"] <- NA  ##similar to the other 2018 decision

snow[c('Plot', "Site")] <-  lapply(snow[c('Plot', "Site")], as.factor)
snow$Year <-  as.numeric(snow$Year)
snow <- subset(snow, select=-c(DOY, Year_Plot))
##end

remove("aux2", "est_DOY", 'Zsnow', "Nsnow" )



