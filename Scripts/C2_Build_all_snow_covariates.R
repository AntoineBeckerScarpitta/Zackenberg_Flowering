#######################################################################################
#
#                           Greenland Flowering project
#                             C2 - SNOW COVARIATES
#
#######################################################################################
# Laura Antao & Antoine Becker-Scarpitta
# January 2022 ##updated 19-01-2021

#  clean R work-space
# rm(list=ls())

library(lubridate)  ##for dealing with dates


# 1 - NUUK -------------------------------------------------------------
# READ - Salix
Nsal0 <- read.csv("data/datasets/snow_cover_nuuk/View_BioBasis_Nuuk_Data_Vegetation_Salix_phenology_and_total_count180520202134518138.csv",
                  stringsAsFactors=FALSE, header=TRUE,  sep="\t", strip.white = T,na.strings = c("","NA"))
Nsal <- Nsal0 %>% 
  dplyr::select(Date, Plot, Section, Snow) %>%
  mutate(Site="Nuuk",
         #Species="SAL", 
         Variable="Snow_cover")


# READ - Silene
Nsil0 <- read.csv("data/datasets/snow_cover_nuuk/View_BioBasis_Nuuk_Data_Vegetation_Silene_phenology_and_total_count180520202134090595.csv",
                  stringsAsFactors=FALSE, header=TRUE,  sep="\t", strip.white = T,na.strings = c("","NA"))
Nsil <- Nsil0 %>% 
  dplyr::select(Date, Plot, Section, Snow) %>%
  mutate(Site="Nuuk",
         #Species="SIL", 
         Variable="Snow_cover")


# READ - Loiseleuria (Kalmia)
NLoi0 <- read.csv("data/datasets/snow_cover_nuuk/View_BioBasis_Nuuk_Data_Vegetation_Loiseleuria_phenology_and_total_count170920201306510146.csv",
                  stringsAsFactors=FALSE, header=TRUE,  sep="\t", strip.white = T,na.strings = c("","NA"))
NLoi <- NLoi0 %>% 
  dplyr::select(Date, Plot, Section, Snow) %>%
  mutate(Site="Nuuk",
         #Species="LOI", 
         Variable="Snow_cover")


# READ - Eriophorum
NEri0 <- read.csv("data/datasets/snow_cover_nuuk/View_BioBasis_Nuuk_Data_Vegetation_Eriophorum_total_count170920201325254357.csv",
                  stringsAsFactors=FALSE, header=TRUE,  sep="\t", strip.white = T,na.strings = c("","NA"))
NEri <- NEri0 %>% 
  dplyr::select(Date, Plot, Section, Snow) %>%
  mutate(Site="Nuuk",
         #Species="ERI", 
         Variable="Snow_cover")


# bind all dataset
Nsnow <- rbind(NEri, NLoi, Nsal, Nsil)


# rename plots
Nsnow[Nsnow$Plot=="SAL1", 'Plot'] <- "N_SAL1"
Nsnow[Nsnow$Plot=="SAL2", 'Plot'] <- "N_SAL2"
Nsnow[Nsnow$Plot=="SAL3", 'Plot'] <- "N_SAL3"
Nsnow[Nsnow$Plot=="SAL4", 'Plot'] <- "N_SAL4"

Nsnow[Nsnow$Plot=="SIL1", 'Plot'] <- "N_SIL1"
Nsnow[Nsnow$Plot=="SIL2", 'Plot'] <- "N_SIL2"
Nsnow[Nsnow$Plot=="SIL3", 'Plot'] <- "N_SIL3"
Nsnow[Nsnow$Plot=="SIL4", 'Plot'] <- "N_SIL4"

Nsnow[Nsnow$Plot=="lOI1", 'Plot'] <- "N_LOI1"  # correct typo
Nsnow[Nsnow$Plot=="LOI1", 'Plot'] <- "N_LOI1" 
Nsnow[Nsnow$Plot=="LOI2", 'Plot'] <- "N_LOI2"  
Nsnow[Nsnow$Plot=="LOI3", 'Plot'] <- "N_LOI3"  
Nsnow[Nsnow$Plot=="LOI4", 'Plot'] <- "N_LOI4"

Nsnow[Nsnow$Plot=="Eri1", 'Plot'] <- "N_ERI1" # typo mistake
Nsnow[Nsnow$Plot=="Eri2", 'Plot'] <- "N_ERI2"
Nsnow[Nsnow$Plot=="Eri3", 'Plot'] <- "N_ERI3"
Nsnow[Nsnow$Plot=="Eri4", 'Plot'] <- "N_ERI4"
Nsnow[Nsnow$Plot=="ERI1", 'Plot'] <- "N_ERI1"
Nsnow[Nsnow$Plot=="ERI2", 'Plot'] <- "N_ERI2"
Nsnow[Nsnow$Plot=="ERI3", 'Plot'] <- "N_ERI3"
Nsnow[Nsnow$Plot=="ERI4", 'Plot'] <- "N_ERI4"


# Recode NA
Nsnow[Nsnow$Snow==-9999, 'Snow'] <-  NA

remove("NEri", "NEri0", "NLoi", "NLoi0", "Nsal", "Nsal0", "Nsil", "Nsil0")
#-------------------------------------------------------------------------------------





# 1 - ZACKENBERG ----------------------------------------
# Snow cover (remarks on original fields only)
# load file
Zsnow <- read.csv("data/datasets/View_BioBasis_Zackenberg_Data_Abiotics_Snow_and_ice_cover220220221309182528.csv",
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
Zsnow$Plot[Zsnow$Plot== "Si4"]     <- "Sil4"
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
  dplyr::mutate(DOY=yday(Date))  %>%
  ##adding concatenation of Year and Plot (as these are the units to look at)
  dplyr::mutate(Year_Plot = as.factor(paste(Year, Plot, sep = "-"))) %>% 
  ##exclude late-season records (>late July)
  dplyr::filter(!Snow %in% c(-9999, NA),
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
##3.1 - 4 plots with super high estimated values 
# Extrem date ---> assign NA (2018 = NA LM does not work)
est_DOY$snowmelt_DOY[est_DOY$Year_Plot=="2018-Pap2"] <- NA
est_DOY$snowmelt_DOY[est_DOY$Year_Plot=="2018-Sal5"] <- NA
est_DOY$snowmelt_DOY[est_DOY$Year_Plot=="2018-Pap3"] <- NA
est_DOY$snowmelt_DOY[est_DOY$Year_Plot=="2018-Pap1"] <- NA
est_DOY$snowmelt_DOY[est_DOY$Year_Plot=="2018-Cas3"] <- NA  ##similar to the other 2018 decision
est_DOY$snowmelt_DOY[est_DOY$Year_Plot=="2002-Dry4"] <- NA  
est_DOY$snowmelt_DOY[est_DOY$Year_Plot=="2018-Cas4"] <- NA

# (remove larger than 365!) + NA
snow <- est_DOY %>% filter(snowmelt_DOY<365)

# basic structure
snow[c('Plot', "Site")] <-  lapply(snow[c('Plot', "Site")], as.factor)
snow$Year <-  as.numeric(snow$Year)
snow <- subset(snow, select=-c(DOY, Year_Plot))
##end

remove("aux2", "est_DOY", 'Zsnow', "Nsnow", "All_snow")



