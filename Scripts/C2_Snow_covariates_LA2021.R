##Laura cleaning and dealing with snow cover data
##aim is to get one DOY value corresponding to 50% snow cover for each plot in each year

##updated 19-01-2021


library(dplyr)
library(stringr)

library(tidyverse)
library(lubridate)  ##for dealing with dates

#### Data cleaning etc.####

# Snow cover (remarks on original fields only)
# load file
Zsnow <- read.csv("data/datasets/View_BioBasis_Zackenberg_Data_Abiotics_Snow_and_ice_cover02122020113324881.csv",
                  stringsAsFactors=FALSE, header=TRUE,  sep="\t",
                  strip.white = T,na.strings = c("","NA"))

Zsnow<- Zsnow %>% dplyr::rename(Date=ï..Date)  ##for some reason this changed when importing the file

# create a new col==Variable
Zsnow$Variable <- "Snow_cover"

# create new col==Site
Zsnow$Site <- "Zackenberg"

# remove field and general remarks
Zsnow <- as.data.frame(Zsnow %>% 
                         dplyr::select(Date, Plot, Section, SnowCoverFraction, 
                                       Variable, Site)) %>%
  
  #remove plots==ERI, ART, VEG
  filter(!Plot %in% c("Art1","Art2","Art3","Art4","Art5","Art6","Art7",
                      "Eri1","Eri2","Eri3","Eri4", "Veg1"))

# divided the date into year, month, day
Zsnow <- cbind(as.data.frame(str_split(Zsnow$Date, "-", simplify=TRUE)), Zsnow)
colnames(Zsnow) <- c("Year", "Month", "Day", 'Date', "Plot", "Section", 
                     "Value", "Variable", "Site")


##renaming plots and sections
sort(unique(Zsnow$Plot))

# dim(Zsnow[Zsnow$Plot== "Sax1Si1",])
# dim(Zsnow[Zsnow$Plot== "Sax1Sil1",])

Zsnow$Plot[Zsnow$Plot== "Sax1Si1"] <- "Sax1Sil1"
Zsnow$Plot[Zsnow$Plot== "Sax2Si2"] <- "Sax2Sil2"
Zsnow$Plot[Zsnow$Plot== "Sax3Si3"] <- "Sax3Sil3"
Zsnow$Plot[Zsnow$Plot== "Si4"] <- "Sil4"
Zsnow$Plot[Zsnow$Plot== "Sax1Si2"] <- "Sax1Sil2"

Zsnow$Section[Zsnow$Section== "A-D"] <- "A"


# Remove -9999, NA
Zsnow[Zsnow$Value==-9999, 'Value'] <- NA
Zsnow <- Zsnow[complete.cases(Zsnow), ]


#### TO DO @Laura ####


Zsnow <- Zsnow %>%
  
  # Create new col DOY=day of the year based on original date
  mutate(DOY=yday(Date))  %>%
  mutate(Year_Plot = as.factor(paste(Year, Plot, sep = "_"))) %>%  ##adding concatenation of Year and Plot (as these are the units to look at)
  
  ##exclude late-season records (>late July)
  filter(DOY<200)




#how many plots
n_distinct(Zsnow$Plot)  #28
n_distinct(Zsnow$Year_Plot) #531 (was 537 before setting the filter above to remove all records >200)


##1. use lm's for all Year_Plot to get estimated DOY with 50% snow cover using predict() ####

est_DOY<- Zsnow %>%
  
  group_by(Year_Plot) %>% 
  do(lm(DOY ~ Value, data = .) %>% 
       predict(., data.frame(Value = 50)) %>%
       data_frame(DOY = .)) %>%
  ungroup() %>%
  
  ##to deal with negative, small or very large estimates (was doing this before)
  ##adding new variable to indicate if estimated DOY is < 30 April
  ##and also if estimated DOY is > mid July
  # mutate(DOY1= ifelse(DOY<120, "NA", 
  #                     ifelse(DOY>200, "NA", DOY))) %>%  ##might not be necessary anymore
  
  separate(., Year_Plot, into= c("Year", "Plot"), sep="_", remove=F) %>%
  
  ##and splitting the "double" plots
  separate(., Plot, sep = 4, into= c("subPlot1", "subPlot2"))  #e.g. "Dry2Sal7"


# ##this has 6 Year_Plot less than total number!
# ##which ones are missing?
# Zsnow %>% distinct(Year_Plot) %>% filter(!Year_Plot %in% est_DOY$Year_Plot)
# 
# # 1997_Dry7    ===> DOY always >200, i.e. all "late" plots
# # 1997_Dry8
# # 1997_Cas5
# # 1997_Cas6
# # 2003_Sal6
# # 2009_Sal7
# 
# View(Zsnow[Zsnow$Year_Plot=="1997_Dry7",])
# 
# ##Will add as NA!!! TO DO


##2. for the Year_Plot which never reach a 50 snow cover value ####
##==> input a given DOY (e.g. NA (would models run?); 0 or 1 (have meaning and would induce outliers?); a more sensible value like end of march or something like that)


###identify which plots never reach 50 value
aux2<- Zsnow  %>%
  group_by(Year_Plot) %>%
  summarize(max.pt = max(Value)) %>%
  filter(max.pt<50)  ##114 Year_Plot that only have values<50


##for all of these, set a given DOY, keeping the estimated lm value for the others
est_DOY<- est_DOY %>%
  mutate(finalDOY= ifelse(Year_Plot %in% aux2$Year_Plot, 90, DOY))  ##now end of March


View(arrange(est_DOY, finalDOY))


##3. check and decide what to do with estimated DOYs too small or too large that still remain ####


##3.1 - 4 plots with super high estimated values (larger than 365!), all 2018 ---> assign NA

est_DOY$finalDOY[est_DOY$Year_Plot=="2018_Pap2Sal5"] <- NA
est_DOY$finalDOY[est_DOY$Year_Plot=="2018_Pap3"] <- NA
est_DOY$finalDOY[est_DOY$Year_Plot=="2018_Pap1"] <- NA
est_DOY$finalDOY[est_DOY$Year_Plot=="2018_Cas4"] <- NA


est_DOY$finalDOY[est_DOY$Year_Plot=="2018_Cas3"] <- NA  ##similar to the other 2018 decision
est_DOY$finalDOY[est_DOY$Year_Plot=="2002_Dry4"] <- NA  ##similar to the other 2018 decision





##
##3.4. other checks

u="2008_Sax2Sil2"

##order finalDOY -- check first one with normal value "2008_Sax2Sil2"
View(Zsnow[Zsnow$Year_Plot==u,])

plot(data=Zsnow[Zsnow$Year_Plot==u,], DOY~Value, xlim=c(0,100), ylim=c(0,200))
mod<-lm(DOY ~ Value, data=Zsnow[Zsnow$Year_Plot==u & Zsnow$DOY<200,])
abline(mod)
abline(lm(DOY ~ Value, data=Zsnow[Zsnow$Year_Plot==u,]), col="grey80")

abline(v=50, col="blue")
abline(h=predict(mod, newdata = data.frame(Value=c(50))), col="red")  ##value from predict

est_DOY$DOY[est_DOY$Year_Plot== u]
predict(lm(DOY ~ Value, data=Zsnow[Zsnow$Year_Plot==u,]), newdata = data.frame(Value=c(50)))

plot(data=Zsnow[Zsnow$Year_Plot==u & Zsnow$DOY<200,], DOY~Value, xlim=c(0,100))




