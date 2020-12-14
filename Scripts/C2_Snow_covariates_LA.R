library(dplyr)
library(stringr)


library(tidyverse)
library(lubridate)  ##for dealing with dates

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

# devided the date into year, month, day
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
Zsnow$Section[Zsnow$Section== "A-D"] <- "A"

# Remove -9999, NA
Zsnow[Zsnow$Value==-9999, 'Value'] <- NA
Zsnow <- Zsnow[complete.cases(Zsnow), ]


# TO DO @Laura

Zsnow <- Zsnow %>%
  
  # Create a col DOY=day of the year based on original date
  mutate(DOY=yday(Date)) 



###different steps now, depending on which subset of data we have

#1. select records that have a 50 value
Zsnow_meltday50 <- Zsnow %>%
  
  # Select the DOY with 50 of snow_cover (this will be the date of snow melt)
  group_by(Year, Plot, Section) %>%
  filter(Value==50) %>%
  mutate(Year_Plot = paste(Year, "_", Plot))  ##adding concatenation of Year and plot (as these are the units to look at)


##then doing some checks

#1.1. how many DOY per plot and year -- to see if there is differences between sections
check1<- Zsnow_meltday50 %>% 
  group_by(Year, Plot) %>% 
  summarise(n=n_distinct(DOY)) %>% 
  arrange(., Year, Plot) %>% 
  
  ##remove the ones that have only a single DOY==all good
  filter(n!=1) %>%
  mutate(Year_Plot = paste(Year, "_", Plot))


##now check the differences in DOY between those sections
auxdata1 <- Zsnow_meltday50 %>%
  filter(Year_Plot %in% check1$Year_Plot)  ##get the records that match the problematic Year_Plot



ggplot(auxdata1) +
  geom_point(aes(x=Year_Plot, y=DOY, colour=Section)) +
  theme(axis.text.x = element_text(angle = 90))

##lots of variation for some plots, with very late dates

##11-12-2020: decided to assume all the dates in Sep/Oct (i.e. DoY>250) must be a mistake (or maybe already snow in late Autumn?)

##variation for the dates around DoY=150 seems more acceptable, but still a few plots have ~30 days between sections/visits
##see e.g. View(auxdata1[auxdata1$Year==2013 & auxdata1$Plot=="Cas4",])

ggplot(auxdata1 %>%
         filter(DOY<200)) +
  geom_point(aes(x=Year_Plot, y=DOY, colour=Section)) +
  theme(axis.text.x = element_text(angle = 90))



##so decided we will calculate the mean of the values for each Year_Plot "ignoring DOY>200"

##checking how many plots would still have different values per sectio
auxdata2 <- auxdata1 %>%
  filter(DOY<200) %>%   ##add this criterion
  group_by(Year_Plot) %>%
  summarise(n=n_distinct(DOY)) %>%
  filter(n!=1)   ##only 13 cases now

##just plot these ones
ggplot(auxdata1 %>%
         filter(Year_Plot %in% auxdata2$Year_Plot) %>%
         filter(DOY<200)) +
  geom_point(aes(x=Year_Plot, y=DOY, colour=Section)) +
  theme(axis.text.x = element_text(angle = 90))

##most look ok ==> calculate mean DOY to have a value per plot (2013_Cas4 is the only problematic one)

doy_to_add<- auxdata1 %>%
  filter(Year_Plot %in% auxdata2$Year_Plot) %>%
  filter(DOY<200) %>%
  group_by(Year_Plot) %>%
  summarise(meandoy= mean(DOY))



###"updating" Zsnow_meltday50

##create new variable
Zsnow_meltday50$DOYnew <- Zsnow_meltday50$DOY

###add the mean values just above
Zsnow_meltday50 <- left_join(Zsnow_meltday50, doy_to_add, by="Year_Plot")  ##will have NAs for the others





#1.2 which plots are missing (ie. do not have a 50 value to subset)
miss_plot<- Zsnow %>%
  filter(!Plot %in% Zsnow_meltday50$Plot) %>%
  mutate(Year_Plot = paste(Year, "_", Plot))

unique(miss_plot$Plot)  ##[1] "Dry7"    "Cas6"    "Sax1Si2"
n_distinct(miss_plot$Year_Plot) #18 models

View(arrange(miss_plot[miss_plot$Plot=="Cas6" & miss_plot$Year==1997,], Date))  ##"value" always zero!
View(arrange(miss_plot[miss_plot$Plot=="Cas6" & miss_plot$Year==1998,], Date))


##for these ones, do a lm() and get the value for x=50
# 
# auxmodels<- miss_plot %>%
#   group_by(Year_Plot) %>%
#   do(mod= lm(Value ~ DOY, data=.) %>%
#        predict(.,value=50))
# 
# 
# coeffs <- rbind(broom::tidy(auxmodels, mod) %>%
#                      filter(term!= "(Intercept)") %>%
#                      mutate(variable="DOY"))
#   -->>>> not working


###AND needs to be the other way around right?

##example to test if this is correct
plot(data=miss_plot[miss_plot$Year_Plot=="1998 _ Cas6",], DOY ~ Value)

mod<-lm(DOY ~ Value, data=miss_plot[miss_plot$Year_Plot=="1998 _ Cas6",])
abline(mod)
abline(v=50, col="blue")

predict(mod, newdata = 50)

abline(v=50, col="blue")


##https://stackoverflow.com/questions/51677597/using-group-by-from-dplyr-with-predict-lm-and-do-in-a-pipe-for-year-linear-e
# test.frame %>%
#   group_by(Country, Entity) %>% 
#   do(lm( value ~ Year , data = .) %>% 
#        predict(., data.frame(Year = 1993)) %>%
#        data_frame(Year = 1993, value = .)) %>%
#   bind_rows(test.frame)


est_DOY<- miss_plot %>%
  group_by(Year_Plot) %>% 
  do(lm(DOY ~ Value, data = .) %>% 
       predict(., data.frame(Value = 50)) %>%
       data_frame(Value = 50, DOY = .))

abline(h=188.4288, col="red")  ##value from est_DOY

# I think we deal the plot level and not the section, thus:
# ...section marked as A-D, can be renamed ==A
# ...plot==ERI, ART, VEG, can be remove
# ...we need to figured out the doubles plots (e.g. Sax1Sil1, Sax2Sil2...)
# ...correct mistakes Plot==Si4, Sax3Si3 in Plot==Sil4, Sax3Sil3
# we need at the end one table with the DOY when the plot has 50% snow cover of a given  year
# this DOY will be one explainatory variable with Tem, Prec, Humd and Time





