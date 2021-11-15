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
  mutate(DOY=yday(Date))  %>%
  mutate(Year_Plot = as.factor(paste(Year, Plot, sep = "_")))  ##adding concatenation of Year and plot (as these are the units to look at)


##some checks
#how many plots
n_distinct(Zsnow$Plot)  #28
n_distinct(Zsnow$Year_Plot) #537



##updated 18-12-2020 ####
##after meeting with Niels and Tomas ==>> simplified everything to simply estimate the DOY for the snowmelt=50 using lm's for all

##also fixing the other plot name
Zsnow$Plot[Zsnow$Plot== "Sax1Si2"] <- "Sax1Sil2"


est_DOY<- Zsnow %>%
  filter(DOY<200) %>%
  group_by(Year_Plot) %>% 
  do(lm(DOY ~ Value, data = .) %>% 
       predict(., data.frame(Value = 50)) %>%
       data_frame(DOY = .)) %>%
  ungroup() %>%
  
  ##to deal with "negative" or "small" estimates
  mutate(DOY1= ifelse(DOY<120, "NA", DOY)) %>%
  
  ##splitting the "double" plots
  separate(., Year_Plot, into= c("Year", "Plot"), sep="_", remove=F) %>%
  separate(., Plot, sep = 4, into= c("subPlot1", "subPlot2"))  #example "Dry2Sal7"


##### to do list for January
#####1. need to check again how to calculate these estimates for the plots (ignoring sections) with values always <50

aux<- Zsnow %>%
  group_by(Year_Plot) %>%
  summarise(check50min= ifelse(min(Value<50), "check", "ok"))


aux %>% filter(check50min=="check") %>% tally()  ##89 Year_Plot that have values<50



#####2. If we had used the previous approach to use the 50 values for the ones that have one, this would not necessarily lead to the same estimated DOY via lm!!!
##two examples

plot(data=Zsnow[Zsnow$Year_Plot=="1997_Sal4",], DOY~Value)
plot(data=Zsnow[Zsnow$Year_Plot=="1996_Sil4",], DOY~Value)

###plus we were not excluding DOY>200 before for these ones of course, but including them or not for the lm's also makes a difference
plot(data=Zsnow[Zsnow$Year_Plot=="1996_Sil4"& Zsnow$DOY<200,], DOY~Value)




ggplot(Zsnow %>%
         filter(Year_Plot %in% aux$Year_Plot)) +
  geom_point(aes(x=Value, y= DOY, group=Year_Plot)) +
  facet_wrap(~Year_Plot)



##then split and re-bind to have a single plot variable including the ones from the "double plots"
# est_DOY_1<- est_DOY %>%
#   select(-subPlot2) %>%
#   rename(Plot=subPlot1)
#   
# est_DOY_2<- est_DOY %>%
#   filter(!subPlot2=="") %>%
#   select(-subPlot1) %>%
#   rename(Plot=subPlot2)


est_DOY_end <- rbind(est_DOY %>%
                       select(-subPlot2) %>%
                       rename(Plot=subPlot1),
                     
                     est_DOY %>%
                       filter(!subPlot2=="") %>%
                       select(-subPlot1) %>%
                       rename(Plot=subPlot2))




##checking the ones with negative or very low estimated DOY
#View(Zsnow[Zsnow$Year_Plot=="2007_Sax2Sil2",])
#View(Zsnow[Zsnow$Year_Plot=="2000_Sax2Sil2",])
#View(Zsnow[Zsnow$Year_Plot=="1999_Sax2Sil2",])

plot(data=Zsnow[Zsnow$Year_Plot=="1996_Sal1" & Zsnow$DOY<200,], DOY ~ Value)
mod<-lm(DOY ~ Value, data=Zsnow[Zsnow$Year_Plot=="1996_Sal1" & Zsnow$DOY<200,])
abline(mod)

est_DOY$DOY[est_DOY$Year_Plot== "1996_Sil4"]


##there a few others where the estimated DOY is way low in the year  -- also have values never reach 50 === e.g. 2000_Sal3 and 2008_Sax1Sil1
##but we have to ignore DOY>200 anyway
View(est_DOY %>%arrange(DOY))










###############################################################################################################################################################
###############################################################################################################################################################
###############################################################################################################################################################

###this is what we had done before the meeting!!!!

needtohave<- Zsnow %>%
  distinct(Year_Plot)

###different steps now, depending on which subset of data we have

#1. select records that have a 50 value
Zsnow_meltday50 <- Zsnow %>%
  
  # Select the DOY with 50 of snow_cover (this will be the date of snow melt)
  group_by(Year, Plot, Section) %>%
  filter(Value==50) %>%
  mutate(Year_Plot = as.factor(paste(Year, Plot, sep = "_"))) %>%  ##adding concatenation of Year and plot (as these are the units to look at)
  ungroup()


n_distinct(Zsnow_meltday50$Plot)  #25
n_distinct(Zsnow_meltday50$Year_Plot)  #152


##then doing some checks

#1.1. how many DOY per plot and year -- to see if there is differences between sections
check1<- Zsnow_meltday50 %>% 
  group_by(Year, Plot) %>% 
  summarise(n= n_distinct(DOY)) %>% 
  arrange(., Year, Plot)

##for the ones with only one DOY - use that value
doy_ok1<- check1 %>%
  filter(n==1) %>%
  mutate(Year_Plot = as.factor(paste(Year, Plot, sep = "_")))

# needtohave<- needtohave %>% left_join(., Zsnow_meltday50 %>%
#                                         select(-c(Section)) %>%
#                                         distinct(Year_Plot, .keep_all = T) %>%
#                                         filter(Year_Plot %in% doy_ok1$Year_Plot),  ##this has 114 Year_Plot!! (152-114=38 in check2)
#                                       by="Year_Plot")
##this was adding all the variables, which we might not need

##so just joining the DOY info
needtohave<- needtohave %>% left_join(., Zsnow_meltday50 %>%
                                        select(Year_Plot, DOY) %>%
                                        distinct(Year_Plot, .keep_all = T) %>%
                                        filter(Year_Plot %in% doy_ok1$Year_Plot),  ##this has 114 Year_Plot!! (152-114=38 in check2)
                                      by="Year_Plot")

dim(needtohave[is.na(needtohave$DOY),])

##now deal with ones with more than one DOY 
check2 <- check1 %>% 
  
  ##remove the ones that have only a single DOY==all good
  filter(n!=1) %>%
  mutate(Year_Plot = as.factor(paste(Year, Plot, sep = "_")))


##now check the differences in DOY between those sections
auxdata1 <- Zsnow_meltday50 %>%
  filter(Year_Plot %in% check2$Year_Plot)  ##get the records that match the problematic Year_Plot


ggplot(auxdata1) +
  geom_point(aes(x=Year_Plot, y=DOY, colour=Section)) +
  theme(axis.text.x = element_text(angle = 90))

##lots of variation for some plots, with very late dates

##11-12-2020: decided to assume all the dates in Sept/Oct (i.e. DoY>250) must be a mistake (or maybe already snow in late Autumn?)

##variation for the dates around DoY=150 seems more acceptable, but still a few plots have ~30 days between sections/visits
##see e.g. View(auxdata1[auxdata1$Year==2013 & auxdata1$Plot=="Cas4",])

ggplot(auxdata1 %>%
         filter(DOY<200)) +
  geom_point(aes(x=Year_Plot, y=DOY, colour=Section)) +
  theme(axis.text.x = element_text(angle = 90))



##so decided we will calculate the mean of the values for each Year_Plot "ignoring DOY>200" for the ones that have multiple days

##checking how many plots would still have different values per section
auxdata2 <- auxdata1 %>%
  filter(DOY<200) %>%   ##add this criterion
  group_by(Year_Plot) %>%
  summarise(n=n_distinct(DOY)) %>%
  filter(n!=1)   ##only 13 cases now  ##but this one is just to plot

##just plot these ones to see the variation again
ggplot(auxdata1 %>%
         filter(Year_Plot %in% auxdata2$Year_Plot) %>%
         filter(DOY<200)) +
  geom_point(aes(x=Year_Plot, y=DOY, colour=Section)) +
  theme(axis.text.x = element_text(angle = 90))

##most look ok ==> calculate mean DOY to have a value per plot (2013_Cas4 is the only problematic one)


##for all the ones with >one DOY - use mean DOY
doy_ok2<- auxdata1 %>%
  group_by(Year_Plot) %>%
  summarise(meandoy= mean(DOY[DOY<200])) %>%
  distinct(Year_Plot, .keep_all = T) %>%
  ungroup()

##this gives some NAs for the ones with DOY>200 
##so checking those
doy_ok2 %>% filter(is.na(meandoy))


##and then add the other manually???
View(auxdata1[auxdata1$Year_Plot=="2009_Sal6",])  ##all look good?

doy_ok3<- auxdata1 %>%
  filter(Year_Plot %in% doy_ok2$Year_Plot[is.na(doy_ok2$meandoy)]) %>%
  group_by(Year_Plot) %>%
  summarise(meandoy= mean(DOY)) %>%
  distinct(Year_Plot, .keep_all = T) %>%
  ungroup()


##combining both
to_add<- rbind(doy_ok2 %>% filter(!is.na(meandoy)),
               doy_ok3)

##add these ones to needtohave
needtohave$DOY[needtohave$Year_Plot %in% to_add$Year_Plot] <- to_add$meandoy

dim(needtohave[is.na(needtohave$DOY),])



# ###"updating" Zsnow_meltday50
# 
# ##create new variable
# Zsnow_meltday50$DOYnew <- Zsnow_meltday50$DOY
# 
# ###add the mean values just above
# Zsnow_meltday50 <- left_join(Zsnow_meltday50, doy_to_add, by="Year_Plot")  ##will have NAs for the others
# 




#1.2 which plots are missing (ie. do not have a 50 value to subset)
miss_plot<- Zsnow %>%
  filter(!Year_Plot %in% Zsnow_meltday50$Year_Plot)

unique(miss_plot$Plot)  ##28
n_distinct(miss_plot$Year_Plot) #385 models

View(arrange(miss_plot[miss_plot$Year_Plot == "1997_Cas6",], Date))  ##"value" always zero!
View(arrange(miss_plot[miss_plot$Year_Plot == "1998_Cas6",], Date))


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
plot(data=miss_plot[miss_plot$Year_Plot=="1998_Cas6",], DOY ~ Value)

mod<-lm(DOY ~ Value, data=miss_plot[miss_plot$Year_Plot=="1998_Cas6",])
abline(mod)
abline(v=50, col="blue")

# v50<-data.frame(Value=c(50))
# predict(mod, newdata = v50)

abline(h=predict(mod, newdata = data.frame(Value=c(50))), col="red")  ##value from predict


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
       data_frame(DOY = .))


##checking another one
plot(data=miss_plot[miss_plot$Year_Plot=="2015_Dry6Pap4",], DOY ~ Value)
mod<-lm(DOY ~ Value, data=miss_plot[miss_plot$Year_Plot=="2015_Dry6Pap4",])
abline(mod)
abline(v=50, col="blue")
abline(h=predict(mod, newdata = data.frame(Value=c(50))), col="red")  ##value from predict


est_DOY$DOY[est_DOY$Year_Plot== "2015_Dry6Pap4"]
abline(h=est_DOY$DOY[est_DOY$Year_Plot== "2015_Dry6Pap4"], col="green", lwd=2)  ##value from est_DOY  IT works!!!!!


summary(est_DOY$DOY)  ##something is wrong!!

##just plotting density
ggplot(est_DOY) +
  geom_density(aes(DOY))

View(est_DOY[est_DOY$DOY<0,])   ###===>>> the range of values is always <50 for these plot_year, so the models are crap....



##checking these ones
plot(data=miss_plot[miss_plot$Year_Plot=="2008_Sax1Sil1",], DOY ~ Value)
mod<-lm(DOY ~ Value, data=miss_plot[miss_plot$Year_Plot=="2008_Sax1Sil1",])
abline(mod)
abline(v=50, col="blue")
abline(h=predict(mod, newdata = data.frame(Value=c(50))), col="red")  ##value from predict





##add NOW these ones to needtohave and then exclude the weird ones (for now?)
needtohave$DOY[needtohave$Year_Plot %in% miss_plot$Year_Plot] <- est_DOY$DOY

dim(needtohave[is.na(needtohave$DOY),])  ##no more NAs


needtohave<- needtohave %>%
  separate(., Year_Plot, into= c("Year", "Plot"), sep="_", remove=F)

##adding the other info from original data
# SNOW_final<- needtohave %>%
#   left_join(., Zsnow, by= "Year_Plot")



##just plotting density
ggplot(needtohave %>%
         filter(DOY>0)) +
  geom_density(aes(DOY))


ggplot(needtohave %>%
         filter(DOY>0)) +
  geom_density(aes(DOY)) +
  facet_wrap(~Plot)









# I think we deal the plot level and not the section, thus:
# ...section marked as A-D, can be renamed ==A
# ...plot==ERI, ART, VEG, can be remove
# ...we need to figured out the doubles plots (e.g. Sax1Sil1, Sax2Sil2...)
# ...correct mistakes Plot==Si4, Sax3Si3 in Plot==Sil4, Sax3Sil3
# we need at the end one table with the DOY when the plot has 50% snow cover of a given  year
# this DOY will be one explainatory variable with Tem, Prec, Humd and Time





