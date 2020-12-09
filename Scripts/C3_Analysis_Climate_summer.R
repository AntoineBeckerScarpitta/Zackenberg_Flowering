#######################################################################################
#
#                           Greenland Flowering project
#                            04 - Analysis covariates
#
#######################################################################################
# Antoine Becker-Scarpitta
# November 2020

#  clean R work-space
rm(list=ls())

# Load 02 - Creation database (+ scripts 00 and 01)
source("Scripts/00_Load_libraries.r")
source("Scripts/01_Import_DB.r")
source("Scripts/02_Creation_DB.r")
source("Scripts/02_Climatic_covariates.R")


# DAY add summer cols
clim_day$Season <- "winter"
clim_day[clim_day$Month=="06", "Season"] <- "summer"
clim_day[clim_day$Month=="07", "Season"] <- "summer"
clim_day[clim_day$Month=="08", "Season"] <- "summer"
# new db with summer value only
clim_day_s <- clim_day[clim_day$Season=="summer", ]

# MONTH add summer cols
clim_month$Season <- "winter"
clim_month[clim_month$Month=="06", "Season"] <- "summer"
clim_month[clim_month$Month=="07", "Season"] <- "summer"
clim_month[clim_month$Month=="08", "Season"] <- "summer"
# new db with summer value only
clim_month_s <- clim_month[clim_month$Season=="summer", ]


# calculate the average Value per Year
clim_year_s <- as.data.frame(clim_month_s %>% 
             dplyr::group_by(Site, Year, Season, Variable) %>% 
             dplyr::summarise(Value=round(mean(Value), 2))) %>%
             dplyr::mutate(Site=as.factor(Site),
             Year=as.numeric(Year), 
             Variable=as.factor(Variable), 
             Season=as.factor(Season))%>%
             ungroup()

par(mfrow=c(1,2))
### CORRELATION BTW PRECIPITATION~HUMIDITY #-------------------------------------------------------
# NUUK DAY
# different dimensions
# ZACK DAY
# different dimensions


# NUUK MONTH
cor(clim_month_s[clim_month_s$Site=="Nuuk" & clim_month_s$Variable=="Precipitation_mm", "Value"], 
     clim_month_s[clim_month_s$Site=="Nuuk" & clim_month_s$Variable=="Humidity_%",  "Value"])

plot(clim_month_s[clim_month_s$Site=="Nuuk" & clim_month_s$Variable=="Precipitation_mm", "Value"], 
     clim_month_s[clim_month_s$Site=="Nuuk" & clim_month_s$Variable=="Humidity_%",  "Value"], 
     ylab="Humidity (%)", xlab='Precipitation (mm)',)


# ZACK MONTH
cor(clim_month_s[clim_month_s$Site=="Zackenberg" & clim_month_s$Variable=="Precipitation_mm", "Value"], 
               clim_month_s[clim_month_s$Site=="Zackenberg" & clim_month_s$Variable=="Humidity_%",  "Value"])

plot(clim_month_s[clim_month_s$Site=="Zackenberg" & clim_month_s$Variable=="Precipitation_mm", "Value"], 
     clim_month_s[clim_month_s$Site=="Zackenberg" & clim_month_s$Variable=="Humidity_%",  "Value"],
     ylab="Humidity (%)", xlab='Precipitation (mm)',)


# NUUK YEAR
cor(clim_year_s[clim_year_s$Site=="Nuuk" & clim_year_s$Variable=="Precipitation_mm", "Value"], 
    clim_year_s[clim_year_s$Site=="Nuuk" & clim_year_s$Variable=="Humidity_%",  "Value"])

plot(clim_year_s[clim_year_s$Site=="Nuuk" & clim_year_s$Variable=="Precipitation_mm", "Value"], 
     clim_year_s[clim_year_s$Site=="Nuuk" & clim_year_s$Variable=="Humidity_%",  "Value"], 
     ylab="Humidity (%)", xlab='Precipitation (mm)',)


# ZACK YEAR
cor(clim_year_s[clim_year_s$Site=="Zackenberg" & clim_year_s$Variable=="Precipitation_mm", "Value"], 
    clim_year_s[clim_year_s$Site=="Zackenberg" & clim_year_s$Variable=="Humidity_%",  "Value"])

plot(clim_year_s[clim_year_s$Site=="Zackenberg" & clim_year_s$Variable=="Precipitation_mm", "Value"], 
     clim_year_s[clim_year_s$Site=="Zackenberg" & clim_year_s$Variable=="Humidity_%",  "Value"],
     ylab="Humidity (%)", xlab='Precipitation (mm)',)




### CLIMATIC ANALYSIS #----------------------------------------------------------------

### TEMPERATURE 
par(mfrow=c(3,2))

# DAILY MEAN TEMPERATURE
# Nuuk DAY
mod_Nd_temp <- lm(Value ~ Year , data=clim_day_s[clim_day_s$Site=="Nuuk" & 
                                               clim_day_s$Variable=="Temperature_C", ])
summary(mod_Nd_temp)
anova(mod_Nd_temp)

plot(Value ~ Year , data=clim_day_s[clim_day_s$Site=="Nuuk" &
                                    clim_day_s$Variable=="Temperature_C", ], 
     ylab="Temperatures (°C)", pch=21, 
     ylim=round(range(clim_day_s[clim_day_s$Variable=="Temperature_C", 
                               "Value"]), 0), 
     cex=2, xaxt='n', xlab=' ', col="blue", bg="blue", cex.axis=1.5, cex.lab=1.5, 
     main=paste("Nuuk day mean temp: F=", as.character(round(anova(mod_Nd_temp)[1,4], 3)), 
                "P-value=" , as.character(round(anova(mod_Nd_temp)[1,5], 3)),
                "R2adj=" , as.character(round(summary(mod_Nd_temp)$adj.r.squared, 3))))     
abline(a=summary(mod_Nd_temp)$coefficients[1] , b=summary(mod_Nd_temp)$coefficients[2], 
       lwd=2, col="blue")
axis(side=1, at=seq(2007, 2019, by=5), cex.axis=1.5)

# Zack DAY
mod_Zd_temp <- lm(Value ~ Year , data=clim_day_s[clim_day_s$Site=="Zackenberg" & 
                                                 clim_day_s$Variable=="Temperature_C", ])
summary(mod_Zd_temp)
anova(mod_Zd_temp)

plot(Value ~ Year , data=clim_day_s[clim_day_s$Site=="Zackenberg" & 
                                    clim_day_s$Variable=="Temperature_C", ], 
     ylab=" ", pch=21, ylim=round(range(clim_day_s[clim_day_s$Variable=="Temperature_C", 
                                                 "Value"]), 0), 
     cex=2, xaxt='n', xlab='', col="red", bg="red", cex.axis=1.5, cex.lab=1.5, 
     main=paste("Zackenberg day mean temp ***: F=", as.character(round(anova(mod_Zd_temp)[1,4], 3)), 
                "P-value=" , as.character(round(anova(mod_Zd_temp)[1,5], 3)),
                "R2adj=" , as.character(round(summary(mod_Zd_temp)$adj.r.squared, 3))))  
abline(a=summary(mod_Zd_temp)$coefficients[1] , b=summary(mod_Zd_temp)$coefficients[2], 
       lwd=2, col="red")
axis(side=1, at=seq(1995, 2019, by=5), cex.axis=1.5)


# MONTHLY MEAN TEMPERATURE
# Nuuk MONTH
mod_Nm_temp <- lm(Value ~ Year , data=clim_month_s[clim_month_s$Site=="Nuuk" & 
                                                   clim_month_s$Variable=="Temperature_C", ])
summary(mod_Nm_temp)
anova(mod_Nm_temp)

plot(Value ~ Year , data=clim_month_s[clim_month_s$Site=="Nuuk" & 
                                    clim_month_s$Variable=="Temperature_C", ], 
     ylab="Temperatures (°C)", pch=21,
     ylim=round(range(clim_month_s[clim_month_s$Variable=="Temperature_C", 
                                 "Value"]), 0), 
     cex=2, xaxt='n', xlab='', col="blue", bg="blue", cex.axis=1.5, cex.lab=1.5, 
     main=paste("Nuuk month mean temp: F=" , as.character(round(anova(mod_Nm_temp)[1,4], 3)), 
                "P-value=" , as.character(round(anova(mod_Nm_temp)[1,5], 3)),
                "R2adj=" , as.character(round(summary(mod_Nm_temp)$adj.r.squared, 3)))) 
abline(a=summary(mod_Nm_temp)$coefficients[1] , b=summary(mod_Nm_temp)$coefficients[2], 
       lwd=2, col="blue")
axis(side=1, at=seq(2007, 2019, by=5), cex.axis=1.5)

# Zack MONTH
mod_Zm_temp <- lm(Value ~ Year , data=clim_month_s[clim_month_s$Site=="Zackenberg" & 
                                                 clim_month_s$Variable=="Temperature_C", ])
summary(mod_Zm_temp)
anova(mod_Zm_temp)

plot(Value ~ Year , data=clim_month_s[clim_month_s$Site=="Zackenberg" & 
                                    clim_month_s$Variable=="Temperature_C", ], 
     ylab=" ", pch=21, ylim=round(range(clim_month_s[clim_month_s$Variable=="Temperature_C", 
                                                   "Value"]), 0), 
     cex=2, xaxt='n', xlab='', col="red", bg="red", cex.axis=1.5, cex.lab=1.5, 
     main=paste("Zackenberg month mean temp: F=", as.character(round(anova(mod_Zm_temp)[1,4], 3)), 
                "P-value=" , as.character(round(anova(mod_Zm_temp)[1,5], 3)),
                "R2adj=" , as.character(round(summary(mod_Zm_temp)$adj.r.squared, 3)))) 
abline(a=summary(mod_Zm_temp)$coefficients[1] , b=summary(mod_Zm_temp)$coefficients[2], 
       lwd=2, col="red")
axis(side=1, at=seq(1995, 2019, by=5), cex.axis=1.5)


# YEARLY MEAN TEMPERATURE
# Nuuk YEAR
mod_Ny_temp <- lm(Value ~ Year , data=clim_year_s[clim_year_s$Site=="Nuuk" & 
                                                clim_year_s$Variable=="Temperature_C", ])
summary(mod_Ny_temp)
anova(mod_Ny_temp)

plot(Value ~ Year , data=clim_year_s[clim_year_s$Site=="Nuuk" & 
                                   clim_year_s$Variable=="Temperature_C", ], 
     ylab="Temperatures (°C)", pch=21, 
     ylim=round(range(clim_year_s[clim_year_s$Variable=="Temperature_C", 
                                "Value"]), 0), 
     cex=2, xaxt='n', xlab='Year', col="blue", bg="blue", cex.axis=1.5, cex.lab=1.5, 
     main=paste("Nuuk year mean temp: F=", as.character(round(anova(mod_Ny_temp)[1,4], 3)), 
                "P-value=" , as.character(round(anova(mod_Ny_temp)[1,5], 3)),
                "R2adj=" , as.character(round(summary(mod_Ny_temp)$adj.r.squared, 3)))) 
abline(a=summary(mod_Ny_temp)$coefficients[1] , b=summary(mod_Ny_temp)$coefficients[2], 
       lwd=2, col="blue")
axis(side=1, at=seq(2007, 2019, by=5), cex.axis=1.5)

# Zack YEAR
mod_Zy_temp <- lm(Value ~ Year , data=clim_year_s[clim_year_s$Site=="Zackenberg" & 
                                                clim_year_s$Variable=="Temperature_C", ])
summary(mod_Zy_temp)
anova(mod_Zy_temp)

plot(Value ~ Year , data=clim_year_s[clim_year_s$Site=="Zackenberg" & 
                                   clim_year_s$Variable=="Temperature_C", ], 
     ylab=" ", pch=21, ylim=round(range(clim_year_s[clim_year_s$Variable=="Temperature_C", 
                                                  "Value"]), 0), 
     cex=2, xaxt='n', xlab='Year', col="red", bg="red", cex.axis=1.5, cex.lab=1.5, 
     main=paste("Zackenberg year mean temp **: F=", as.character(round(anova(mod_Zy_temp)[1,4], 3)), 
                "P-value=" , as.character(round(anova(mod_Zy_temp)[1,5], 3)),
                "R2adj=" , as.character(round(summary(mod_Zy_temp)$adj.r.squared, 3)))) 
abline(a=summary(mod_Zy_temp)$coefficients[1] , b=summary(mod_Zy_temp)$coefficients[2], 
       lwd=2, col="red")
axis(side=1, at=seq(1995, 2019, by=5), cex.axis=1.5)
##END-----------------------------------------------------------------------------------






### PRECIPITATION 
par(mfrow=c(3,2))

# DAILY MEAN PRECIPITATION
# Nuuk DAY
mod_Nd_prec <- lm(Value ~ Year , data=clim_day_s[clim_day_s$Site=="Nuuk" & 
                                               clim_day_s$Variable=="Precipitation_mm", ])
summary(mod_Nd_prec)
anova(mod_Nd_prec)

plot(Value ~ Year , data=clim_day_s[clim_day_s$Site=="Nuuk" &
                                  clim_day_s$Variable=="Precipitation_mm", ], 
     ylab="Precipitation (mm)", pch=21, 
     ylim=round(range(clim_day_s[clim_day_s$Variable=="Precipitation_mm", 
                               "Value"]), 0), 
     cex=2, xaxt='n', xlab=' ', col="blue", bg="blue", cex.axis=1.5, cex.lab=1.5, 
     main=paste("Nuuk day mean prec: F=", as.character(round(anova(mod_Nd_prec)[1,4], 3)), 
                "P-value=" , as.character(round(anova(mod_Nd_prec)[1,5], 3)),
                "R2adj=" , as.character(round(summary(mod_Nd_prec)$adj.r.squared, 3))))
abline(a=summary(mod_Nd_prec)$coefficients[1] , b=summary(mod_Nd_prec)$coefficients[2], 
       lwd=2, col="blue")
axis(side=1, at=seq(2007, 2019, by=5), cex.axis=1.5)

# Zack DAY
mod_Zd_prec <- lm(Value ~ Year , data=clim_day_s[clim_day_s$Site=="Zackenberg" & 
                                                 clim_day_s$Variable=="Precipitation_mm", ])
summary(mod_Zd_prec)
anova(mod_Zd_prec)

plot(Value ~ Year , data=clim_day_s[clim_day_s$Site=="Zackenberg" & 
                                  clim_day_s$Variable=="Precipitation_mm", ], 
     ylab=" ", pch=21, 
     ylim=round(range(clim_day_s[clim_day_s$Site=="Zackenberg" & clim_day_s$Variable=="Precipitation_mm", 
                               "Value"]), 0), 
     cex=2, xaxt='n', xlab='', col="red", bg="red", cex.axis=1.5, cex.lab=1.5, 
     main=paste("Zackenberg day mean prec: F=", as.character(round(anova(mod_Zd_prec)[1,4], 3)), 
                "P-value=" , as.character(round(anova(mod_Zd_prec)[1,5], 3)),
                "R2adj=" , as.character(round(summary(mod_Zd_prec)$adj.r.squared, 3))))
abline(a=summary(mod_Zd_prec)$coefficients[1] , b=summary(mod_Zd_prec)$coefficients[2], 
       lwd=2, col="red")
axis(side=1, at=seq(1995, 2019, by=5), cex.axis=1.5)


# MONTHLY MEAN PRECIPITATION
# Nuuk MONTH
mod_Nm_prec <- lm(Value ~ Year , data=clim_month_s[clim_month_s$Site=="Nuuk" & 
                                                 clim_month_s$Variable=="Precipitation_mm", ])
summary(mod_Nm_prec)
anova(mod_Nm_prec)

plot(Value ~ Year , data=clim_month_s[clim_month_s$Site=="Nuuk" & 
                                    clim_month_s$Variable=="Precipitation_mm", ], 
     ylab="Precipitation (mm)", pch=21, 
     ylim=round(range(clim_month_s[clim_month_s$Variable=="Precipitation_mm", 
                                 "Value"]), 3),
     cex=2, xaxt='n',xlab='', col="blue", bg="blue", cex.axis=1.5, cex.lab=1.5, 
     main=paste("Nuuk month mean prec: F=", as.character(round(anova(mod_Nm_prec)[1,4], 3)), 
                "P-value=" , as.character(round(anova(mod_Nm_prec)[1,5], 3)),
                "R2adj=" , as.character(round(summary(mod_Nm_prec)$adj.r.squared, 3))))
abline(a=summary(mod_Nm_prec)$coefficients[1] , b=summary(mod_Nm_prec)$coefficients[2], 
       lwd=2, col="blue")
axis(side=1, at=seq(2007, 2019, by=5), cex.axis=1.5)

# Zack MONTH
mod_Zm_prec <- lm(Value ~ Year , data=clim_month_s[clim_month_s$Site=="Zackenberg" & 
                                                 clim_month_s$Variable=="Precipitation_mm", ])
summary(mod_Zm_prec)
anova(mod_Zm_prec)

plot(Value ~ Year , data=clim_month_s[clim_month_s$Site=="Zackenberg" & 
                                    clim_month_s$Variable=="Precipitation_mm", ], 
     ylab=" ", pch=21, 
     ylim=round(range(clim_month_s[clim_month_s$Site=="Zackenberg" & clim_month_s$Variable=="Precipitation_mm", 
                               "Value"]), 3), 
     cex=2, xaxt='n', xlab='', col="red", bg="red", cex.axis=1.5, cex.lab=1.5, 
     main=paste("Zackenberg month mean prec : F=", as.character(round(anova(mod_Zm_prec)[1,4], 3)), 
                "P-value=" , as.character(round(anova(mod_Zm_prec)[1,5], 3)),
                "R2adj=" , as.character(round(summary(mod_Zm_prec)$adj.r.squared, 3))))
abline(a=summary(mod_Zm_prec)$coefficients[1] , b=summary(mod_Zm_prec)$coefficients[2], 
       lwd=2, col="red")
axis(side=1, at=seq(1995, 2019, by=5), cex.axis=1.5)


# YEARLY MEAN PRECIPITATION
# Nuuk YEAR
mod_Ny_prec <- lm(Value ~ Year , data=clim_year_s[clim_year_s$Site=="Nuuk" & 
                                                clim_year_s$Variable=="Precipitation_mm", ])
summary(mod_Ny_prec)
anova(mod_Ny_prec)

plot(Value ~ Year , data=clim_year_s[clim_year_s$Site=="Nuuk" & 
                                   clim_year_s$Variable=="Precipitation_mm", ], 
     ylab="Precipitation (mm)", pch=21, ylim=round(range(clim_year_s[clim_year_s$Variable=="Precipitation_mm", 
                                                                   "Value"]), 3), 
     cex=2, xaxt='n', xlab='Year', col="blue", bg="blue", cex.axis=1.5, cex.lab=1.5, 
     main=paste("Nuuk year mean prec *: F=", as.character(round(anova(mod_Ny_prec)[1,4], 3)), 
                "P-value=" , as.character(round(anova(mod_Ny_prec)[1,5], 3)),
                "R2adj=" , as.character(round(summary(mod_Ny_prec)$adj.r.squared, 3))))
abline(a=summary(mod_Ny_prec)$coefficients[1] , b=summary(mod_Ny_prec)$coefficients[2], 
       lwd=2, col="blue")
axis(side=1, at=seq(2007, 2019, by=5), cex.axis=1.5)

# Zack YEAR
mod_Zy_prec <- lm(Value ~ Year , data=clim_year_s[clim_year_s$Site=="Zackenberg" & 
                                                clim_year_s$Variable=="Precipitation_mm", ])
summary(mod_Zy_prec)
anova(mod_Zy_prec)

plot(Value ~ Year , data=clim_year_s[clim_year_s$Site=="Zackenberg" & 
                                   clim_year_s$Variable=="Precipitation_mm", ], 
     ylab=" ", pch=21,  
     ylim=round(range(clim_year_s[clim_year_s$Site=="Zackenberg" & clim_year_s$Variable=="Precipitation_mm", 
                                                    "Value"]), 3),
     cex=2, xaxt='n', xlab='Year', col="red", bg="red", cex.axis=1.5, cex.lab=1.5, 
     main=paste("Zackenberg year mean prec **: F=" , as.character(round(anova(mod_Zy_prec)[1,4], 3)), 
                "P-value=" , as.character(round(anova(mod_Zy_prec)[1,5], 3)),
                "R2adj=" , as.character(round(summary(mod_Zy_prec)$adj.r.squared, 3))))
abline(a=summary(mod_Zy_prec)$coefficients[1] , b=summary(mod_Zy_prec)$coefficients[2], 
       lwd=2, col="red")
axis(side=1, at=seq(1995, 2019, by=5), cex.axis=1.5)
##END-----------------------------------------------------------------------------------






### HUMIDITY 
par(mfrow=c(3,2))

# DAILY MEAN HUMIDITY
# Nuuk DAY
mod_Nd_hum <- lm(Value ~ Year , data=clim_day_s[clim_day_s$Site=="Nuuk" & 
                                              clim_day_s$Variable=="Humidity_%", ])
summary(mod_Nd_hum)
anova(mod_Nd_hum)

plot(Value ~ Year , data=clim_day_s[clim_day_s$Site=="Nuuk" &
                                  clim_day_s$Variable=="Humidity_%", ], 
     ylab="Humidity (%)", pch=21, ylim=round(range(clim_day_s[clim_day_s$Variable=="Humidity_%", 
                                                            "Value"]),0), 
     cex=2, xaxt='n', xlab=' ', col="blue", bg="blue", cex.axis=1.5, cex.lab=1.5, 
     main=paste("Nuuk day mean hum ***: F=" , as.character(round(anova(mod_Nd_hum)[1,4], 3)), 
                "P-value=" , as.character(round(anova(mod_Nd_hum)[1,5], 3)),
                "R2adj=" , as.character(round(summary(mod_Nd_hum)$adj.r.squared, 3))))
abline(a=summary(mod_Nd_hum)$coefficients[1] , b=summary(mod_Nd_hum)$coefficients[2], 
       lwd=2, col="blue")
axis(side=1, at=seq(2007, 2019, by=5), cex.axis=1.5)

# Zack DAY
mod_Zd_hum <- lm(Value ~ Year , data=clim_day_s[clim_day_s$Site=="Zackenberg" & 
                                              clim_day_s$Variable=="Humidity_%", ])
summary(mod_Zd_hum)
anova(mod_Zd_hum)

plot(Value ~ Year , data=clim_day_s[clim_day_s$Site=="Zackenberg" & 
                                  clim_day_s$Variable=="Humidity_%", ], 
     ylab=" ", pch=21, ylim=round(range(clim_day_s[clim_day_s$Site=="Zackenberg" &
                                                 clim_day_s$Variable=="Humidity_%", 
                                                 "Value"]),0), 
     cex=2, xaxt='n', xlab='', col="red", bg="red", cex.axis=1.5, cex.lab=1.5, 
     main=paste("Zackenberg day mean hum: F=" , as.character(round(anova(mod_Zd_hum)[1,4], 3)), 
                "P-value=" , as.character(round(anova(mod_Zd_hum)[1,5], 3)),
                "R2adj=" , as.character(round(summary(mod_Zd_hum)$adj.r.squared, 3))))
abline(a=summary(mod_Zd_hum)$coefficients[1] , b=summary(mod_Zd_hum)$coefficients[2], 
       lwd=2, col="red")
axis(side=1, at=seq(1995, 2019, by=5), cex.axis=1.5)


# MONTHLY MEAN HUMIDITY
# Nuuk MONTH
mod_Nm_hum <- lm(Value ~ Year , data=clim_month_s[clim_month_s$Site=="Nuuk" & 
                                                clim_month_s$Variable=="Humidity_%", ])
summary(mod_Nm_hum)
anova(mod_Nm_hum)

plot(Value ~ Year , data=clim_month_s[clim_month_s$Site=="Nuuk" & 
                                    clim_month_s$Variable=="Humidity_%", ], 
     ylab="Humidity (%)", pch=21, ylim=round(range(clim_month_s[clim_month_s$Variable=="Humidity_%", 
                                                            "Value"]), 0),
     cex=2, xaxt='n',xlab='', col="blue", bg="blue", cex.axis=1.5, cex.lab=1.5, 
     main=paste("Nuuk month mean hum **: F=" , as.character(round(anova(mod_Nm_hum)[1,4], 3)), 
                "P-value=" , as.character(round(anova(mod_Nm_hum)[1,5], 3)),
                "R2adj=" , as.character(round(summary(mod_Nm_hum)$adj.r.squared, 3))))
abline(a=summary(mod_Nm_hum)$coefficients[1] , b=summary(mod_Nm_hum)$coefficients[2], 
       lwd=2, col="blue")
axis(side=1, at=seq(2007, 2019, by=5), cex.axis=1.5)

# Zack MONTH
mod_Zm_hum <- lm(Value ~ Year , data=clim_month_s[clim_month_s$Site=="Zackenberg" & 
                                                clim_month_s$Variable=="Humidity_%", ])
summary(mod_Zm_hum)
anova(mod_Zm_hum)

plot(Value ~ Year , data=clim_month_s[clim_month_s$Site=="Zackenberg" & 
                                    clim_month_s$Variable=="Humidity_%", ], 
     ylab=" ", pch=21, ylim=round(range(clim_month_s[clim_month_s$Site=="Zackenberg" &
                                                   clim_month_s$Variable=="Humidity_%", 
                                                   "Value"]), 0), 
     cex=2, xaxt='n', xlab='', col="red", bg="red", cex.axis=1.5, cex.lab=1.5, 
     main=paste("Zackenberg month mean hum: F=" , as.character(round(anova(mod_Zm_hum)[1,4], 3)), 
                "P-value=" , as.character(round(anova(mod_Zm_hum)[1,5], 3)),
                "R2adj=" , as.character(round(summary(mod_Zm_hum)$adj.r.squared, 3))))
abline(a=summary(mod_Zm_hum)$coefficients[1] , b=summary(mod_Zm_hum)$coefficients[2], 
       lwd=2, col="red")
axis(side=1, at=seq(1995, 2019, by=5), cex.axis=1.5)


# YEARLY MEAN HUMIDITY
# Nuuk YEAR
mod_Ny_hum <- lm(Value ~ Year , data=clim_year_s[clim_year_s$Site=="Nuuk" & 
                                               clim_year_s$Variable=="Humidity_%", ])
summary(mod_Ny_hum)
anova(mod_Ny_hum)

plot(Value ~ Year , data=clim_year_s[clim_year_s$Site=="Nuuk" & 
                                   clim_year_s$Variable=="Humidity_%", ], 
     ylab="Humidity (%)", pch=21, 
     ylim=round(range(clim_year_s[clim_year_s$Variable=="Humidity_%", 
                                "Value"]), 0), 
     cex=2, xaxt='n', xlab='Year', col="blue", bg="blue", cex.axis=1.5, cex.lab=1.5, 
     main=paste("Nuuk year mean hum ***: F=" , as.character(round(anova(mod_Ny_hum)[1,4], 3)), 
                "P-value=" , as.character(round(anova(mod_Ny_hum)[1,5], 3)),
                "R2adj=" , as.character(round(summary(mod_Ny_hum)$adj.r.squared, 3))))
abline(a=summary(mod_Ny_hum)$coefficients[1] , b=summary(mod_Ny_hum)$coefficients[2], 
       lwd=2, col="blue")
axis(side=1, at=seq(2007, 2019, by=5), cex.axis=1.5)

# Zack YEAR
mod_Zy_hum <- lm(Value ~ Year , data=clim_year_s[clim_year_s$Site=="Zackenberg" & 
                                               clim_year_s$Variable=="Humidity_%", ])
summary(mod_Zy_hum)
anova(mod_Zy_hum)

plot(Value ~ Year , data=clim_year_s[clim_year_s$Site=="Zackenberg" & 
                                   clim_year_s$Variable=="Humidity_%", ], 
     ylab=" ", pch=21, ylim=round(range(clim_year_s[clim_year_s$Site=="Zackenberg" &
                                                  clim_year_s$Variable=="Humidity_%", 
                                                  "Value"]), 0), 
     cex=2, xaxt='n', xlab='Year', col="red", bg="red", cex.axis=1.5, cex.lab=1.5, 
     main=paste("Zackenberg year mean hum **: F=" , as.character(round(anova(mod_Zy_hum)[1,4], 3)), 
                "P-value=" , as.character(round(anova(mod_Zy_hum)[1,5], 3)),
                "R2adj=" , as.character(round(summary(mod_Zy_hum)$adj.r.squared, 3))))
abline(a=summary(mod_Zy_hum)$coefficients[1] , b=summary(mod_Zy_hum)$coefficients[2], 
       lwd=2, col="red")
axis(side=1, at=seq(1995, 2019, by=5), cex.axis=1.5)
##END-----------------------------------------------------------------------------------