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


### BASIC CLIMATIC EXPLORATION #----------------------------------------------------
# distribution of temperature along year
par(mfrow=c(1,2))
# NUUK TEMPERATURE
plot(clim[clim$Site=="Nuuk" & clim$Variable=="Temperature_C" , "Month"], 
     clim[clim$Site=="Nuuk" & clim$Variable=="Temperature_C" , "Value"], 
     ylab="Temperatures (°C)", xlab="Month", 
     ylim=range(clim[clim$Variable=="Temperature_C" , "Value"]), 
     main="Nuuk, distribution of temperature")
abline(0, 0, col="grey")
abline(v=c('05', "09"), col="red")
abline(v=c('04', "10"), col="orange")
# summer at Nuuk==05, 06, 07, 08, 09
# spring at Nuuk==04 ; fall=10

# ZACKENBERG TEMPERATURE
plot(clim[clim$Site=="Zackenberg" & clim$Variable=="Temperature_C" , "Month"], 
     clim[clim$Site=="Zackenberg" & clim$Variable=="Temperature_C" , "Value"], 
     xlab="Month", 
     ylim=range(clim[clim$Variable=="Temperature_C" , "Value"]), 
     main="Zackenberg, distribution of temperature")
abline(0, 0, col="grey")
abline(v=c('06', "08"), col="red")
abline(v=c('05', "09"), col="orange")
# summer at Zack==06, 07, 08
# spring at Nuuk==05 ; fall=09



### CLIMATIC ANALYSIS #----------------------------------------------------------------
### TEMPERATURE 
par(mfrow=c(3,2))

# YEARLY MEAN TEMPERATURE
# Nuuk YEAR
mod_Ny_temp <- lm(Value ~ Year , data=clim_year[clim_year$Site=="Nuuk" & 
                                                        clim_year$Variable=="Temperature_C", ])
summary(mod_Ny_temp)
anova(mod_Ny_temp)

plot(Value ~ Year , data=clim_year[clim_year$Site=="Nuuk" & 
                                           clim_year$Variable=="Temperature_C", ], 
     ylab="Temperatures (°C)", pch=21, 
     ylim=round(range(clim_year[clim_year$Variable=="Temperature_C", 
                                "Value"]), 0), 
     cex=2, xaxt='n', xlab='Year', col="blue", bg="blue", cex.axis=1.5, cex.lab=1.5, 
     main=paste("Nuuk year mean temp: F=", as.character(round(anova(mod_Ny_temp)[1,4], 3)), 
                "P-value=" , as.character(round(anova(mod_Ny_temp)[1,5], 3)),
                "R2adj=" , as.character(round(summary(mod_Ny_temp)$adj.r.squared, 3)))) 
abline(a=summary(mod_Ny_temp)$coefficients[1] , b=summary(mod_Ny_temp)$coefficients[2], 
       lwd=2, col="blue")
axis(side=1, at=seq(2007, 2019, by=5), cex.axis=1.5)

# Zack YEAR
mod_Zy_temp <- lm(Value ~ Year , data=clim_year[clim_year$Site=="Zackenberg" & 
                                                        clim_year$Variable=="Temperature_C", ])
summary(mod_Zy_temp)
anova(mod_Zy_temp)

plot(Value ~ Year , data=clim_year[clim_year$Site=="Zackenberg" & 
                                           clim_year$Variable=="Temperature_C", ], 
     ylab=" ", pch=21, ylim=round(range(clim_year[clim_year$Variable=="Temperature_C", 
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
# YEARLY MEAN PRECIPITATION
# Nuuk YEAR
mod_Ny_prec <- lm(Value ~ Year , data=clim_year[clim_year$Site=="Nuuk" & 
                                                        clim_year$Variable=="Precipitation_mm", ])
summary(mod_Ny_prec)
anova(mod_Ny_prec)

plot(Value ~ Year , data=clim_year[clim_year$Site=="Nuuk" & 
                                           clim_year$Variable=="Precipitation_mm", ], 
     ylab="Precipitation (mm)", pch=21, ylim=round(range(clim_year[clim_year$Variable=="Precipitation_mm", 
                                                                   "Value"]), 3), 
     cex=2, xaxt='n', xlab='Year', col="blue", bg="blue", cex.axis=1.5, cex.lab=1.5, 
     main=paste("Nuuk year mean prec *: F=", as.character(round(anova(mod_Ny_prec)[1,4], 3)), 
                "P-value=" , as.character(round(anova(mod_Ny_prec)[1,5], 3)),
                "R2adj=" , as.character(round(summary(mod_Ny_prec)$adj.r.squared, 3))))
abline(a=summary(mod_Ny_prec)$coefficients[1] , b=summary(mod_Ny_prec)$coefficients[2], 
       lwd=2, col="blue")
axis(side=1, at=seq(2007, 2019, by=5), cex.axis=1.5)

# Zack YEAR
mod_Zy_prec <- lm(Value ~ Year , data=clim_year[clim_year$Site=="Zackenberg" & 
                                                        clim_year$Variable=="Precipitation_mm", ])
summary(mod_Zy_prec)
anova(mod_Zy_prec)

plot(Value ~ Year , data=clim_year[clim_year$Site=="Zackenberg" & 
                                           clim_year$Variable=="Precipitation_mm", ], 
     ylab=" ", pch=21,  
     ylim=round(range(clim_year[clim_year$Site=="Zackenberg" & clim_year$Variable=="Precipitation_mm", 
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
# YEARLY MEAN HUMIDITY
# Nuuk YEAR
mod_Ny_hum <- lm(Value ~ Year , data=clim_year[clim_year$Site=="Nuuk" & 
                                                       clim_year$Variable=="Humidity_%", ])
summary(mod_Ny_hum)
anova(mod_Ny_hum)

plot(Value ~ Year , data=clim_year[clim_year$Site=="Nuuk" & 
                                           clim_year$Variable=="Humidity_%", ], 
     ylab="Humidity (%)", pch=21, 
     ylim=round(range(clim_year[clim_year$Variable=="Humidity_%", 
                                "Value"]), 0), 
     cex=2, xaxt='n', xlab='Year', col="blue", bg="blue", cex.axis=1.5, cex.lab=1.5, 
     main=paste("Nuuk year mean hum ***: F=" , as.character(round(anova(mod_Ny_hum)[1,4], 3)), 
                "P-value=" , as.character(round(anova(mod_Ny_hum)[1,5], 3)),
                "R2adj=" , as.character(round(summary(mod_Ny_hum)$adj.r.squared, 3))))
abline(a=summary(mod_Ny_hum)$coefficients[1] , b=summary(mod_Ny_hum)$coefficients[2], 
       lwd=2, col="blue")
axis(side=1, at=seq(2007, 2019, by=5), cex.axis=1.5)

# Zack YEAR
mod_Zy_hum <- lm(Value ~ Year , data=clim_year[clim_year$Site=="Zackenberg" & 
                                                       clim_year$Variable=="Humidity_%", ])
summary(mod_Zy_hum)
anova(mod_Zy_hum)

plot(Value ~ Year , data=clim_year[clim_year$Site=="Zackenberg" & 
                                           clim_year$Variable=="Humidity_%", ], 
     ylab=" ", pch=21, ylim=round(range(clim_year[clim_year$Site=="Zackenberg" &
                                                          clim_year$Variable=="Humidity_%", 
                                                  "Value"]), 0), 
     cex=2, xaxt='n', xlab='Year', col="red", bg="red", cex.axis=1.5, cex.lab=1.5, 
     main=paste("Zackenberg year mean hum **: F=" , as.character(round(anova(mod_Zy_hum)[1,4], 3)), 
                "P-value=" , as.character(round(anova(mod_Zy_hum)[1,5], 3)),
                "R2adj=" , as.character(round(summary(mod_Zy_hum)$adj.r.squared, 3))))
abline(a=summary(mod_Zy_hum)$coefficients[1] , b=summary(mod_Zy_hum)$coefficients[2], 
       lwd=2, col="red")
axis(side=1, at=seq(1995, 2019, by=5), cex.axis=1.5)
# END -------------------------------------------------------------------------------

