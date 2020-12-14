#######################################################################################
#
#                           Greenland Flowering project
#                       C3 - Analysis Climatic covariates
#
#######################################################################################
# Antoine Becker-Scarpitta
# November 2020

#  clean R work-space
rm(list=ls())

# Load 02 - Creation database (+ scripts 00 and 01)
source("Scripts/00_Load_libraries.r")
source("Scripts/C2_Climatic_covariates.R")



### CLIMATIC ANALYSIS #----------------------------------------------------------------
### TEMPERATURE 
par(mfrow=c(3,2))

# YEARLY MEAN TEMPERATURE
# Nuuk YEAR
mod_Ny_temp <- lm(Value ~ Year , data=clim_season_year[clim_season_year$Site=="Nuuk" & 
                                                    clim_season_year$Variable=="Temperature_C"&
                                                    clim_season_year$Season=="summer", ])
summary(mod_Ny_temp)
anova(mod_Ny_temp)

plot(Value ~ Year , data=clim_season_year[clim_season_year$Site=="Nuuk" & 
                                       clim_season_year$Variable=="Temperature_C"&
                                           clim_season_year$Season=="summer", ], 
     ylab="Temperatures (°C)", pch=21, 
     ylim=round(range(clim_season_year[clim_season_year$Variable=="Temperature_C"&
                                           clim_season_year$Season=="summer", 
                                "Value"]), 0), 
     cex=2, xaxt='n', xlab='Year', col="blue", bg="blue", cex.axis=1.5, cex.lab=1.5, 
     main=paste("Nuuk year mean temp: F=", as.character(round(anova(mod_Ny_temp)[1,4], 3)), 
                "P-value=" , as.character(round(anova(mod_Ny_temp)[1,5], 3)),
                "R2adj=" , as.character(round(summary(mod_Ny_temp)$adj.r.squared, 3)))) 
abline(a=summary(mod_Ny_temp)$coefficients[1] , b=summary(mod_Ny_temp)$coefficients[2], 
       lwd=2, col="blue")
axis(side=1, at=seq(2007, 2019, by=5), cex.axis=1.5)

# Zack YEAR
mod_Zy_temp <- lm(Value ~ Year , data=clim_season_year[clim_season_year$Site=="Zackenberg" & 
                                                    clim_season_year$Variable=="Temperature_C"&
                                                        clim_season_year$Season=="summer", ])
summary(mod_Zy_temp)
anova(mod_Zy_temp)

plot(Value ~ Year , data=clim_season_year[clim_season_year$Site=="Zackenberg" & 
                                       clim_season_year$Variable=="Temperature_C"&
                                           clim_season_year$Season=="summer", ], 
     ylab=" ", pch=21, ylim=round(range(clim_season_year[clim_season_year$Variable=="Temperature_C"&
                                                             clim_season_year$Season=="summer", 
                                                  "Value"]), 0), 
     cex=2, xaxt='n', xlab='Year', col="red", bg="red", cex.axis=1.5, cex.lab=1.5, 
     main=paste("Zackenberg year mean temp *: F=", as.character(round(anova(mod_Zy_temp)[1,4], 3)), 
                "P-value=" , as.character(round(anova(mod_Zy_temp)[1,5], 3)),
                "R2adj=" , as.character(round(summary(mod_Zy_temp)$adj.r.squared, 3)))) 
abline(a=summary(mod_Zy_temp)$coefficients[1] , b=summary(mod_Zy_temp)$coefficients[2], 
       lwd=2, col="red")
axis(side=1, at=seq(1995, 2019, by=5), cex.axis=1.5)
##END-----------------------------------------------------------------------------------






### PRECIPITATION 
# YEARLY MEAN PRECIPITATION
# Nuuk YEAR
mod_Ny_prec <- lm(Value ~ Year , data=clim_season_year[clim_season_year$Site=="Nuuk" & 
                                                    clim_season_year$Variable=="Precipitation_mm"&
                                                        clim_season_year$Season=="summer", ])
summary(mod_Ny_prec)
anova(mod_Ny_prec)

plot(Value ~ Year , data=clim_season_year[clim_season_year$Site=="Nuuk" & 
                                       clim_season_year$Variable=="Precipitation_mm"&
                                           clim_season_year$Season=="summer", ], 
     ylab="Precipitation (mm)", pch=21, 
     ylim=round(range(clim_season_year[clim_season_year$Variable=="Precipitation_mm"&
                                       clim_season_year$Season=="summer","Value"]), 3), 
     cex=2, xaxt='n', xlab='Year', col="blue", bg="blue", cex.axis=1.5, cex.lab=1.5, 
     main=paste("Nuuk year mean prec: F=", as.character(round(anova(mod_Ny_prec)[1,4], 3)), 
                "P-value=" , as.character(round(anova(mod_Ny_prec)[1,5], 3)),
                "R2adj=" , as.character(round(summary(mod_Ny_prec)$adj.r.squared, 3))))
abline(a=summary(mod_Ny_prec)$coefficients[1] , b=summary(mod_Ny_prec)$coefficients[2], 
       lwd=2, col="blue")
axis(side=1, at=seq(2007, 2019, by=5), cex.axis=1.5)

# Zack YEAR
mod_Zy_prec <- lm(Value ~ Year , data=clim_season_year[clim_season_year$Site=="Zackenberg" & 
                                                    clim_season_year$Variable=="Precipitation_mm"&
                                                        clim_season_year$Season=="summer", ])
summary(mod_Zy_prec)
anova(mod_Zy_prec)

plot(Value ~ Year , data=clim_season_year[clim_season_year$Site=="Zackenberg" & 
                                       clim_season_year$Variable=="Precipitation_mm"&
                                           clim_season_year$Season=="summer", ], 
     ylab=" ", pch=21,  
     ylim=round(range(clim_season_year[clim_season_year$Site=="Zackenberg" & 
                                           clim_season_year$Variable=="Precipitation_mm"&
                                           clim_season_year$Season=="summer", "Value"]), 3),
     cex=2, xaxt='n', xlab='Year', col="red", bg="red", cex.axis=1.5, cex.lab=1.5, 
     main=paste("Zackenberg year mean prec: F=" , as.character(round(anova(mod_Zy_prec)[1,4], 3)), 
                "P-value=" , as.character(round(anova(mod_Zy_prec)[1,5], 3)),
                "R2adj=" , as.character(round(summary(mod_Zy_prec)$adj.r.squared, 3))))
abline(a=summary(mod_Zy_prec)$coefficients[1] , b=summary(mod_Zy_prec)$coefficients[2], 
       lwd=2, col="red")
axis(side=1, at=seq(1995, 2019, by=5), cex.axis=1.5)
##END-----------------------------------------------------------------------------------






### HUMIDITY 
# YEARLY MEAN HUMIDITY
# Nuuk YEAR
mod_Ny_hum <- lm(Value ~ Year , data=clim_season_year[clim_season_year$Site=="Nuuk" & 
                                                   clim_season_year$Variable=="Humidity_%"&
                                                       clim_season_year$Season=="summer", ])
summary(mod_Ny_hum)
anova(mod_Ny_hum)

plot(Value ~ Year , data=clim_season_year[clim_season_year$Site=="Nuuk" & 
                                       clim_season_year$Variable=="Humidity_%"&
                                           clim_season_year$Season=="summer", ], 
     ylab="Humidity (%)", pch=21, 
     ylim=round(range(clim_season_year[clim_season_year$Variable=="Humidity_%"&
                                           clim_season_year$Season=="summer","Value"]), 0), 
     cex=2, xaxt='n', xlab='Year', col="blue", bg="blue", cex.axis=1.5, cex.lab=1.5, 
     main=paste("Nuuk year mean hum *: F=" , as.character(round(anova(mod_Ny_hum)[1,4], 3)), 
                "P-value=" , as.character(round(anova(mod_Ny_hum)[1,5], 3)),
                "R2adj=" , as.character(round(summary(mod_Ny_hum)$adj.r.squared, 3))))
abline(a=summary(mod_Ny_hum)$coefficients[1] , b=summary(mod_Ny_hum)$coefficients[2], 
       lwd=2, col="blue")
axis(side=1, at=seq(2007, 2019, by=5), cex.axis=1.5)

# Zack YEAR
mod_Zy_hum <- lm(Value ~ Year , data=clim_season_year[clim_season_year$Site=="Zackenberg" & 
                                                   clim_season_year$Variable=="Humidity_%"&
                                                       clim_season_year$Season=="summer", ])
summary(mod_Zy_hum)
anova(mod_Zy_hum)

plot(Value ~ Year , data=clim_season_year[clim_season_year$Site=="Zackenberg" & 
                                       clim_season_year$Variable=="Humidity_%"&
                                           clim_season_year$Season=="summer", ], 
     ylab=" ", pch=21, 
     ylim=round(range(clim_season_year[clim_season_year$Site=="Zackenberg" &
                                       clim_season_year$Variable=="Humidity_%"&
                                       clim_season_year$Season=="summer","Value"]), 0), 
     cex=2, xaxt='n', xlab='Year', col="red", bg="red", cex.axis=1.5, cex.lab=1.5, 
     main=paste("Zackenberg year mean hum: F=" , as.character(round(anova(mod_Zy_hum)[1,4], 3)), 
                "P-value=" , as.character(round(anova(mod_Zy_hum)[1,5], 3)),
                "R2adj=" , as.character(round(summary(mod_Zy_hum)$adj.r.squared, 3))))
abline(a=summary(mod_Zy_hum)$coefficients[1] , b=summary(mod_Zy_hum)$coefficients[2], 
       lwd=2, col="red")
axis(side=1, at=seq(1995, 2019, by=5), cex.axis=1.5)
# END -------------------------------------------------------------------------------




