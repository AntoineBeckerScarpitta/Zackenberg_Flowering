#######################################################################################
#
#                           Greenland Flowering project
#                                04 - Analysis per species
#
#######################################################################################
# Antoine Becker-Scarpitta
# September 2020

#  clean R work-space
rm(list=ls())

# Load 02 - Creation database (+ scripts 00 and 01)
source("Scripts/00_Load_libraries.r")
source("Scripts/01_Import_DB.r")
source("Scripts/02_Creation_DB.r")
source("Scripts/02_Climatic_covariates.R")


# SPECIFIC DATA MANAGMENT FOR ANALYSIS -----------------------------------------------
# Create new response variable with no null value (for log transfo)
flow$Flow_m2_log <- flow$Flow_m2
flow[flow$Flow_m2==0, "Flow_m2_log"] <- 0.0001



# NUUK
# Erithrum
mod_eri <- lmer(Flow_m2 ~ Year + (1|Plot), 
              data=flow[flow$Site=="Nuuk" & flow$Species=="ERI",])
qqmath(mod_eri)
summary(mod_eri) ; anova(mod_eri)
MuMIn::r.squaredGLMM(mod_eri)

# Loiseuria
mod_loi <- lmer(Flow_m2 ~ Year + (1|Plot), 
              data=flow[flow$Site=="Nuuk" & flow$Species=="LOI",])
qqmath(mod_loi)
summary(mod_loi) ; anova(mod_loi)
MuMIn::r.squaredGLMM(mod_loi)

# Salix
mod_sal <- lmer(Flow_m2 ~ Year + (1|Plot), 
              data=flow[flow$Site=="Nuuk" & flow$Species=="SAL",])
qqmath(mod_sal)
summary(mod_sal) ; anova(mod_sal)
MuMIn::r.squaredGLMM(mod_sal)

# Silene
mod_sil <- lmer(Flow_m2 ~ Year + (1|Plot), 
              data=flow[flow$Site=="Nuuk" & flow$Species=="SIL",])
qqmath(mod_sil)
summary(mod_sil) ; anova(mod_sil)
MuMIn::r.squaredGLMM(mod_sil)






# ZACKENBERG
# Cassiope
mod_Zcas <- lmer(Flow_m2 ~ Year + (1|Plot), 
              data=flow[flow$Site=="Zackenberg" & flow$Species=="CAS",])
qqmath(mod_Zcas)
summary(mod_Zcas) ; anova(mod_Zcas)
MuMIn::r.squaredGLMM(mod_Zcas)

# Dryas
mod_Zday <- lmer(Flow_m2 ~ Year  + (1|Plot), 
                 data=flow[flow$Site=="Zackenberg" & flow$Species=="DRY",])
qqmath(mod_Zday)
summary(mod_Zday) ; anova(mod_Zday)
MuMIn::r.squaredGLMM(mod_Zday)

# Papaver
mod_Zpap <- lmer(Flow_m2 ~ Year + (1|Plot), 
                 data=flow[flow$Site=="Zackenberg" & flow$Species=="PAP",])
qqmath(mod_Zpap)
summary(mod_Zpap) ; anova(mod_Zpap)
MuMIn::r.squaredGLMM(mod_Zpap)

# Saxifraga
mod_Zsax <- lmer(Flow_m2 ~ Year + (1|Plot), 
                 data=flow[flow$Site=="Zackenberg" & flow$Species=="SAX",])
qqmath(mod_Zsax)
summary(mod_Zsax) ; anova(mod_Zsax)
MuMIn::r.squaredGLMM(mod_Zsax)

# Silene
mod_Zsil <- lmer(Flow_m2 ~ Year + (1|Plot), 
                 data=flow[flow$Site=="Zackenberg" & flow$Species=="SIL",])
qqmath(mod_Zsil)
summary(mod_Zsil) ; anova(mod_Zsil)
MuMIn::r.squaredGLMM(mod_Zsil)





