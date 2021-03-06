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
# source("Scripts/00_Load_libraries.r")
# source("Scripts/01_Import_DB.r")
source("Scripts/02_Creation_DB.r")


# SPECIFIC DATA MANAGMENT FOR ANALYSIS -------------------------------------------
# log(density) !=0 (Flow_m2 + 0.001 in 02_Creation_DB, line 131)


fits_sp_z <- flow %>%
  group_by(Site, Species) %>%
  do(broom::glance(lm(log(trans_Flow_m2) ~ Year, data=.)))



# # NUUK
# # Erithrum
# mod_eri <- lmer(log(trans_Flow_m2) ~ Year + (1|Plot), 
#               data=droplevels(flow[flow$Site=="Nuuk" & flow$Species=="ERI",]))
# qqmath(mod_eri)
# summary(mod_eri) 
# anova(mod_eri)
# MuMIn::r.squaredGLMM(mod_eri)
# 
# 
# # Loiseuria
# mod_loi <- lmer(log(trans_Flow_m2) ~ Year + (1|Plot), 
#               data=droplevels(flow[flow$Site=="Nuuk" & flow$Species=="LOI",]))
# qqmath(mod_loi)
# summary(mod_loi) ; anova(mod_loi)
# MuMIn::r.squaredGLMM(mod_loi)
# 
# 
# # Salix
# # female flowers
# mod_sal_f <- lmer(log(trans_Flow_m2) ~ Year + (1|Plot), 
#               data=droplevels(flow[flow$Site=="Nuuk" & flow$Species=="SAL_female",]))
# qqmath(mod_sal_f)
# summary(mod_sal_f) ; anova(mod_sal_f)
# MuMIn::r.squaredGLMM(mod_sal_f)
# 
# # male flowers
# mod_sal_m <- lmer(log(trans_Flow_m2) ~ Year + (1|Plot), 
#                 data=droplevels(flow[flow$Site=="Nuuk" & flow$Species=="SAL_male",]))
# qqmath(mod_sal_m)
# summary(mod_sal_m) ; anova(mod_sal_m)
# MuMIn::r.squaredGLMM(mod_sal_m)
# 
# 
# # Silene
# mod_sil <- lmer(log(trans_Flow_m2) ~ Year + (1|Plot), 
#               data=flow[flow$Site=="Nuuk" & flow$Species=="SIL",])
# qqmath(mod_sil)
# summary(mod_sil) ; anova(mod_sil)
# MuMIn::r.squaredGLMM(mod_sil)
# 
# 
# 
# 
# 
# 
# # ZACKENBERG
# # Cassiope
# mod_Zcas <- lmer(log(trans_Flow_m2) ~ Year + (1|Plot), 
#               data=flow[flow$Site=="Zackenberg" & flow$Species=="CAS",])
# qqmath(mod_Zcas)
# summary(mod_Zcas) ; anova(mod_Zcas)
# MuMIn::r.squaredGLMM(mod_Zcas)
# 
# 
# # Dryas
# mod_Zday <- lmer(log(trans_Flow_m2) ~ Year  + (1|Plot), 
#                  data=flow[flow$Site=="Zackenberg" & flow$Species=="DRY",])
# qqmath(mod_Zday)
# summary(mod_Zday) ; anova(mod_Zday)
# MuMIn::r.squaredGLMM(mod_Zday)
# 
# 
# # Papaver
# mod_Zpap <- lmer(log(trans_Flow_m2) ~ Year + (1|Plot), 
#                  data=flow[flow$Site=="Zackenberg" & flow$Species=="PAP",])
# qqmath(mod_Zpap)
# summary(mod_Zpap) ; anova(mod_Zpap)
# MuMIn::r.squaredGLMM(mod_Zpap)
# 
# 
# # Saxifraga
# mod_Zsax <- lmer(log(trans_Flow_m2) ~ Year + (1|Plot), 
#                  data=flow[flow$Site=="Zackenberg" & flow$Species=="SAX",])
# qqmath(mod_Zsax)
# summary(mod_Zsax) ; anova(mod_Zsax)
# MuMIn::r.squaredGLMM(mod_Zsax)
# 
# 
# # Silene
# mod_Zsil <- lmer(log(trans_Flow_m2) ~ Year + (1|Plot), 
#                  data=flow[flow$Site=="Zackenberg" & flow$Species=="SIL",])
# qqmath(mod_Zsil)
# summary(mod_Zsil) ; anova(mod_Zsil)
# MuMIn::r.squaredGLMM(mod_Zsil)
# 
# 
# # Salix
# # female flowers
# mod_Zsal_f <- lmer(log(trans_Flow_m2) ~ Year + (1|Plot), 
#                   data=droplevels(flow[flow$Site=="Zackenberg" & flow$Species=="SAL_female",]))
# qqmath(mod_Zsal_f)
# summary(mod_Zsal_f) ; anova(mod_Zsal_f)
# MuMIn::r.squaredGLMM(mod_Zsal_f)
# 
# # male flowers
# mod_Zsal_m <- lmer(log(trans_Flow_m2) ~ Year + (1|Plot), 
#                   data=droplevels(flow[flow$Site=="Zackenberg" & flow$Species=="SAL_male",]))
# qqmath(mod_Zsal_m)
# summary(mod_Zsal_m) ; anova(mod_Zsal_m)
# MuMIn::r.squaredGLMM(mod_Zsal_m)


