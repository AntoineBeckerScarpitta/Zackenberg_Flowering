#######################################################################################
#
#                           Greenland Flowering project
#                                04 - Analysis
#
#######################################################################################
# Antoine Becker-Scarpitta
# September 2020

#  clean R work-space
rm(list=ls())

# # Load 02 - Creation database (+ scripts 00 and 01)
# source("Scripts/00_Load_libraries.r")
# source("Scripts/01_Import_DB.r")
source("Scripts/02_Creation_DB.r")
source("Scripts/C2_Climatic_covariates.R")


# SPECIFIC DATA MANAGMENT FOR ANALYSIS -----------------------------------------------
# log(density) !=0 (Flow_m2 + 0.001 in 02_Creation_DB, line 131)


# data  distribution exploration, flo density, log(flow density)
par(mfrow=c(2,2))

hist(flow[flow$Site=="Nuuk", "Flow_m2"], 
     main='A - Nuuk flowering density', xlab="Flower m2")
hist(flow[flow$Site=="Zackenberg", "Flow_m2"], 
     main='B - Zackenberg flowering density', xlab="Flower m2")
hist(log(flow[flow$Site=="Nuuk", "trans_Flow_m2"]), 
     main='C - Nuuk log(flow density)', xlab="log(Flower m2)")
hist(log(flow[flow$Site=="Zackenberg", "trans_Flow_m2"]), 
     main='D - Zackenberg log(flow density)', xlab="log(Flower m2)")

par(mfrow=c(1,1))





#  MODEL 1: flow(t) ~ Sp * Year + flow(t-1) + ranef(plot)
mod1 <- lmer(log(trans_Flow_m2) ~ Species * as.numeric(Year) + 
               Lag(log(trans_Flow_m2), k = 1) + (1|Plot),
             data= as.data.frame(flow %>% filter(Site=="Zackenberg")), 
             REML=T, na.action=na.omit)
summary(mod1)
#------------------------------------------------------------------------------------




#  MODEL 2: flow(t) ~ Sp * clim(summer) + 
                    # Sp * clim(fall-1) +
                    # Sp * SnowMelt(DOY) +
                    # flow(t-1) + 
                    # ranef(plot)
mod2 <- lme(

# Yt
log(trans_Flow_m2) ~

# Species : Temperature_C (june, july, august) = actual flower production condition
Species : (clim_season_year %>% filter(Site=="Zackenberg",
                            Variable=="Temperature_C",
                            Season=="summer")) +

# Species : Temperature_C (sept, oct, nov) t-1 = next year preparation (buds)
Species: (clim_season_year %>% filter(Site=="Zackenberg",
                            Variable=="Temperature_C",
                            Season=="fall")) +

# Species : DOYsnowmelt = protection + opening the season
Species: 
# Yt-1: autoregressive term to account for residual autocorrelation
# of counts as a function of time between censuses,
correlation=corAR1(0.95, form=~Year|Plot),

# Random effect
random=~1|Plot,

# DataSet
data=flow, method="REML")
#------------------------------------------------------------------------------------






# TEst the AR term in lme
summary(lmer(
  log(trans_Flow_m2) ~ Species * Year + 
  (1|Plot),
  data= flow %>% filter(Site=="Zackenberg")))



modus <-  lme(log(trans_Flow_m2) ~ Species + as.numeric(Year) , 
              data= as.data.frame(flow %>% filter(Site=="Zackenberg")), 
              method='REML',
              random=~1|Plot,
              correlation=corAR1(0.95, form=~as.factor(paste(flow$Year, flow$Plot, sep="_"))),
              na.action=na.omit)







# test the ar function
armod <- ar(flow[flow$Site=="Zackenberg", 'trans_Flow_m2'], FALSE, 1) 
predict(armod, n.ahead = 30)



# plot the autocorrelation function. 
par(mfrow=c(1,3))
acf(na.omit(flow$TotalFlower), plot = T, lag.max = 30, 
    main = "Sample Autocorrelation for Total flowering number")

acf(na.omit(flow$Flow_m2), plot = T, lag.max = 30, 
    main = "Sample Autocorrelation for Flowering density")


acf(na.omit(log(flow$trans_Flow_m2)), plot = T, lag.max = 30, 
    main = "Sample Autocorrelation for Log(Flowering density)")
par(mfrow=c(1,1))









#  mod1 - log(trans_Flow_m2) ~ YEAR + SPECIES + RANEF(plot)
# trans_Flow_m2 = Flow_m2 + 0.001
# NUUK
mod1n <- lmer(log(trans_Flow_m2) ~ Year * Species + (1|Plot), 
              data=flow[flow$Site=="Nuuk",])
qqmath(mod1n)
summary(mod1n) ; anova(mod1n)
MuMIn::r.squaredGLMM(mod1n)

# ZACK
mod1z <- lmer(log(trans_Flow_m2) ~ Year * Species + (1|Plot), 
              data=flow[flow$Site=="Zackenberg",])
qqmath(mod1z)
summary(mod1z) ; anova(mod1z)
MuMIn::r.squaredGLMM(mod1z)


# posthoc test
library(emmeans)
emmeans(mod1n, list(pairwise ~ Species), adjust = "tukey")
emmeans(mod1z, list(pairwise ~ Species), adjust = "tukey")


# # r2 marginal et conditionnels des effets fixes
r2glmm::r2beta(mod1n, method = 'nsj') # check les methodes, ?a peux faire une diff?rence...
r2glmm::r2beta(mod1z, method = 'nsj') # check les methodes, ?a peux faire une diff?rence...


# # plot les "graph criticism plots"
LMERConvenienceFunctions::mcp.fnc(mod1n)
LMERConvenienceFunctions::mcp.fnc(mod1z)
# 
# # plot rapide des effects fixes significatif sous forme de graph
plot(effects::allEffects(mod1n),multiline=T,rug=F,ci.style = "line",show.data=T)
plot(effects::allEffects(mod1z),multiline=T,rug=F,ci.style = "line",show.data=T)

#
# # plot rapide de tout les effects fixes ( la ligne verticale du 0 ?tant le "niveau 1" de chaque effet fixe)
sjPlot::plot_model(mod1n,show.values = T,vline.color = "grey",value.offset = -0.3)
sjPlot::plot_model(mod1z,show.values = T,vline.color = "grey",value.offset = -0.3)

# 
# # plot des effets fixes en d?tail 
# # ,show.data=T si tu veux voir les points
# # ,type = "eff" si tu veux voir les effets "reels" ; ,type ="pred" si tu veux voir les effets pr?dits par le mod?le 
# sjPlot::plot_model(mod, type = "eff", terms = c("var_conti1","factor1"),show.data=F)+theme_bw()
# sjPlot::plot_model(mod, type = "eff", terms = c("factor1","factor2","factor3"),show.data=F)+theme_bw()
# sjPlot::plot_model(mod, type = "eff", terms = c("var_conti2"),show.data=F)+theme_bw()

# # une bonne alternative pour faire un plot de tes effects cat?goriques
# interactions::cat_plot(mod0n, pred = "Year",,plot.points = F, interval = TRUE,int.width = 0.95)#+theme_bw()
# 
# # ensuite, c'est Inkscape qui prend le relais...
# 
# # posthoc pour tester l'interaction var_conti*factor1 (c'est un test de diff?rence de pente entre les deux facteurs)
# interaction1<-emmeans::emtrends(mod, "factor1", var = "var_conti1")
# pairs(interaction1)
# summary(interaction1)
# 
# # posthoc pour tester l'interaction triples des facteurs
# 
# tt = emmeans::lsmeans(mod, specs = ~ factor1 | factor2:factor3)
# dd = pairs(tt, reverse = TRUE)   # compare les deux niveaux du facteur 1 en fixant les facteurs 2 et 3
# summary(dd, by = NULL)
# pairs(dd, by = "factor2") # compare les deux niveaux du facteur 2 en fixant les facteurs 1 et 3
# pairs(dd, by = "factor3") # compare les deux niveaux du facteur 3 en fixant les facteurs 1 et 2


#####
