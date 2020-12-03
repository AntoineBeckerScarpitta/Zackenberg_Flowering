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

# Load 02 - Creation database (+ scripts 00 and 01)
source("Scripts/00_Load_libraries.r")
source("Scripts/01_Import_DB.r")
source("Scripts/02_Creation_DB.r")
source("Scripts/02_Climatic_covariates.R")


# SPECIFIC DATA MANAGMENT FOR ANALYSIS -----------------------------------------------
# Create new response variable with no null value (for log transfo)
flow$Flow_m2_log <- flow$Flow_m2
flow[flow$Flow_m2==0, "Flow_m2_log"] <- 0.0001


# Correlation between Density and Total
cor(flow$Flow_m2 , flow$TotalFlower);
plot(flow$Flow_m2 ~ flow$TotalFlower, col=flow$Species)

# basic explo
hist(flow$Flow_m2)
hist(log(flow$Flow_m2))
hist(log(flow$Flow_m2_log))

par(mfrow=c(2,2))
hist(flow[flow$Site=="Nuuk", "Flow_m2"], main='Nuuk raw', xlab="Flower m2")
hist(flow[flow$Site=="Zackenberg", "Flow_m2"], main='Zackenberg raw', xlab="Flower m2")
hist(log(flow[flow$Site=="Nuuk", "Flow_m2"]), main='Nuuk log', xlab="log(Flower m2)")
hist(log(flow[flow$Site=="Zackenberg", "Flow_m2"]), main='Zackenberg log', xlab="log(Flower m2)")
par(mfrow=c(1,1))

#  mod0 - Flow_m2 ~ YEAR + SPECIES + RANEF(plot)
mod0n <- lmer(Flow_m2 ~ Year * Species + (1|Plot), data=flow[flow$Site=="Nuuk",])
qqmath(mod0n)
summary(mod0n) ; anova(mod0n)
MuMIn::r.squaredGLMM(mod0n)


mod0z <- lmer(Flow_m2 ~ Year * Species + (1|Plot), data=flow[flow$Site=="Zackenberg",])
qqmath(mod0z)
summary(mod0z) ; anova(mod0z)
MuMIn::r.squaredGLMM(modoz)


library(emmeans)
emmeans(mod0n, list(pairwise ~ Species), adjust = "tukey")
emmeans(mod0z, list(pairwise ~ Species), adjust = "tukey")


#  mod1 - log(Flow_m2() ~ YEAR + SPECIES + RANEF(plot)
mod1n <- lmer(log(Flow_m2_log) ~ Year * Species + (1|Plot), data=flow[flow$Site=="Nuuk",])
qqmath(mod1n)
summary(mod1n) ; anova(mod1n)
MuMIn::r.squaredGLMM(mod1n)


mod1z <- lmer(log(Flow_m2_log) ~ Year * Species + (1|Plot), 
              data=flow[flow$Site=="Zackenberg",])
qqmath(mod1z)
summary(mod1z) ; anova(mod1z)
MuMIn::r.squaredGLMM(mod1z)




# # R2 marginal et conditionnels
MuMIn::r.squaredGLMM(mod0n)
# MuMIn::r.squaredGLMM(mod1n)

# MuMIn::r.squaredGLMM(mod0z)
MuMIn::r.squaredGLMM(mod1z)

# 
# # r2 marginal et conditionnels des effets fixes
r2glmm::r2beta(mod0n, method = 'nsj') # check les methodes, ?a peux faire une diff?rence...
r2glmm::r2beta(mod1z, method = 'nsj') # check les methodes, ?a peux faire une diff?rence...


# # plot les "graph criticism plots"
LMERConvenienceFunctions::mcp.fnc(mod0n)
LMERConvenienceFunctions::mcp.fnc(mod1z)
# 
# # plot rapide des effects fixes significatif sous forme de graph
plot(effects::allEffects(mod0n),multiline=T,rug=F,ci.style = "line",show.data=T)
plot(effects::allEffects(mod1z),multiline=T,rug=F,ci.style = "line",show.data=T)

#
# # plot rapide de tout les effects fixes ( la ligne verticale du 0 ?tant le "niveau 1" de chaque effet fixe)
sjPlot::plot_model(mod0n,show.values = T,vline.color = "grey",value.offset = -0.3)
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
