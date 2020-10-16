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
source("Scripts/02_Creation_DB.r")


# Correlation between Density and Total
cor(flow$Flow_m2 , flow$TotalFlower);
plot(flow$Flow_m2 ~ flow$TotalFlower, col=flow$Species)


# Basic LMER models
mod1 <- lmer((scale(Flow_m2, center=TRUE, scale=TRUE))~Year*Species+(1|Plot),
             data=flow)

mod2 <- glmer(scale(Flow_m2, center=TRUE, scale=TRUE) ~ Year + Species + (1|Plot),
              poisson(link = "log"), data=flow)

qqmath(mod1)
summary(mod1) ; anova(mod1)




# # R2 marginal et conditionnels
# MuMIn::r.squaredGLMM(mod)
# 
# # r2 marginal et conditionnels des effets fixes
# r2glmm::r2beta(mod, method = 'nsj') # check les methodes, ?a peux faire une diff?rence...
# 
# # plot les "graph criticism plots"
# LMERConvenienceFunctions::mcp.fnc(mod)
# 
# # plot rapide des effects fixes significatif sous forme de graph
# plot(effects::allEffects(mod),multiline=T,rug=F,ci.style = "line",show.data=T)
# 
# # plot rapide de tout les effects fixes ( la ligne verticale du 0 ?tant le "niveau 1" de chaque effet fixe)
# sjPlot::plot_model(mod,show.values = T,vline.color = "grey",value.offset = -0.3)
# 
# # plot des effets fixes en d?tail 
# # ,show.data=T si tu veux voir les points
# # ,type = "eff" si tu veux voir les effets "reels" ; ,type ="pred" si tu veux voir les effets pr?dits par le mod?le 
# sjPlot::plot_model(mod, type = "eff", terms = c("var_conti1","factor1"),show.data=F)+theme_bw()  
# sjPlot::plot_model(mod, type = "eff", terms = c("factor1","factor2","factor3"),show.data=F)+theme_bw()
# sjPlot::plot_model(mod, type = "eff", terms = c("var_conti2"),show.data=F)+theme_bw()
# 
# # une bonne alternative pour faire un plot de tes effects cat?goriques
# interactions::cat_plot(mod, pred = "factor1", modx = "factor2",mod2 = "factor3",plot.points = F, interval = TRUE,int.width = 0.95)#+theme_bw() 
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
