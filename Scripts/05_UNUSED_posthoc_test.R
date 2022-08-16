
#### POSTHOC TEST

# # Plot of interactions
# sjPlot::plot_model(mod_full_z_cross, type = "int")
# 
# # SPECIES * SUMMER TEMPERATURE
# sjPlot::plot_model(mod_full_z_cross, type="eff", show.data=F, 
#                    terms=c("Species", "Temp_summer")) + theme_bw()
# 
# # SPECIES * LAG FALL TEMP (previous automn temperature)
# sjPlot::plot_model(mod_full_z_cross, type="eff", show.data=F, 
#                    terms=c("Species", "lag_Temp_fall")) + theme_bw()
# 
# # SPECIES * LAG FLOW DENSITY (density dependence)
# sjPlot::plot_model(mod_full_z_cross, type="est", show.data=F, 
#                    terms=c("Species", "lag_trans_Flow_m2")) + theme_bw()
# 
# # Value per Sp and Sp paire-wise difference
# emmeans(mod_full_z_cross, list(pairwise ~ Species), adjust = "tukey")
# 
# ## R2 marginal et conditionnels des effets fixes
# ## Methods nsj = Nakagawa and Schielzeth for lmer
# r2glmm::r2beta(mod_full_z_cross, method = 'nsj', data=flow_snow_clim_z)
# 
# ## Graph of models residuals (criticism plots)
# ## graph 1= density of the model residuals
# ## graph 2= quantile-quantile plot,standardized residuals vs theor quantiles
# ## graph 3= fitted values vs the standardized residuals
# LMERConvenienceFunctions::mcp.fnc(mod_full_z_cross, trim = 2.5)
# 
# ## Plot of the fixed effects
# plot(Effect(c("Species", "Temp_summer"), mod_full_z_cross, residuals=TRUE)) 
# plot(Effect(c("Species", "lag_Temp_fall"), mod_full_z_cross, residuals=TRUE))
# plot(Effect(c("Species", "snowmelt_DOY"), mod_full_z_cross, residuals=TRUE))
# plot(Effect(c("Species", "log_lag_flow"), mod_full_z_cross, residuals=TRUE))
##END ZACK--------------------------------------------------------------------



# 3.B - Nuuk
# R2c,m
MuMIn::r.squaredGLMM(mod_basic_n)
MuMIn::r.squaredGLMM(mod_full_n_cross)

# # Plot of interactions
sjPlot::plot_model(mod_full_n_cross, type = "int")


# # Value per Sp and Sp paire-wise difference
# emmeans(mod_full_n_cross, list(pairwise ~ Species), 
#         adjust = "tukey", data=flow_snow_clim_n)
# 
# ## R2 marginal et conditionnels des effets fixes
# ## Methods nsj = Nakagawa and Schielzeth for lmer
# r2glmm::r2beta(mod_full_n_cross, method = 'nsj', data=flow_snow_clim_n)
# 
# ## Graph of models residuals (criticism plots)
# ## graph 1= density of the model residuals
# ## graph 2= quantile-quantile plot, standardized residuals vs theore quantiles
# ## graph 3= fitted values vs the standardized residuals
# LMERConvenienceFunctions::mcp.fnc(mod_full_n_cross, trim = 2.5)
# 
# ## Plot of the fixed effects
# plot(Effect(c("Species", "Temp_summer"), mod_full_n_cross, residuals=TRUE)) 
# plot(Effect(c("Species", "lag_Temp_fall"), mod_full_n_cross, residuals=TRUE))
# plot(Effect(c("Species", "snowmelt_DOY"), mod_full_n_cross, residuals=TRUE))
# plot(Effect(c("Species", "lag_trans_Flow_m2"), mod_full_n_cross, residuals=TRUE))
# 
# # SPECIES
# sjPlot::plot_model(mod_full_n_cross, type="eff", 
#                    terms=c("Species"), show.data=T) + theme_bw()
# 
# # SPECIES * SUMMER TEMPERATURE
# sjPlot::plot_model(mod_full_n_cross, type="eff", show.data=F, 
#                    terms=c("Species", "Temp_summer")) + theme_bw()
# 
# # SPECIES * LAG FALL TEMP (previous automn temperature)
# sjPlot::plot_model(mod_full_n_cross, type="eff", show.data=F, 
#                    terms=c("Species", "lag_Temp_fall")) + theme_bw()
# 
# # SPECIES * LAG FLOW DENSITY (density dependence)
# sjPlot::plot_model(mod_full_n_cross, type="eff", show.data=F, 
#                    terms=c("Species", "lag_trans_Flow_m2")) + theme_bw()
##-----------------------------------------------------------------------------------
