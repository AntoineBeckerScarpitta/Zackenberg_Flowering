#######################################################################################
#
#                           Greenland Flowering project
#                            05 - Models Exploration
#
#######################################################################################
# Antoine Becker-Scarpitta & Laura Antao
# Febuary 2021

#  clean R work-space
rm(list=ls())

source("Scripts/00_Load_libraries.r")

# In this script you have no data, ONLY models outputs
# script to run models are: 04_Analysis_Zackenberg & 04_Analysis_Nuuk
# in these scripts (04_), you will find mod and plot effects


# 1 - MODELS STRUCTURE FOR A-ZACKENBERG ; B-NUUK
# A - ZACKENBERG
mod_basic_z <- readRDS('results/Models/mod_basic_z.rds')
mod_full_z_cross <- readRDS('results/Models/mod_full_z_cross')
mod_full_z_nest <- readRDS('results/Models/mod_full_z_nest')
mod_bw_sel_z <- readRDS('results/Models/mod_bw_sel_z')

# B - NUUK
mod_basic_n <- readRDS('results/Models/mod_basic_n.rds')
mod_full_n <- readRDS('results/Models/mod_full_n.rds')
mod_full_2_n <- readRDS('results/Models/mod_full_ranef_plot_year_n.rds')




# 2 - MODELS TABS
# Tab basic mod Zack+Nuuk
tab_model(mod_basic_z, mod_basic_n,
          p.val = "kr", 
          show.df = TRUE, 
          dv.labels = c("Basic Zack", "Basic Nuuk"))

# Tab full cross mod Zack+Nuuk
tab_model(mod_full_z_cross, mod_full_n_cross,
          p.val = "kr", 
          show.df = TRUE, 
          dv.labels = c("Full cross mod Zack", "Full cross mod Nuuk"))
          
          
          
          