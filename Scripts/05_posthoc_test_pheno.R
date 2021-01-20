###############################################################################
#
#                           Greenland Flowering project
#                     05 posthoc analysis early/late phenology
#
###############################################################################
# Antoine Becker-Scarpitta
# January 2021


#  clean R work-space
rm(list=ls())


# Load Lib, and Data
source("Scripts/02_Creation_DB.R")
source("Scripts/C2_Climatic_covariates.R")
# source("Scripts/04_Analysis.R")


# Load Phenological data
Pheno_Z <- read.csv("data/datasets/Phenology_50P_open_flowers.csv", 
                    header=TRUE, sep=";", stringsAsFactors=FALSE, 
                    strip.white = T, na.strings = c("","NA"))

Pheno_N <- read.csv("data/datasets/Phenology_SIL_LOI_SAL_50P_open_flowers.csv", 
                    header=TRUE,  sep=";")















