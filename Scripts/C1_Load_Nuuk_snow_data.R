#######################################################################################
#
#                           Greenland Flowering project
#                         C2 - LOAD SNOW COVARIATES NUUK
#
#######################################################################################
# Antoine Becker-Scarpitta
# Agust 2021 

# clean R work-space
# rm(list=ls())
source('scripts/00_Load_libraries.R')
library(lubridate)  ##for dealing with dates

# 1 - NUUK -------------------------------------------------------------
# READ - Salix
Nsal0 <- read.csv("data/datasets/snow_cover_nuuk/View_BioBasis_Nuuk_Data_Vegetation_Salix_phenology_and_total_count180520202134518138.csv",
                  stringsAsFactors=FALSE, header=TRUE,  sep="\t", strip.white = T,na.strings = c("","NA"))
Nsal <- Nsal0 %>% 
  dplyr::select(Date, Plot, Section, Snow) %>%
  mutate(Site="Nuuk",
         Species="SAL") 


# READ - Silene
Nsil0 <- read.csv("data/datasets/snow_cover_nuuk/View_BioBasis_Nuuk_Data_Vegetation_Silene_phenology_and_total_count180520202134090595.csv",
                  stringsAsFactors=FALSE, header=TRUE,  sep="\t", strip.white = T,na.strings = c("","NA"))
Nsil <- Nsil0 %>% 
  dplyr::select(Date, Plot, Section, Snow) %>%
  mutate(Site="Nuuk",
         Species="SIL")


# READ - Loiseleuria (Kalmia)
NLoi0 <- read.csv("data/datasets/snow_cover_nuuk/View_BioBasis_Nuuk_Data_Vegetation_Loiseleuria_phenology_and_total_count170920201306510146.csv",
                  stringsAsFactors=FALSE, header=TRUE,  sep="\t", strip.white = T,na.strings = c("","NA"))
NLoi <- NLoi0 %>% 
  dplyr::select(Date, Plot, Section, Snow) %>%
  mutate(Site="Nuuk",
         Species="LOI")


# READ - Eriophorum
NEri0 <- read.csv("data/datasets/snow_cover_nuuk/View_BioBasis_Nuuk_Data_Vegetation_Eriophorum_total_count170920201325254357.csv",
                  stringsAsFactors=FALSE, header=TRUE,  sep="\t", strip.white = T,na.strings = c("","NA"))
NEri <- NEri0 %>% 
  dplyr::select(Date, Plot, Section, Snow) %>%
  mutate(Site="Nuuk",
         Species="ERI")


# bind all dataset
snow_nuuk <- rbind(NEri, NLoi, Nsal, Nsil)
remove("NEri0", "NLoi0", "Nsal0", "Nsil0")


# rename plots
snow_nuuk[snow_nuuk$Plot=="SAL1", 'Plot'] <- "N_Sal1"
snow_nuuk[snow_nuuk$Plot=="SAL2", 'Plot'] <- "N_Sal2"
snow_nuuk[snow_nuuk$Plot=="SAL3", 'Plot'] <- "N_Sal3"
snow_nuuk[snow_nuuk$Plot=="SAL4", 'Plot'] <- "N_Sal4"

snow_nuuk[snow_nuuk$Plot=="SIL1", 'Plot'] <- "N_Sil1"
snow_nuuk[snow_nuuk$Plot=="SIL2", 'Plot'] <- "N_Sil2"
snow_nuuk[snow_nuuk$Plot=="SIL3", 'Plot'] <- "N_Sil3"
snow_nuuk[snow_nuuk$Plot=="SIL4", 'Plot'] <- "N_Sil4"

snow_nuuk[snow_nuuk$Plot=="LOI1", 'Plot'] <- "N_Loi1"
snow_nuuk[snow_nuuk$Plot=="lOI1", 'Plot'] <- "N_Loi1"  # correct typo
snow_nuuk[snow_nuuk$Plot=="LOI2", 'Plot'] <- "N_Loi2"
snow_nuuk[snow_nuuk$Plot=="LOI3", 'Plot'] <- "N_Loi3"
snow_nuuk[snow_nuuk$Plot=="LOI4", 'Plot'] <- "N_Loi4"

snow_nuuk[snow_nuuk$Plot=="ERI1", 'Plot'] <- "N_Eri1"
snow_nuuk[snow_nuuk$Plot=="Eri1", 'Plot'] <- "N_Eri1" # typo mistake
snow_nuuk[snow_nuuk$Plot=="ERI2", 'Plot'] <- "N_Eri2"
snow_nuuk[snow_nuuk$Plot=="Eri2", 'Plot'] <- "N_Eri2"
snow_nuuk[snow_nuuk$Plot=="ERI3", 'Plot'] <- "N_Eri3"
snow_nuuk[snow_nuuk$Plot=="Eri3", 'Plot'] <- "N_Eri3"
snow_nuuk[snow_nuuk$Plot=="ERI4", 'Plot'] <- "N_Eri4"
snow_nuuk[snow_nuuk$Plot=="Eri4", 'Plot'] <- "N_Eri4"


# Remove NA
snow_nuuk[snow_nuuk$Snow==-9999, 'Snow'] <-  NA
snow_n <- snow_nuuk[complete.cases(snow_nuuk),]
