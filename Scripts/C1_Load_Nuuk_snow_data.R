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
         #Species="SAL", 
         Variable="Snow_cover")


# READ - Silene
Nsil0 <- read.csv("data/datasets/snow_cover_nuuk/View_BioBasis_Nuuk_Data_Vegetation_Silene_phenology_and_total_count180520202134090595.csv",
                  stringsAsFactors=FALSE, header=TRUE,  sep="\t", strip.white = T,na.strings = c("","NA"))
Nsil <- Nsil0 %>% 
  dplyr::select(Date, Plot, Section, Snow) %>%
  mutate(Site="Nuuk",
         #Species="SIL", 
         Variable="Snow_cover")


# READ - Loiseleuria (Kalmia)
NLoi0 <- read.csv("data/datasets/snow_cover_nuuk/View_BioBasis_Nuuk_Data_Vegetation_Loiseleuria_phenology_and_total_count170920201306510146.csv",
                  stringsAsFactors=FALSE, header=TRUE,  sep="\t", strip.white = T,na.strings = c("","NA"))
NLoi <- NLoi0 %>% 
  dplyr::select(Date, Plot, Section, Snow) %>%
  mutate(Site="Nuuk",
         #Species="LOI", 
         Variable="Snow_cover")


# READ - Eriophorum
NEri0 <- read.csv("data/datasets/snow_cover_nuuk/View_BioBasis_Nuuk_Data_Vegetation_Eriophorum_total_count170920201325254357.csv",
                  stringsAsFactors=FALSE, header=TRUE,  sep="\t", strip.white = T,na.strings = c("","NA"))
NEri <- NEri0 %>% 
  dplyr::select(Date, Plot, Section, Snow) %>%
  mutate(Site="Nuuk",
         #Species="ERI", 
         Variable="Snow_cover")


# bind all dataset
Nsnow <- rbind(NEri, NLoi, Nsal, Nsil)


# rename plots
Nsnow[Nsnow$Plot=="SAL1", 'Plot'] <- "N_Sal1"
Nsnow[Nsnow$Plot=="SAL2", 'Plot'] <- "N_Sal2"
Nsnow[Nsnow$Plot=="SAL3", 'Plot'] <- "N_Sal3"
Nsnow[Nsnow$Plot=="SAL4", 'Plot'] <- "N_Sal4"

Nsnow[Nsnow$Plot=="SIL1", 'Plot'] <- "N_Sil1"
Nsnow[Nsnow$Plot=="SIL2", 'Plot'] <- "N_Sil2"
Nsnow[Nsnow$Plot=="SIL3", 'Plot'] <- "N_Sil3"
Nsnow[Nsnow$Plot=="SIL4", 'Plot'] <- "N_Sil4"

Nsnow[Nsnow$Plot=="lOI1", 'Plot'] <- "N_Loi1"  # correct typo
Nsnow[Nsnow$Plot=="LOI1", 'Plot'] <- "N_Loi1" 
Nsnow[Nsnow$Plot=="LOI2", 'Plot'] <- "N_Loi2"  
Nsnow[Nsnow$Plot=="LOI3", 'Plot'] <- "N_Loi3"  
Nsnow[Nsnow$Plot=="LOI4", 'Plot'] <- "N_Loi4"

Nsnow[Nsnow$Plot=="Eri1", 'Plot'] <- "N_Eri1" # typo mistake
Nsnow[Nsnow$Plot=="Eri2", 'Plot'] <- "N_Eri2"
Nsnow[Nsnow$Plot=="Eri3", 'Plot'] <- "N_Eri3"
Nsnow[Nsnow$Plot=="Eri4", 'Plot'] <- "N_Eri4"
Nsnow[Nsnow$Plot=="ERI1", 'Plot'] <- "N_Eri1"
Nsnow[Nsnow$Plot=="ERI2", 'Plot'] <- "N_Eri2"
Nsnow[Nsnow$Plot=="ERI3", 'Plot'] <- "N_Eri3"
Nsnow[Nsnow$Plot=="ERI4", 'Plot'] <- "N_Eri4"


# Recode NA
Nsnow[Nsnow$Snow==-9999, 'Snow'] <-  NA


remove("NEri", "NEri0", "NLoi", "NLoi0", "Nsal", "Nsal0", "Nsil", "Nsil0")
