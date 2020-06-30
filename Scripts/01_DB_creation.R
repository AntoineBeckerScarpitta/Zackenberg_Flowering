#######################################################################################
#
#                           Greenland Flowering project
#                                 01 - DB CREATION
#
#######################################################################################
# Antoine Becker-Scarpitta
# June 2020

#  clean R work-space
rm(list=ls())

# load library
# R, Rstudio need to be updated
# Library need to be installed (see 00_Load_libraries script)
source("Scripts/00_Load_libraries.r")




### READ DATASET ###

### Zackenberg (6 species)
# Cassiope
Zcas0 <- read.csv("data/datasets/View_BioBasis_Zackenberg_Data_Vegetation_Cassiope_phenology_and_total_count.csv", 
                 stringsAsFactors=FALSE, header=TRUE,  sep="\t", strip.white = T,
                 na.strings = c("","NA"))
# delete remarks and comments columns
Zcas <- Zcas0[ ,c("Date", "Plot", "Section", "Buds", "Flowers", 
                  "Senescent", "TotalFlowering", "TotalCount")]
#  add SITE column with ZACK
Zcas$Site <- 'Zackenberg'

# Dryas
Zdry0 <- read.csv("data/datasets/View_BioBasis_Zackenberg_Data_Vegetation_Cassiope_phenology_and_total_count.csv", 
                 stringsAsFactors=FALSE, header=TRUE,  sep="\t", strip.white = T,na.strings = c("","NA"))
# delete remarks and comments
Zdry <- Zdry0[ ,c("Date", "Plot", "Section", "Buds", "Flowers", 
                  "Senescent", "TotalFlowering", "TotalCount")]
#  add SITE column with ZACK
Zdry$Site <- 'Zackenberg'


# Papaver
Zpap0 <- read.csv("data/datasets/View_BioBasis_Zackenberg_Data_Vegetation_Cassiope_phenology_and_total_count.csv", 
                 stringsAsFactors=FALSE, header=TRUE,  sep="\t", strip.white = T,na.strings = c("","NA"))
# delete remarks and comments
Zpap <- Zpap0[ ,c("Date", "Plot", "Section", "Buds", "Flowers", 
                  "Senescent", "TotalFlowering", "TotalCount")]
#  add SITE column with ZACK
Zpap$Site <- 'Zackenberg'


# Saxifraga
Zsax0 <- read.csv("data/datasets/View_BioBasis_Zackenberg_Data_Vegetation_Cassiope_phenology_and_total_count.csv", 
                 stringsAsFactors=FALSE, header=TRUE,  sep="\t", strip.white = T,na.strings = c("","NA"))
# delete remarks and comments
Zsax <- Zsax0[ ,c("Date", "Plot", "Section", "Buds", "Flowers", 
                  "Senescent", "TotalFlowering", "TotalCount")]
#  add SITE column with ZACK
Zsax$Site <- 'Zackenberg'



# Silene
Zsil0 <- read.csv("data/datasets/View_BioBasis_Zackenberg_Data_Vegetation_Cassiope_phenology_and_total_count.csv", 
                 stringsAsFactors=FALSE, header=TRUE,  sep="\t", strip.white = T,na.strings = c("","NA"))
# delete remarks and comments
Zsil <- Zsil0[ ,c("Date", "Plot", "Section", "Buds", "Flowers", 
                  "Senescent", "TotalFlowering", "TotalCount")]
#  add SITE column with ZACK
Zsil$Site <- 'Zackenberg'



# Salix
Zsal0 <- read.csv("data/datasets/View_BioBasis_Zackenberg_Data_Vegetation_Cassiope_phenology_and_total_count.csv", 
                 stringsAsFactors=FALSE, header=TRUE,  sep="\t", strip.white = T,na.strings = c("","NA"))
# delete remarks and comments
Zsal <- Zsal0[ ,c("Date", "Plot", "Section", "Buds", "Flowers", 
                  "Senescent", "TotalFlowering", "TotalCount")]
#  add SITE column with ZACK
Zsal$Site <- 'Zackenberg'




### Nuuk
# 2 species
# Salix
Nsal0 <- read.csv("data/datasets/View_BioBasis_Zackenberg_Data_Vegetation_Cassiope_phenology_and_total_count.csv", 
                 stringsAsFactors=FALSE, header=TRUE,  sep="\t", strip.white = T,na.strings = c("","NA"))
# delete remarks and comments
Nsal <- Nsal0[ ,c("Date", "Plot", "Section", "Buds", "Flowers", 
                  "Senescent", "TotalFlowering", "TotalCount")]
#  add SITE column with NUUK
Nsal$Site <- 'Nuuk'



# Silene
Nsil0 <- read.csv("data/datasets/View_BioBasis_Zackenberg_Data_Vegetation_Cassiope_phenology_and_total_count.csv", 
                  stringsAsFactors=FALSE, header=TRUE,  sep="\t", strip.white = T,na.strings = c("","NA"))
# delete remarks and comments
Nsil <- Nsil0[ ,c("Date", "Plot", "Section", "Buds", "Flowers", 
                  "Senescent", "TotalFlowering", "TotalCount")]
#  add SITE column with NUUK
Nsil$Site <- 'Nuuk'











