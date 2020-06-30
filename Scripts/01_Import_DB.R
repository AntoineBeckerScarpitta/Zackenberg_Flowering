#######################################################################################
#
#                           Greenland Flowering project
#                             01 - Import DataBase
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



# READ DATASETS + 
# SELECT COLS + 
# ADD SITE +
# MELT TABLES



### ZACKENBERG (6 species)
# READ - Cassiope
Zcas0 <- read.csv("data/datasets/View_BioBasis_Zackenberg_Data_Vegetation_Cassiope_phenology_and_total_count.csv", 
                 stringsAsFactors=FALSE, header=TRUE,  sep="\t", strip.white = T,
                 na.strings = c("","NA"))
# SELECT COLS - delete remarks and comments columns
Zcas1 <- Zcas0[ ,c("Date", "Plot", "Section", "Buds", "Flowers", 
                  "Senescent", "TotalFlowering", "TotalCount")]
# ADD SITE - Zack or Nuuk
Zcas1$Site <- 'Zackenberg'
# MELT - in long format to rbind all DSets
Zcas <- reshape2::melt(Zcas1, id.vars = c("Site", "Date", "Plot", "Section", "TotalCount"),
            variable.name = "Flower_var", 
            value.name = "Value")




# READ - Dryas
Zdry0 <- read.csv("data/datasets/View_BioBasis_Zackenberg_Data_Vegetation_Dryas_phenology_and_total_count180520201149561.csv", 
                 stringsAsFactors=FALSE, header=TRUE,  sep="\t", strip.white = T,na.strings = c("","NA"))
# SELECT COLS - delete remarks and comments columns
Zdry1 <- Zdry0[ ,c("Date", "Plot", "Section", "Buds", "Flowers", 
                  "Senescent", "Larvae", "Eaten", "TotalFlowering", "TotalCount")]
# ADD SITE - Zack or Nuuk
Zdry1$Site <- 'Zackenberg'
# MELT - in long format to rbind all DSets
Zdry <- reshape2::melt(Zdry1, id.vars = c("Site", "Date", "Plot", "Section", "TotalCount"),
                       variable.name = "Flower_var", 
                       value.name = "Value")




# READ - Papaver
Zpap0 <- read.csv("data/datasets/View_BioBasis_Zackenberg_Data_Vegetation_Papaver_phenology_and_total_count180520202129447952.csv", 
                 stringsAsFactors=FALSE, header=TRUE,  sep="\t", strip.white = T,na.strings = c("","NA"))
# SELECT COLS - delete remarks and comments columns
Zpap1 <- Zpap0[ ,c("Date", "Plot", "Section", "Buds", "Flowers", 
                  "Senescent", "Open", "Eaten", "TotalFlowering", "TotalCount")]
# ADD SITE - Zack or Nuuk
Zpap1$Site <- 'Zackenberg'
# MELT - in long format to rbind all DSets
Zpap <- reshape2::melt(Zpap1, id.vars = c("Site", "Date", "Plot", "Section", "TotalCount"),
                       variable.name = "Flower_var", 
                       value.name = "Value")




# READ - Saxifraga
Zsax0 <- read.csv("data/datasets/View_BioBasis_Zackenberg_Data_Vegetation_Saxifraga_phenology_and_total_count180520202130297066.csv", 
                 stringsAsFactors=FALSE, header=TRUE,  sep="\t", strip.white = T,na.strings = c("","NA"))
# SELECT COLS - delete remarks and comments columns
Zsax1 <- Zsax0[ ,c("Date", "Plot", "Section", "Buds", "Flowers", 
                  "Senescent", "Open", "TotalFlowering", "TotalCount")]
# ADD SITE - Zack or Nuuk
Zsax1$Site <- 'Zackenberg'
# MELT - in long format to rbind all DSets
Zsax <- reshape2::melt(Zsax1, id.vars = c("Site", "Date", "Plot", "Section", "TotalCount"),
                       variable.name = "Flower_var", 
                       value.name = "Value")




# READ - Silene
Zsil0 <- read.csv("data/datasets/View_BioBasis_Zackenberg_Data_Vegetation_Silene_phenology_and_total_count180520202130532109.csv", 
                 stringsAsFactors=FALSE, header=TRUE,  sep="\t", strip.white = T,na.strings = c("","NA"))
# SELECT COLS - delete remarks and comments columns
Zsil1 <- Zsil0[ ,c("Date", "Plot", "Section", "Buds", "Flowers", 
                  "Senescent", "TotalFlowering", "TotalCount")]
# ADD SITE - Zack or Nuuk
Zsil1$Site <- 'Zackenberg'
# MELT - in long format to rbind all DSets
Zsil <- reshape2::melt(Zsil1, id.vars = c("Site", "Date", "Plot", "Section", "TotalCount"),
                       variable.name = "Flower_var", 
                       value.name = "Value")




# READ - Salix
Zsal0 <- read.csv("data/datasets/View_BioBasis_Zackenberg_Data_Vegetation_Salix_phenology_and_total_count18052020213537866.csv", 
                 stringsAsFactors=FALSE, header=TRUE,  sep="\t", strip.white = T,na.strings = c("","NA"))
# SELECT COLS - delete remarks and comments columns
Zsal1 <- Zsal0[ ,c("Date", "Plot", "Section", "Buds", 
                  "Male_flowers", "Female_flowers", "Seed_hairs", 
                  "Total_Male", "Total_Female", "TotalCount")]
# ADD SITE - Zack or Nuuk
Zsal1$Site <- 'Zackenberg'
# MELT - in long format to rbind all DSets
Zsal <- reshape2::melt(Zsal1, id.vars = c("Site", "Date", "Plot", "Section", "TotalCount"),
                       variable.name = "Flower_var", 
                       value.name = "Value")




### NUUK ###
# 2 species
# READ - Salix
Nsal0 <- read.csv("data/datasets/View_BioBasis_Nuuk_Data_Vegetation_Salix_phenology_and_total_count180520202134518138.csv", 
                 stringsAsFactors=FALSE, header=TRUE,  sep="\t", strip.white = T,na.strings = c("","NA"))
# SELECT COLS - delete remarks and comments columns
Nsal1 <- Nsal0[ ,c("Date", "Plot", "Section", "Buds", 
                  "Male_flowers", "Female_flowers", "Seed_hairs", 
                  "Total_Male", "Total_Female", "TotalCount")]
# ADD SITE - Zack or Nuuk
Nsal1$Site <- 'Nuuk'
# MELT - in long format to rbind all DSets
Nsal <- reshape2::melt(Nsal1, id.vars = c("Site", "Date", "Plot", "Section", "TotalCount"),
                       variable.name = "Flower_var", 
                       value.name = "Value")




# READ - Silene
Nsil0 <- read.csv("data/datasets/View_BioBasis_Nuuk_Data_Vegetation_Silene_phenology_and_total_count180520202134090595.csv", 
                  stringsAsFactors=FALSE, header=TRUE,  sep="\t", strip.white = T,na.strings = c("","NA"))
# SELECT COLS - delete remarks and comments columns
Nsil1 <- Nsil0[ ,c("Date", "Plot", "Section", "Buds", "Flowers", 
                  "Senescent", "TotalFlowering", "TotalCount")]
# ADD SITE - Zack or Nuuk
Nsil1$Site <- 'Nuuk'
# MELT - in long format to rbind all DSets
Nsil <- reshape2::melt(Nsil1, id.vars = c("Site", "Date", "Plot", "Section", "TotalCount"),
                       variable.name = "Flower_var", 
                       value.name = "Value")










