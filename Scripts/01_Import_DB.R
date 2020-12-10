#######################################################################################
#
#                           Greenland Flowering project
#                             01 - Import DataBase
#
#######################################################################################
# Antoine Becker-Scarpitta
# June 2020

#  clean R work-space
# rm(list=ls())

# load library
# R, Rstudio need to be updated
# Library need to be installed (see 00_Load_libraries script)
source("Scripts/00_Load_libraries.r")



# READ ZACK+NUUK PLOT SIZE
Plot_size <- read.csv("data/datasets/Plot_size_coord_Z_N.csv", header=TRUE,  sep=";")



### ZACKENBERG (6 species) -----------------------------------------------------------
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
# add species names
Zcas$Species <- 'CAS'


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
# add species names
Zdry$Species <- 'DRY'


# READ - Papaver
Zpap0 <- read.csv("data/datasets/View_BioBasis_Zackenberg_Data_Vegetation_Papaver_phenology_and_total_count180520202129447952.csv", 
                 stringsAsFactors=FALSE, header=TRUE,  sep="\t", 
                 strip.white = T,na.strings = c("","NA"))
# SELECT COLS - delete remarks and comments columns
Zpap1 <- Zpap0[ ,c("Date", "Plot", "Section", "Buds", "Flowers", 
                  "Senescent", "Open", "Eaten", "TotalFlowering", "TotalCount")]
# ADD SITE - Zack or Nuuk
Zpap1$Site <- 'Zackenberg'
# MELT - in long format to rbind all DSets
Zpap <- reshape2::melt(Zpap1, id.vars = c("Site", "Date", "Plot", "Section", "TotalCount"),
                       variable.name = "Flower_var", 
                       value.name = "Value")
# add species names
Zpap$Species <- 'PAP'


# READ - Saxifraga
Zsax0 <- read.csv("data/datasets/View_BioBasis_Zackenberg_Data_Vegetation_Saxifraga_phenology_and_total_count180520202130297066.csv", 
                 stringsAsFactors=FALSE, header=TRUE,  sep="\t", 
                 strip.white = T,na.strings = c("","NA"))
# SELECT COLS - delete remarks and comments columns
Zsax1 <- Zsax0[ ,c("Date", "Plot", "Section", "Buds", "Flowers", 
                  "Senescent", "Open", "TotalFlowering", "TotalCount")]
# ADD SITE - Zack or Nuuk
Zsax1$Site <- 'Zackenberg'
# MELT - in long format to rbind all DSets
Zsax <- reshape2::melt(Zsax1, id.vars = c("Site", "Date", "Plot", "Section", "TotalCount"),
                       variable.name = "Flower_var", 
                       value.name = "Value")
# add species names
Zsax$Species <- 'SAX'


# READ - Silene
Zsil0 <- read.csv("data/datasets/View_BioBasis_Zackenberg_Data_Vegetation_Silene_phenology_and_total_count180520202130532109.csv", 
                 stringsAsFactors=FALSE, header=TRUE,  sep="\t", 
                 strip.white = T,na.strings = c("","NA"))
# SELECT COLS - delete remarks and comments columns
Zsil1 <- Zsil0[ ,c("Date", "Plot", "Section", "Buds", "Flowers", 
                  "Senescent", "TotalFlowering", "TotalCount")]
# ADD SITE - Zack or Nuuk
Zsil1$Site <- 'Zackenberg'
# MELT - in long format to rbind all DSets
Zsil <- reshape2::melt(Zsil1, id.vars = c("Site", "Date", "Plot", "Section", "TotalCount"),
                       variable.name = "Flower_var", 
                       value.name = "Value")
# add species names
Zsil$Species <- 'SIL'
# correct Plot==Si1,2,3 with SIL
Zsil[Zsil$Plot=="Si1", 'Plot'] <- "Sil1"
Zsil[Zsil$Plot=="Si2", 'Plot'] <- "Sil2"
Zsil[Zsil$Plot=="Si3", 'Plot'] <- "Sil3"
Zsil[Zsil$Plot=="Si4", 'Plot'] <- "Sil4"


# READ - Salix
Zsal0 <- read.csv("data/datasets/View_BioBasis_Zackenberg_Data_Vegetation_Salix_phenology_and_total_count18052020213537866.csv", 
                 stringsAsFactors=FALSE, header=TRUE,  sep="\t",
                 strip.white = T,na.strings = c("","NA"))
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
# add species names
Zsal$Species <- 'SAL'
#---END ZACKENBERG DATASETS-----------------------------------------------------------






### NUUK (4 species)  ----------------------------------------------------------------
# Databe given by Katrine Raundrup 
Nuuk_all0 <- read.csv("data/datasets/Nuuk_all_sp_TotalCounts_Katrine_version.csv", 
                     stringsAsFactors=FALSE, header=TRUE,  sep=";", strip.white = T,
                     na.strings = c("","NA"))

# Add Site col
Nuuk_all0$Site <- "Nuuk"

# correct Eri in ERI to match all DB
Nuuk_all0[Nuuk_all0$Plot== "Eri1", "Plot"] <- "ERI1"
Nuuk_all0[Nuuk_all0$Plot== "Eri2", "Plot"] <- "ERI2"
Nuuk_all0[Nuuk_all0$Plot== "Eri3", "Plot"] <- "ERI3"
Nuuk_all0[Nuuk_all0$Plot== "Eri4", "Plot"] <- "ERI4"
# Renames plots
Nuuk_all0$Plot <- paste("N", Nuuk_all0$Plot, sep="_")

# Species code
Nuuk_all0[Nuuk_all0$Species== "Salix_glauca", "Species"] <- "SAL"
Nuuk_all0[Nuuk_all0$Species== "Eriophorum_angustifolium", "Species"] <- "ERI"
Nuuk_all0[Nuuk_all0$Species== "Silene_acaulis", "Species"] <- "SIL"
Nuuk_all0[Nuuk_all0$Species== "Loiseleuria_procumbens", "Species"] <-  "LOI" 





# OLD VERSION WITH DB IN GEM. No longer useful since Katrine send new DB
# --------------------------------------------------------------------------------------
# # READ - Salix
# Nsal0 <- read.csv("data/datasets/View_BioBasis_Nuuk_Data_Vegetation_Salix_phenology_and_total_count180520202134518138.csv", 
#                  stringsAsFactors=FALSE, header=TRUE,  sep="\t", strip.white = T,na.strings = c("","NA"))
# # SELECT COLS - delete remarks and comments columns
# Nsal1 <- Nsal0[ ,c("Date", "Plot", "Section", "Buds", 
#                   "Male_flowers", "Female_flowers", "Seed_hairs", 
#                   "Total_Male", "Total_Female", "TotalCount")]
# # ADD SITE - Zack or Nuuk
# Nsal1$Site <- 'Nuuk'
# # MELT - in long format to rbind all DSets
# Nsal <- reshape2::melt(Nsal1, id.vars = c("Site", "Date", "Plot", "Section", "TotalCount"),
#                        variable.name = "Flower_var", 
#                        value.name = "Value")
# # add species names
# Nsal$Species <- 'SAL'
# # SALIX correct typo mistakes: replace -9907 with -9999
# Nsal[Nsal$TotalCount==-9907, "TotalCount"] <- -9999
# # rename plots
# Nsal[Nsal$Plot=="SAL1", 'Plot'] <- "N_Sal1"
# Nsal[Nsal$Plot=="SAL2", 'Plot'] <- "N_Sal2"
# Nsal[Nsal$Plot=="SAL3", 'Plot'] <- "N_Sal3"
# Nsal[Nsal$Plot=="SAL4", 'Plot'] <- "N_Sal4"
# 
# # ++++++++++++++ PROBLEM ++++++++++++++
# # # calculate sum male female flower for Salix
# # Nsal_sel <- droplevels(Nsal[Nsal$Flower_var=='Total_Female'|Nsal$Flower_var=='Total_Male', ])
# # ddply(Nsal, .(Site, Date, Plot, Section, TotalCount, Species), summarize, sum())
# # # PB DOUBLONS DANS LA SUM DES MALE OF FEMALE FLOWER
# # ++++++++++++++++++++++++++++++++++++
# 
# # READ - Silene
# Nsil0 <- read.csv("data/datasets/View_BioBasis_Nuuk_Data_Vegetation_Silene_phenology_and_total_count180520202134090595.csv", 
#                   stringsAsFactors=FALSE, header=TRUE,  sep="\t", strip.white = T,na.strings = c("","NA"))
# # SELECT COLS - delete remarks and comments columns
# Nsil1 <- Nsil0[ ,c("Date", "Plot", "Section", "Buds", "Flowers", 
#                   "Senescent", "TotalFlowering", "TotalCount")]
# # ADD SITE - Zack or Nuuk
# Nsil1$Site <- 'Nuuk'
# # MELT - in long format to rbind all DSets
# Nsil <- reshape2::melt(Nsil1, id.vars = c("Site", "Date", "Plot", "Section", "TotalCount"),
#                        variable.name = "Flower_var", 
#                        value.name = "Value")
# # add species names
# Nsil$Species <- 'SIL'
# # rename plots
# Nsil[Nsil$Plot=="SIL1", 'Plot'] <- "N_Sil1"
# Nsil[Nsil$Plot=="SIL2", 'Plot'] <- "N_Sil2"
# Nsil[Nsil$Plot=="SIL3", 'Plot'] <- "N_Sil3"
# Nsil[Nsil$Plot=="SIL4", 'Plot'] <- "N_Sil4"
# 
# # READ - Loiseleuria (Kalmia)
# NLoi0 <- read.csv("data/datasets/View_BioBasis_Nuuk_Data_Vegetation_Loiseleuria_phenology_and_total_count170920201306510146.csv", 
#                   stringsAsFactors=FALSE, header=TRUE,  sep="\t", strip.white = T,na.strings = c("","NA"))
# # SELECT COLS - delete remarks and comments columns
# NLoi0 <- NLoi0[ ,c("Date", "Plot", "Section", "Buds", "Flowers", 
#                    "Senescent", "TotalFlowering", "TotalCount")]
# # ADD SITE - Zack or Nuuk
# NLoi0$Site <- 'Nuuk'
# # MELT - in long format to rbind all DSets
# NLoi <- reshape2::melt(NLoi0, id.vars = c("Site", "Date", "Plot", "Section", "TotalCount"),
#                        variable.name = "Flower_var", 
#                        value.name = "Value")
# # add species names
# NLoi$Species <- 'LOI'
# # Select only TotalFlowering
# NLoi <- NLoi[NLoi$Flower_var=='TotalFlowering', ]
# 
# # # replace -9999 with NA (easier to manipulate)
# # NLoi[NLoi$TotalCount==-9999, 'TotalCount'] <- NA
# # #  delete NA in the subset
# # NLoi <- NLoi[complete.cases(NLoi),]
# 
# 
# # ++++++++++++++ PROBLEM ++++++++++++++
# # TotalCount value does't match with TotalFLowering
# # Are 0 real 0?
# 
# 
# # rename plots
# NLoi[NLoi$Plot=="LOI1", 'Plot'] <- "N_Loi1"  	
# NLoi[NLoi$Plot=="lOI1", 'Plot'] <- "N_Loi1"  # correct typo
# NLoi[NLoi$Plot=="LOI2", 'Plot'] <- "N_Loi2"
# NLoi[NLoi$Plot=="LOI3", 'Plot'] <- "N_Loi3"
# NLoi[NLoi$Plot=="LOI4", 'Plot'] <- "N_Loi4"
# 
# # READ - Eriophorum 
# NEri0 <- read.csv("data/datasets/View_BioBasis_Nuuk_Data_Vegetation_Eriophorum_total_count170920201325254357.csv", 
#                   stringsAsFactors=FALSE, header=TRUE,  sep="\t", strip.white = T,na.strings = c("","NA"))
# # SELECT COLS - delete remarks and comments columns
# NEri0 <- NEri0[ ,c("Date", "Plot", "Section", "TotalCount")]
# # ADD SITE - Zack or Nuuk
# NEri0$Site <- 'Nuuk'
# # MELT - in long format to rbind all DSets
# NEri <- reshape2::melt(NEri0, id.vars = c("Site", "Date", "Plot", "Section", "TotalCount"),
#                        variable.name = "Flower_var", 
#                        value.name = "Value")
# # add species names
# NEri$Species <- 'ERI'
# # Add col Flower_var==TotalFLowering
# NEri$Flower_var <- "TotalFlowering"
# # Add Value with the totla flowering value (in totalcount)
# NEri$Value <- NEri$TotalCount
# # rename TOTALCOUNT instead of value
# NEri$TotalCount <- 'TOTALCOUNT'
# # rename plots
# NEri[NEri$Plot=="ERI1", 'Plot'] <- "N_Eri1" 
# NEri[NEri$Plot=="Eri1", 'Plot'] <- "N_Eri1" # typo mistake
# NEri[NEri$Plot=="ERI2", 'Plot'] <- "N_Eri2"
# NEri[NEri$Plot=="Eri2", 'Plot'] <- "N_Eri2"
# NEri[NEri$Plot=="ERI3", 'Plot'] <- "N_Eri3"
# NEri[NEri$Plot=="Eri3", 'Plot'] <- "N_Eri3"
# NEri[NEri$Plot=="ERI4", 'Plot'] <- "N_Eri4"
# NEri[NEri$Plot=="Eri4", 'Plot'] <- "N_Eri4"
#END ----------------------------------------------------------------------------------
