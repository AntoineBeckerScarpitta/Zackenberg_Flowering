#######################################################################################
#
#                           Greenland Flowering project
#                             02 - DataBase Creation
#
#######################################################################################
# Antoine Becker-Scarpitta
# September 2020

#  clean R work-space
# rm(list=ls())

# Load 01 - Import_DB (and 00-Librairies)
source("Scripts/00_Load_libraries.r")
source("Scripts/01_Import_DB.r")


# WHAT THE SCRIPT DO ----------------
# RBIND ALL DB + 
# MERGE plot size
# SELECT "TOTALCOUNT"
# DELETE K and W plots
# SPLIT Date into Year, Month, Day
# REPLACE -9999 with NA
# DELETE cas5, cas6, dry7, dry8
# REPLACE Section = A-D, A-B with A
# CALCUL the total flower per plot per year (sum of all sections)
# DEVISE by plot_size
# TRANSFORM quali variable as factor
#------------------------------------





### -- 1 GENERATE THE FINAL DATABASE  ------------------------------------------------
####  NUUK ####
# split Date col, to extract year, month, day in new cols
Nuuk_all <- cbind(str_split_fixed(Nuuk_all0[, "Date"], '/', n=3), Nuuk_all0)
colnames(Nuuk_all) <- c("Day","Month", "Year", "Date", "Species", "Plot", "Section",
                        "Male_flower", "Female_flower", "TotalFlower", "Remarks", "Site")

# merge NUUK with plot size
Nuuk_all <- merge(Nuuk_all, Plot_size[, c('Plot_size', 'Plot')], by="Plot", all.x=TRUE)


# SALIX SELECTION devided MAle and Female flower
# devided male and female flower for Salix
Nsal_tot_plot <- as.data.frame(Nuuk_all %>% filter(Species=="SAL") %>% 
  dplyr::select(Plot, Year, Species, Section, Site, Plot_size, Male_flower, Female_flower) %>%
  pivot_longer(cols=c(Male_flower, Female_flower), names_to="Flower_var", values_to="Value") %>%
  filter(Value != -9999) %>%
  group_by(Site, Year, Species, Plot, Plot_size, Flower_var) %>%
  summarise(TotalFlower=sum(Value)) %>%
  ungroup() %>%
  mutate(Flow_m2=round(TotalFlower/Plot_size, 2)) %>%
  mutate(Species=ifelse(Flower_var=="Male_flower", "SAL_male","SAL_female")) %>%
  dplyr::select(-Flower_var))
  

# Other Species
# Transforme -9999 into NA
Nuuk_all[Nuuk_all$TotalFlower==-9999, "TotalFlower"] <- NA

# calculate the total flower per plot per year (sum of all sections)
Nuuk_all_sub <- Nuuk_all %>% filter(., Species!="SAL") %>%
  group_by(Site, Year, Species, Plot, Plot_size) %>%
  summarise(TotalFlower=sum(TotalFlower)) %>% ungroup()

# remove NA
Nuuk_tot_plot <- Nuuk_all_sub[complete.cases(Nuuk_all_sub), ]

# devise by plot_size
Nuuk_tot_plot$Flow_m2 <- round(Nuuk_tot_plot$TotalFlower/Nuuk_tot_plot$Plot_size, 2)
#---------






####  ZACKENBERG ####
# Salix :Need to rearrange : sum male + sum female (with 50/50 buds)
# divided Date into Year, months, days and select only Year
Zsal <- cbind(str_split_fixed(Zsal[,"Date"], '-', n=3), Zsal)
colnames(Zsal) <- c("Year", "Month", "Day", "Site", "Date", "Plot", "Section",
                    "Flower_var", "Value", "Species")

# sleect TOTALCOUNT, remove -9999, sum at plot level and 
# select only Total Male, Female and Buds
Zsal_sel <- as.data.frame(Zsal %>% 
                        group_by(Site, Year, Plot, Flower_var, Species) %>%
                        dplyr::summarise(Value=sum(Value)) %>% 
                        ungroup() %>%
                        dplyr::filter(Flower_var %in% c("Total_Female", "Total_Male", "Buds"),
                               Value>0)) 
# select only Buds
buds <- Zsal_sel %>% filter(Flower_var=="Buds")
# select only total male and female
smf <- Zsal_sel %>% filter(Flower_var!="Buds")


#SexRatio for Salix Flowers
#  male / female  = x male:1 female ; 1:1 equilibrium SR
sexRatio <- smf %>% 
  pivot_wider(id_cols=c(Site, Year, Plot, Species),
              names_from=Flower_var, values_from=Value, values_fill = 0) %>%
  group_by(Plot) %>%
  dplyr::summarise(sexratio=Total_Male/Total_Female) %>% 
  # Sal6 2019, only male, sex ratio replace with 1
  replace(sapply(., is.infinite), 1) %>%
  #calculate the mean sexration per plot
  dplyr::summarise(MeanSR=mean(sexratio))



# Final arrangement
Zsal_tot_plot <- as.data.frame(smf %>% pivot_wider(id_cols=c(Site, Year, Plot, Species),
                    names_from=Flower_var, values_from=Value, values_fill = 0) %>%
  # merge Buds and smf (need one row for each obs) 
  left_join(.,  dplyr::select(buds, Year, Plot, Value), by=c("Year", "Plot")) %>%
    replace(is.na(.), 0) %>%
  # merge SEXRATIO
  left_join(.,  sexRatio, by="Plot") %>%
  # divided following sexratio
  dplyr::mutate(Total_Male_b=Total_Male + (Value*MeanSR), 
         Total_Female_b=Total_Female + (Value*(1-MeanSR))) %>%
  dplyr::select(c(Site, Year, Species, Plot, Total_Male_b, Total_Female_b)) %>%
  pivot_longer(cols=c(Total_Male_b, Total_Female_b), 
               names_to="Flower_var", values_to="TotalFlower") %>%
    dplyr::filter(TotalFlower>0) %>% ungroup() %>%
  #  created a new species level Salix_male and Salix_female
  mutate(Species=ifelse(Flower_var=="Total_Male_b", "SAL_male","SAL_female")) %>%
  # remove Flowe_var (which was total male, female)
  dplyr::select(., -Flower_var) %>%
  # merge Plot size
  left_join(., Plot_size[,c('Plot_size', 'Plot')], by="Plot") %>%
  # rearrange col order to rbind other species
  dplyr::select(Site, Year, Species, Plot, Plot_size, TotalFlower) %>%
  # calculate the flowering density
  mutate(Flow_m2=round(TotalFlower/Plot_size, 2)))
#END SALIX---- 
         


# Other SPECIES at ZACK
# Rbind all ZACK datasets together except SALIX
Zack0 <- rbind(Zcas, Zdry, Zpap, Zsax, Zsil)

# merge with plot size
Zack1 <- merge(Zack0, Plot_size[,c('Plot_size', 'Plot')], by="Plot", all.x=TRUE)

# delete plot K and W at Zack
Zack <- Zack1[!Zack1$Plot %in% c('Cas5','Cas6','Dry7','Dry8',"K1C","K2C","K3C",'K4C',
                              "K5C","W1C","W2C","W3C",'W4C',"W5C","K3S","K4S","K5S",
                              "W3S","W4S","W5S", "K1S","K2S","W1S","W2S"), ]

# split Date col, to extract year, month, day in new cols
Zack <- cbind(str_split_fixed(Zack[,"Date"], '-', n=3), Zack)
colnames(Zack) <- c("Year", "Month", "Day", "Plot", "Site", "Date", "Section",
                    "Flower_var", "Value", "Species", "Plot_size")

# select only TotalFlowering line for the total flower per year
Zack_sub <- droplevels(Zack[Zack$Flower_var=="TotalFlowering", ])

# replace '-9999' with NA
Zack_sub[Zack_sub$Value==-9999, 'Value'] <- NA
#  delete NA in the subset
Zack_sub <- Zack_sub[complete.cases(Zack_sub),]

# Replace Section = A-D, A-B with A (will be lumped anyway)
Zack_sub[Zack_sub$Section=="A-D", "Section"] <- "A"
Zack_sub[Zack_sub$Section=="A-B", "Section"] <- "A"

# calculate the total flower per plot per year (sum of all sections)
Zack_tot_plot <- Zack_sub %>% group_by(Site, Year, Species, Plot, Plot_size) %>%
                          summarise(TotalFlower=sum(Value)) %>% ungroup() %>%
  # devise by plot_size for density
  dplyr::mutate(Flow_m2=round(TotalFlower/Plot_size, 2))
# END ZACK ------





# Combined ZACKENBERG AND NUUK _ SALIX in 1 table
flow <- as.data.frame(rbind(Zack_tot_plot, Nuuk_tot_plot, Zsal_tot_plot, Nsal_tot_plot))

# data as numeric or factor
flow[,"Year"] <- as.numeric(flow[,"Year"])
flow[c("Site","Species", "Plot")] <- lapply(flow[c("Site","Species", "Plot")], as.factor)

# Remove year 1995 (installation, low trust in that survey)
flow <- flow[flow$Year>1995, ]

# Create new response variable with 0==0.001 (for log transfo)
flow$trans_Flow_m2 <- flow$Flow_m2
flow[flow$Flow_m2==0, 'trans_Flow_m2'] <- 0.001
#### END -----------------------------------------------------------------------------


# #Study design
# table(flow[flow$Site=="Zackenberg", "Year"], flow[flow$Site=="Zackenberg", "Species"])
# table(flow[flow$Site=="Nuuk", "Year"], flow[flow$Site=="Nuuk", "Species"])


# Community level flower production
flow_com <- flow %>% group_by(Site, Year) %>%
  #summarising the total site flower production and plot are per year
  dplyr::summarise(ComFlow=sum(TotalFlower), 
            ComArea=sum(Plot_size)) %>%
  ungroup() %>%
  #calculate the community level density
  dplyr::mutate(ComFlow_m2=ComFlow/ComArea) %>%
  #replace 0 with 0.001 as in sp level data
  dplyr::mutate(ComFlow_m2 = replace(ComFlow_m2, ComFlow_m2==0, 0.001))
#### END -----------------------------------------------------------------------------



# remove temprary files
remove("Nuuk_all", "Nuuk_all_sub", "Nuuk_all0",  "Zack", "Zack_sub", "Zack0", "Zack1", 
      "Zcas", "Zcas0", "Zcas1", "Zdry", "Zdry0", "Zdry1", "Zpap", "Zpap0", "Zpap1", 
      "Zsal", "Zsal0", "Zsal_sel", "buds", "smf", "Zsal1", "Zsax", "Zsax0", "Zsax1", 
      "Zsil", "Zsil0", "Zsil1", "sexRatio", "Zack_tot_plot", "Nuuk_tot_plot", "Nsal_tot_plot",
      "Zsal_tot_plot", "Plot_size")



