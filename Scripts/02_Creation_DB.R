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
source("Scripts/01_Import_DB.r")

# DID
# delete K, W, plots => DONE
# add SITE in all dataset => DONE
# correct Zack Sil , Si names => DONE
# as.factor(year) => DONE
# NA in SECTION => DONE
# cas5, cas6, dry7, dry8 half no data => DONE
#  ADD plot size and divide flow numb by plot size => DONE
# different format of data in SECTION A-D, A-B => DONE

# TO DO 
# NUUK has a different structure NEED TO FIGURED OUT how to integrated it


# Check the variation in survey period (month level)



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
#  NUUK
# split Date col, to extract year, month, day in new cols
Nuuk_all <- cbind(str_split_fixed(Nuuk_all0[, "Date"], '/', n=3), Nuuk_all0)
colnames(Nuuk_all) <- c("Day","Month", "Year", "Date", "Species", "Plot", "Section",
                        "Male_flower", "Female_flower", "TotalFlower", "Remarks", "Site")

# merge NUUK with plot size
Nuuk_all <- merge(Nuuk_all, Plot_size[, c('Plot_size', 'Plot')], by="Plot", all.x=TRUE)

# Transforme -9999 into NA
Nuuk_all[Nuuk_all$TotalFlower==-9999, "TotalFlower"] <- NA

# calculate the total flower per plot per year (sum of all sections)
Nuuk_all_sub <- ddply(Nuuk_all, .(Site, Year, Species, Plot, Plot_size), 
                      summarise, TotalFlower=sum(TotalFlower))

# remove NA
Nuuk_tot_plot <- Nuuk_all_sub[complete.cases(Nuuk_all_sub), ]

# devise by plot_size
Nuuk_tot_plot$Flow_m2 <- round(Nuuk_tot_plot$TotalFlower/Nuuk_tot_plot$Plot_size, 0)




# ZACKENBERG
# Rbind all ZACK datasets together
Zack0 <- rbind(Zcas, Zdry, Zpap, Zsal, Zsax, Zsil)

# merge with plot size
Zack1 <- merge(Zack0, Plot_size[,c('Plot_size', 'Plot')], by="Plot", all.x=TRUE)

# subset only line with "TOTALCOUNT"
Zack <- subset(Zack1, TotalCount=="TOTALCOUNT")

# delete plot K and W at Zack
Zack <- Zack[!Zack$Plot %in% c('Cas5','Cas6','Dry7','Dry8',"K1C","K2C","K3C",'K4C',
                              "K5C","W1C","W2C","W3C",'W4C',"W5C","K3S","K4S","K5S",
                              "W3S","W4S","W5S", "K1S","K2S","W1S","W2S"), ]

# split Date col, to extract year, month, day in new cols
Zack <- cbind(str_split_fixed(Zack[,"Date"], '-', n=3), Zack)
colnames(Zack) <- c("Year", "Month", "Day", "Plot", "Site", "Date", "Section",
                    "TotalCount", "Flower_var", "Value", "Species", "Plot_size")

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
Zack_tot_plot <- ddply(Zack_sub, .(Site, Year, Species, Plot, Plot_size), 
                       summarise, TotalFlower=sum(Value))

# devise by plot_size
Zack_tot_plot$Flow_m2 <- round(Zack_tot_plot$TotalFlower/Zack_tot_plot$Plot_size, 0)



# Combined ZACKENBERG AND NUUK in 1 table
flow <- rbind(Zack_tot_plot, Nuuk_tot_plot)

# data as numeric or factor
flow[,"Year"] <- as.numeric(flow[,"Year"])
flow[c("Site","Species", "Plot")] <- lapply(flow[c("Site","Species", "Plot")], as.factor)


#### END -----------------------------------------------------------------------------





