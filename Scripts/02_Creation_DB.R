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
#------------------------------------


# Rbind all datasets together
flow0 <- rbind(Nsal, Nsil, Zcas, Zdry, Zpap, Zsal, Zsax, Zsil)

# merge with plot size
flow1 <- merge(flow0, Zplot_size[,c('Plot_size', 'Plot')], by="Plot")

# subset only line with "TOTALCOUNT"
flow <- subset(flow1, TotalCount=="TOTALCOUNT")

# delete plot K and W
flow <-flow[!flow$Plot %in% c("K1C","K2C","K3C",'K4C',"K5C","W1C","W2C","W3C",
                  'W4C',"W5C","K3S","K4S","K5S","W3S","W4S","W5S", 
                  "K1S","K2S","W1S","W2S"), ]

# split Date col, to extract year, month, day in new cols
flow <- cbind(str_split_fixed(flow[,"Date"], '-', n=3), flow)
colnames(flow) <- c("Year", "Month", "Day", "Plot", "Site", "Date", "Section",
                    "TotalCount", "Flower_var", "Value", "Species", "Plot_size")

# replace '-9999'
flow[flow$Value==-9999, 'Value'] <- NA

# data as numeric
flow[c("Year", "Month", "Day")] <- sapply(flow[c("Year", "Month", "Day")], as.numeric)
### END



# DATA STRUCTURE FOR ANALYSIS
# select only TotalFlowering line for the total flower per year
flow_sub <- droplevels(flow[flow$Flower_var=="TotalFlowering", ])

#  delete NA in the subset
flow_sub <- flow_sub[complete.cases(flow_sub),]

# delete cas5, cas6, dry7, dry8
flow_sub <-flow_sub[!flow_sub$Plot %in% c('Cas5', 'Cas6', 'Dry7', 'Dry8'), ]

# Replace Section = A-D, A-B with A (will be lumped anyway)
flow_sub[flow_sub$Section=="A-D", "Section"] <- "A"
flow_sub[flow_sub$Section=="A-B", "Section"] <- "A"

# calculate the total flower per plot per year (sum of all sections)
flow_tot_plot <- ddply(flow_sub, .(Site, Year, Species, Plot, Plot_size), 
                       summarise, TotalFlower=sum(Value))

# devise by plot_size
flow_tot_plot$Flow_m2 <- round(flow_tot_plot$TotalFlower/flow_tot_plot$Plot_size, 0)
#### END ---



