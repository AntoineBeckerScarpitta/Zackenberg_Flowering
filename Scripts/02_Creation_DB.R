#######################################################################################
#
#                           Greenland Flowering project
#                             02 - DataBase Creation
#
#######################################################################################
# Antoine Becker-Scarpitta
# June 2020

#  clean R work-space
# rm(list=ls())

# load library
# R, Rstudio need to be updated
# Library need to be installed (see 00_Load_libraries script)
# source("Scripts/00_Load_libraries.r")

# Load 01 - Import_DB
source("Scripts/01_Import_DB.r")


# WHAT THE SCRIPT DO ----------------
# RBIND ALL DB + 
# SELECT "TOTALCOUNT"
# SPLIT Date into Year, Month, Day
# REPLACE -9999 with NA
#------------------------------------


# Rbind all datasets together
flow0 <- rbind(Nsal, Nsil, Zcas, Zdry, Zpap, Zsal, Zsax, Zsil)

# subset only line with "TOTALCOUNT"
flow <- subset(flow0, TotalCount=="TOTALCOUNT")

# split Date col, to extract year, month, day in new cols
flow <- cbind(str_split_fixed(flow[,"Date"], '-', n=3), flow)
colnames(flow) <- c("Year", "Month", "Day", "Site", "Date", "Plot", "Section",
                    "TotalCount", "Flower_var", "Value")

# replace '-9999'
flow[flow$Value==-9999, 'Value'] <- NA

### END
