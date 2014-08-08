rm(list=ls())
if(basename(getwd())!="Regional_Collapse"){cat("Plz change your working directory. It should be 'Regional_Collapse'")}
require(plyr); require(dplyr)

# create fish.csv #
###########################################

#read in timeseries_values_view.csv (time series of stock biomass)
fish <- read.csv("original_data/timeseries_values_view.csv", header=T, 
                 stringsAsFactors=F)

#remove the anchovy in the Caspian Sea, save only variables we need
fish <- subset(fish, ASSESSID != "CSERG-ANCHOVYKILKACS-1991-2007-JENSEN" & TSYEAR > 1944, 
               select=c(ASSESSID, TSYEAR, SSB, TOTAL, CATCH_LANDINGS))

# need to make sure time series are in proper units
units <- read.csv("original_data/timeseries_units.csv", stringsAsFactors=F)
# make column to show whether fish units have been checked
fish$CU_total <- NA
fish$CU_ssb <- NA
fish$CU_catch <- NA
####################
# check units for total
#####################
# if MT for total, leave unchanged, mark as checked
  total_mt = subset(units, ASSESSID %in% fish$ASSESSID & TOTAL_UNIT=="MT")
  fish$CU_total[which(fish$ASSESSID %in% total_mt$ASSESSID)] <- 1

# if E03 for total, multiply by (0.45359237/1000)
  e03_total = subset(units, ASSESSID %in% fish$ASSESSID & TOTAL_UNIT=="E03")
  fish$TOTAL[which(fish$ASSESSID %in% e03_total$ASSESSID)] <- fish$TOTAL[which(fish$ASSESSID %in% e03_total$ASSESSID)]*(0.45359237/1000) 
  fish$CU_total[which(fish$ASSESSID %in% e03_total$ASSESSID)] <- 1

# if TOTAL is NA, mark CU_total as checked
  fish$CU_total[is.na(fish$TOTAL)] <- 1

# if relative for total, put in "relative into CU_total
  relative_total <- subset(units, ASSESSID %in% fish$ASSESSID & TOTAL_UNIT=="relative")
  fish$CU_total[which(fish$ASSESSID %in% relative_total$ASSESSID)] <- "relative"

any(is.na(fish$CU_total)) # FALSE, we've checked all totals
######################
# check units for catch
#######################
# if MT for catch, leave unchanged, mark as checked
catch_mt = subset(units, ASSESSID %in% fish$ASSESSID & CATCH_LANDINGS_UNIT=="MT")
fish$CU_catch[which(fish$ASSESSID %in% catch_mt$ASSESSID)] <- 1

# if E03 for catch, divide by 1000 for each
e03_catch = subset(units, ASSESSID %in% fish$ASSESSID & CATCH_LANDINGS_UNIT=="E03")
fish$CATCH_LANDINGS[which(fish$ASSESSID %in% e03_catch$ASSESSID)] <- fish$CATCH_LANDINGS[which(fish$ASSESSID %in% e03_catch$ASSESSID)]*(0.45359237/1000)
fish$CU_catch[which(fish$ASSESSID %in% e03_catch$ASSESSID)] <- 1

# if catch is NA, mark CU_total as checked
fish$CU_catch[is.na(fish$CATCH_LANDINGS)] <- 1

any(is.na(fish$CU_catch)) # FALSE, we've checked them all
######################
# check units for ssb
#######################
# if MT for SSB, leave unchanged, mark as checked
ssb_mt = subset(units, ASSESSID %in% fish$ASSESSID & SSB_UNIT=="MT")
fish$CU_ssb[which(fish$ASSESSID %in% ssb_mt$ASSESSID)] <- 1

# if E03 for total, divide by 1000 for each
e03_ssb = subset(units, ASSESSID %in% fish$ASSESSID & SSB_UNIT=="E03")
fish$SSB[which(fish$ASSESSID %in% e03_ssb$ASSESSID)] <- fish$SSB[which(fish$ASSESSID %in% e03_ssb$ASSESSID)]*(0.45359237/1000)
fish$CU_ssb[which(fish$ASSESSID %in% e03_ssb$ASSESSID)] <- 1

# if TOTAL is NA, mark CU_total as checked
fish$CU_ssb[is.na(fish$SSB)] <- 1

# if relative for total, put in "relative into CU_total
  relative_ssb <- subset(units, ASSESSID %in% fish$ASSESSID & SSB_UNIT=="relative")
  fish$CU_ssb[which(fish$ASSESSID %in% relative_ssb$ASSESSID)] <- "relative"
  
# if larvae for ssb, put in "relative" into CU_ssb
  larvae_ssb <- subset(units, ASSESSID %in% fish$ASSESSID & SSB_UNIT=="E06larvae")
  fish$CU_ssb[which(fish$ASSESSID %in% larvae_ssb$ASSESSID)] <- "E06larvae"
  
# if eggs for ssb, put "eggs" into CU_ssb
  eggs_ssb <- subset(units, ASSESSID %in% fish$ASSESSID & SSB_UNIT=="E03eggs")
  fish$CU_ssb[which(fish$ASSESSID %in% eggs_ssb$ASSESSID)] <- "E03eggs"
  
  any(is.na(fish$CU_ssb)) # FALSE, we've checked all totals
############################
# make units columns useful
##########################
  colnames(fish) <- c("ASSESSID","TSYEAR","SSB","TOTAL","CATCH_LANDINGS","units_total","units_ssb","units_catch")

fish$units_total[fish$units_total==1] <- "MT"
fish$units_total[is.na(fish$TOTAL)] <- NA

fish$units_ssb[fish$units_ssb==1] <- "MT"
fish$units_ssb[is.na(fish$SSB)] <- NA

fish$units_catch[fish$units_catch==1] <- "MT"
fish$units_catch[is.na(fish$CATCH_LANDINGS)] <- NA
##########################
# write csv
###################
write.csv(fish,"data_tables/fish.csv") 
