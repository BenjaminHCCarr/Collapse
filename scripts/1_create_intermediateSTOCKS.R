#Please set your working directory to .../Regional_Collapse"
# requires: reference_point_value_view.csv
#           assessor.csv
#           taxonomy.csv
#           RAM_Legacy_database_summary.csv
#           lmerefs.csv
#           lmetostocks.csv
#           timeseries_units.csv

rm(list=ls())
if(basename(getwd())!="Regional_Collapse"){cat("Plz change your working directory. It should be 'Regional_Collapse'")}
require(plyr); require(dplyr)

# load fish.csv, if doesn't exist, run script to generate
  if(!file.exists("data_tables/fish.csv")){source("Rscripts/0_createFISH.R")}
  fish <- read.csv("data_tables/fish.csv", stringsAsFactors=F)

# define warning function
check_rows <- function(stocks){
  if(nrow(stocks)==330){
    cat("All good, 330 rows")}else{
      warning("Something's wrong, there should be 330 rows in stocks")} 
}

# create ASSESSID to start
stocks <- data.frame(ASSESSID = unique(fish$ASSESSID))

###########################
# add in BMSY and SSBMSY - in correct units!
###########################
# read in reference_point_values_view.csv
  reference <- read.csv("original_data/reference_point_value_view.csv", 
                      stringsAsFactors=F)
# drop columns we don't need, reformat
  reference <- select(reference, ASSESSID, COMMON, AREA, BMSY, SSBMSY, UMSY)
  reference$BMSY <- as.numeric(reference$BMSY) # should introduce NAs

# get units -- everything needs to be in MT (metric tons)

# make column in stocks for TOTAL and SSB that is a 1 if TOTAL can be made into 
# MT, else 0. If there's no BMSY, is NA. 

# load unit data
  units <- read.csv("original_data/timeseries_units.csv", stringsAsFactors = F)
  units <- select(units, ASSESSID, SSB_UNIT, TOTAL_UNIT, CATCH_LANDINGS_UNIT)
# create column to mark
  stocks$BRP_SSB <- NA
  stocks$BRP_BMSY <- NA 
  stocks$metric_TOTAL <- NA
  stocks$metric_SSB <- NA
  
# for all total units that have "MT", fill in the value in stocks and label 
# "MT" in metrics column
  mt_total <- subset(units, TOTAL_UNIT=="MT")
  # make sure these stocks have a reference entry
  mt_total <- subset(mt_total, ASSESSID %in% reference$ASSESSID & ASSESSID %in% stocks$ASSESSID)
  # one stock is missing, so should expect 256 non NA entries in stocks$metric_TOTAL
  length(which(mt_total$ASSESSID %in% stocks$ASSESSID))
  
  for(i in 1:nrow(mt_total)){
    stock_id = mt_total$ASSESSID[i]
    # does the stock exist in reference dataset?
    if(length(reference$BMSY[reference$ASSESSID==stock_id])>0){
    stocks$BRP_BMSY[stocks$ASSESSID==stock_id] = 
      reference$BMSY[reference$ASSESSID==stock_id]
    stocks$metric_TOTAL[stocks$ASSESSID==stock_id] = "MT"
    }
  }

  length(which(!is.na(stocks$metric_TOTAL))) == nrow(mt_total)
  # should be TRUE, means everything accounted for

# change any metric_total that have E03 to metric tons, conversion is 0.45359237 MT per 1000 lbs
  total_e03 <- subset(units, TOTAL_UNIT=="E03")
  total_e03 <- subset(total_e03, ASSESSID %in% reference$ASSESSID & ASSESSID %in% stocks$ASSESSID)

  for(i in 1:nrow(total_e03)){
    stock_id = total_e03$ASSESSID[i]
    # does the stock exist in reference dataset?
    if(length(reference$BMSY[reference$ASSESSID==stock_id])>0){
      stocks$BRP_BMSY[stocks$ASSESSID==stock_id] = 
        reference$BMSY[reference$ASSESSID==stock_id]*(0.45359237/1000)
      stocks$metric_TOTAL[stocks$ASSESSID==stock_id] = "MT"
    }
  }
  
  # now should expect this to be true
  length(which(!is.na(stocks$metric_TOTAL))) == nrow(mt_total) + nrow(total_e03)

# any stocks that don't have E03 or MT as units, will be put in with their metric into the table
  left_stocks <- subset(units, TOTAL_UNIT!="MT" & TOTAL_UNIT!="E03")
  left_stocks <- subset(left_stocks, ASSESSID %in% reference$ASSESSID & ASSESSID %in% stocks$ASSESSID)
  
  for(i in 1:nrow(left_stocks)){
    stock_id = left_stocks$ASSESSID[i]
    stocks$BRP_BMSY[stocks$ASSESSID==stock_id] <- reference$BMSY[reference$ASSESSID==stock_id]
    stocks$metric_TOTAL[stocks$ASSESSID==stock_id] <- units$TOTAL_UNIT[units$ASSESSID==stock_id]
  }

# any values that have blank for type of metric will be filled in with "no value"
  stocks[stocks==""] <- "no value"
  stocks$metric_TOTAL[is.na(stocks$metric_TOTAL)] <- "no value"

# if E03 for ssb, change to MT and mark
  ssb_e03 <- subset(units, SSB_UNIT == "E03" & ASSESSID %in% reference$ASSESSID & ASSESSID %in% stocks$ASSESSID)
  
  for(i in 1:nrow(ssb_e03)){
    stock_id = ssb_e03$ASSESSID[i]
    stocks$BRP_SSB[stocks$ASSESSID==stock_id] <- reference$SSBMSY[reference$ASSESSID==stock_id]*(0.45359237/1000)
    stocks$metric_SSB[stocks$ASSESSID==stock_id] <- "MT"
  }

# if something else for ssb_units -> don't do anything but mark
  ssb_other <- subset(units, ASSESSID %in% reference$ASSESSID & ASSESSID %in% stocks$ASSESSID)

  for(i in 1:nrow(ssb_other)){
    stock_id=ssb_other$ASSESSID[i]
    stocks$BRP_SSB[stocks$ASSESSID==stock_id] <- reference$SSBMSY[reference$ASSESSID==stock_id]
    stocks$metric_SSB[stocks$ASSESSID==stock_id] <- ssb_other$SSB_UNIT[i]
  }

# replace all NAs, empty entries with "no value"
  stocks$metric_SSB[stocks$metric_SSB==""] <- "no value"
  stocks$metric_SSB[is.na(stocks$metric_SSB)] <- "no value"
###########
# put in measure of fishing intensity
##############
# calculate mean, median and 90th percentile of harvesting rate (Ct/Bt)
ut = ddply(fish, .(ASSESSID), summarize, mean_ut = mean(CATCH_LANDINGS/TOTAL,na.rm=T), median_ut = median(CATCH_LANDINGS/TOTAL, na.rm=T),q90_ut = quantile(CATCH_LANDINGS/TOTAL, .9, na.rm=T))

stocks <- merge(stocks, ut, by="ASSESSID", all.x=T)
check_rows(stocks)
# calculate mean, median and 90th percentile of Ut/Umsy for those provided
  # make columns for catch and UMSY by species
  catch_msy = select(fish, ASSESSID, TOTAL, CATCH_LANDINGS)
  umsy = select(reference, ASSESSID, UMSY)
  catch_msy = merge(catch_msy, umsy, by = "ASSESSID", all.x=T)
  catch_msy$ut = catch_msy$CATCH_LANDINGS/catch_msy$TOTAL
  catch_msy$ratio = catch_msy$ut/catch_msy$UMSY

u_msy = ddply(catch_msy, .(ASSESSID), summarize, mean_umsy = mean(ratio, na.rm =T), median_umsy = median(ratio,na.rm=T), q90umsy=quantile(ratio, .9, na.rm=T))

stocks <- merge(stocks, u_msy, by="ASSESSID", all.x=T)

###########
# merge in common name and area from reference.csv
############
cn <- select(reference, ASSESSID, COMMON, AREA)
stocks <- merge(stocks, cn, by="ASSESSID", all.x=T)
check_rows(stocks)
######################
# getting mgmt area
#######################
#read in assessor.csv

assessor <- read.csv("original_data/assessor.csv", 
                     stringsAsFactors = F)
assessor = assessor[-35,] #RAM was making a big problem

# add a column for "management area" (MGMT)
MGMT = data.frame(ASSESSID = stocks$ASSESSID, 
                  MGMT = rep(NA, length(stocks$ASSESSID)))
for(i in 1:length(assessor$ASSESSORID)){
  rows = grep(assessor$ASSESSORID[i],stocks$ASSESSID)
  if(length(rows)>0){MGMT[rows,2] = assessor$MGMT[i]}
}

stocks <- merge(stocks, MGMT, by = "ASSESSID", all.x = TRUE)
check_rows(stocks)
##########################
# get scientific name
######################
# read in taxanomy.csv
taxa = read.csv("original_data/taxonomy.csv", stringsAsFactors = F)

# add a column for "latin name" (LATINNAME)
taxonomy = data.frame(ASSESSID = stocks$ASSESSID, 
                      Scientific_name = rep(NA, nrow(stocks)),
                      Common_name = rep(NA,nrow(stocks)))
for(i in 1:length(taxa$SCIENTIFICNAME)){
  rows = grep(taxa$MYERSNAME[i],stocks$ASSESSID)
  if(length(rows)>0){
    taxonomy[rows,2] = taxa$SCIENTIFICNAME[i]
    taxonomy[rows,3] = taxa$COMMONNAME1[i]}
}

stocks <- merge(stocks, taxonomy[,1:2], by = "ASSESSID", all.x = TRUE)
stocks$COMMON[which(is.na(stocks$COMMON))] <- taxonomy$Common_name[which(is.na(stocks$COMMON))]

# three species missing scientific name, gotten by hand from fishbase
stocks$COMMON[48]
stocks$Scientific_name[48] <- "Thunnus maccoyii"
stocks$COMMON[21]
stocks$Scientific_name[21] <- "Sebastes alutus" # came from this db
stocks$COMMON[110]
stocks$Scientific_name[110] <- "Engraulis ringens" # came from wikipedia
check_rows(stocks)
#########################
# LME number and name
#######################
#assign an LME to each stock using RAM_Legacy_database_summary.csv
lmes <- read.csv("original_data/RAM_Legacy_database_summary.csv", 
                 stringsAsFactors = F)
lmes <- select(lmes, ASSESSID, LME_NAME)
stocks <- merge(stocks, lmes, by="ASSESSID", all.x=T, all.y=F)
check_rows(stocks)
#################################
#add LME_NUMBER from lmerefs.csv
#################################
refs <- read.csv("original_data/lmerefs.csv", header=T, stringsAsFactors = F)
stocks <- merge(stocks,refs, by="LME_NAME",all.x = TRUE, all.y=FALSE)
check_rows(stocks)
####################
#add a column for the number of LMEs each stock has been assigned to (ranges from primary (1 LME) to quintary (5 LMEs)).
######################
lmetostocks <- read.csv("original_data/lmetostocks.csv", stringsAsFactors = F)
n.lmes <- ddply(lmetostocks, .(STOCKID), summarize, LME_COUNT = length(LME_NUMBER))

#merge that into stocks file by searching for STOCKID in ASSESSID
stockID <- data.frame(ASSESSID = stocks$ASSESSID, NUM_LMEs = rep(NA, nrow(stocks)))
for(i in 1:nrow(stockID)){
  rows = grep(n.lmes$STOCKID[i], stocks$ASSESSID)
  if(length(rows)>0){
    stockID[rows,2] = n.lmes$LME_COUNT[i]
  }
}

stocks = merge(stocks, stockID, by = "ASSESSID", all.x = TRUE)
check_rows(stocks)
####################
# add in first year of TS
######################
# add first year of time series for TOTAL and SSB
# will generate warnings because for some there are no entries for TOTAL or SSB. 
int_ts_TOTAL <- subset(fish, TOTAL > 0) # there needs to be a value for one of these
int_ts_SSB <- subset(fish, SSB > 0)
min_TOTAL <- ddply(int_ts_TOTAL, .(ASSESSID), summarize, firstYR_TOTAL = min(TSYEAR, na.rm=T))
min_SSB <- ddply(int_ts_SSB, .(ASSESSID), summarize, firstYR_SSB = min(TSYEAR,na.rm=T))

stocks <- merge(stocks, min_TOTAL, by="ASSESSID", all.x=T)
stocks <- merge(stocks, min_SSB, by="ASSESSID", all.x = T)
check_rows(stocks)
######################
# reorder rows
###################
ordered_names <- c("ASSESSID", "COMMON","Scientific_name", "MGMT","AREA",
                   "LME_NAME","LME_NUMBER","NUM_LMEs","BRP_BMSY","BRP_SSB", 
                   "metric_TOTAL","metric_SSB","firstYR_TOTAL","firstYR_SSB", "mean_ut","median_ut","q90_ut","mean_umsy","median_umsy","q90umsy")
stocks <- stocks[,ordered_names]

# rename for clarity
colnames(stocks) <- c("ASSESSID", "COMMON","Scientific_name", "MGMT","AREA",
                      "LME_NAME","LME_NUMBER","NUM_LMEs","BRP_BMSY",
                      "BRP_SSBMSY","metric_TOTAL","metric_SSB","firstYR_TOTAL","firstYR_SSB","mean_ut","median_ut","q90_ut","mean_umsy","median_umsy","q90umsy")
####################
# write intermediate stocks.
########################
write.csv(stocks, row.names=FALSE, "data_tables/intermediate_stocks.csv")
