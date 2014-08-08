################################################
#  Regional Collapse Code (OSS Workshop 2014) #
################################################
#Please set your working directory to .../Regional_Collapse"
rm(list=ls())
if(basename(getwd())!="Regional_Collapse"){cat("Dude, change your working directory. It should be 'Regional_Collapse'")}
require(plyr); require(dplyr)

# define warning function
  check_rows <- function(stocks){
  if(nrow(stocks)==330){
    cat("All good, 330 rows")}else{
    warning("Something's wrong, there should be 330 rows in stocks")} 
  }
###########################################
# create stocks.csv

# read in reference_point_values_view.csv
  reference <- read.csv("original_data/reference_point_value_view.csv", 
                        stringsAsFactors=F)
# drop columns we don't need
  reference <- select(reference, ASSESSID, COMMON, AREA, BMSY, SSBMSY)
  reference$BMSY <- as.numeric(reference$BMSY) # should introduce NAs

# create ASSESSID to start
  stocks <- data.frame(ASSESSID = unique(fish$ASSESSID))
  stocks <- merge(stocks, reference, by="ASSESSID", all.x = TRUE)
    check_rows(stocks)

# getting mgmt area
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

# get scientific name
# read in taxanomy.csv
  taxa = read.csv("original_data/taxonomy.csv", stringsAsFactors = F)

# add a column for "latin name" (LATINNAME)
  taxonomy = data.frame(ASSESSID = stocks$ASSESSID, 
                      Scientific_name = rep(NA, nrow(stocks)))
  for(i in 1:length(taxa$SCIENTIFICNAME)){
      rows = grep(taxa$MYERSNAME[i],stocks$ASSESSID)
      if(length(rows)>0){taxonomy[rows,2] = taxa$SCIENTIFICNAME[i]}
    }

stocks <- merge(stocks, taxonomy, by = "ASSESSID", all.x = TRUE)
check_rows(stocks)

# LME number and name
  #assign an LME to each stock using RAM_Legacy_database_summary.csv
  lmes <- read.csv("original_data/RAM_Legacy_database_summary.csv", 
                   stringsAsFactors = F)
  lmes <- select(lmes, ASSESSID, LME_NAME)
  stocks <- merge(stocks, lmes, by="ASSESSID", all.x=T, all.y=F)
  check_rows(stocks)

#add LME_NUMBER from lmerefs.csv
  refs <- read.csv("original_data/lmerefs.csv", header=T, stringsAsFactors = F)
  stocks <- merge(stocks,refs, by="LME_NAME",all.x = TRUE, all.y=FALSE)
  check_rows(stocks)

#add a column for the number of LMEs each stock has been assigned to (ranges from primary (1 LME) to quintary (5 LMEs)).
  lmetostocks <- read.csv("original_data/lmetostocks.csv", stringsAsFactors = F)
  n.lmes <- ddply(lmetostocks, .(STOCKID), summarize, LME_COUNT = length(LME_NUMBER))

#merge that into stocks file by searching for STOCKID in ASSESSID
  stockID <- data.frame(ASSESSID = stocks$ASSESSID, STOCKID = rep(NA, nrow(stocks)))
  for(i in 1:nrow(stockID)){
    rows = grep(n.lmes$STOCKID[i], stocks$ASSESSID)
    if(length(rows)>0){
      stockID[rows,2] = n.lmes$LME_COUNT[i]
    }
  }

stocks = merge(stocks, stockID, by = "ASSESSID", all.x = TRUE)
check_rows(stocks)



stocks$final_analysis <- rep(0, nrow(stocks))
stocks$final_analysis[which(as.character(stocks$ASSESSID) %in% as.character(final_stocks$ASSESSID))] = 1


# add ADMB estimated brps
admb <- read.csv("ADMB/BMSY.csv", stringsAsFactors=F)
# remain bmsy so we don't get confused
admb$X <- NULL
admb$short_id <- NULL
colnames(admb) <- c("ASSESSID","admb_bmsy")

##### fish
fish <- merge(fish, admb, by="ASSESSID", all.x=TRUE)

# want stocks that have brps, or at least 20 years of data in total biomass
	summary_table <- ddply(fish, .(ASSESSID), summarize, 
                           mean_ssb = mean(SSB, na.rm=T, nan.rm=T), 
                           mean_total = mean(TOTAL, na.rm=T, nan.rm=T),
                           mean_catch = mean(CATCH_LANDINGS, na.rm=T, nan.rm=T), 
                           mean_admb_bmsy = mean(admb_bmsy, na.rm=T, nan.rm=T), 
                           mean_brp_bmsy = mean(BMSY, na.rm=T, nan.rm=T), 
                           mean_brp_ssbmsy = mean(SSBMSY, na.rm=T, nan.rm=T), 
                           num_total = length(which(!is.na(TOTAL))), 
                           num_ssb = length(which(!is.na(SSB)))
                           )

# remove any time points that have no ssb and no total
    ts_sum1 <- subset(summary_table, !is.na(mean_ssb) | !is.na(mean_total) )
    # we loose 3 stocks
      nrow(summary_table) - nrow(ts_sum1)
      subset(summary_table, is.na(mean_ssb) & is.na(mean_total))
  # keep stocks that have a mean total number and a brp_bmsy
    bmsy_brp = subset(ts_sum1, mean_total>0 & mean_brp_bmsy>0)
  # remove that from stocks to check
    stocks_check <- subset(ts_sum1, !(ASSESSID %in% bmsy_brp$ASSESSID))
  # check to make sure all stocks accounted
    nrow(stocks_check)+nrow(bmsy_brp) == nrow(ts_sum1) # should be TRUE
    
  # now for remaining stocks, do they have biomass total and an admb estimate of collapse?
      bmsy_admb <- subset(stocks_check, mean_total > 0 & mean_admb_bmsy > 0)
    # remove these from stocks to check
      stchk <- subset(stocks_check, !(ASSESSID %in% bmsy_admb$ASSESSID))
    # check
      nrow(stchk) + nrow(bmsy_admb) == nrow(stocks_check) # should be TRUE

  # now for remaining stocks, do they have an ssb total and brp for ssb?
    ssb_have <- subset(stchk, mean_ssb > 0, mean_brp_ssbmsy > 0)
  # remove these from stocks to check
    stck1 <- subset(stchk, !(ASSESSID %in% ssb_have$ASSESSID))
  # check
    nrow(stck1) + nrow(ssb_have) == nrow(stchk) # should be TRUE

final_stocks <- subset(summary_table, !(ASSESSID %in% stck1$ASSESSID)) # this will go into stocks.csv. hold on for now. 


fish <- fish[order(fish$ASSESSID, fish$TSYEAR),]
str(fish)

# need to make sure fish.csv are all in the same units
units <- read.csv("original_data/timeseries_units.csv", stringsAsFactors=F)
units <- select(units, ASSESSID, SSB_UNIT, TOTAL_UNIT, CATCH_LANDINGS_UNIT)

fish <- merge(fish, units, by="ASSESSID", all.x = TRUE)

# for any units which have "E03" want to convert that to metric tons. will go through each one at a time
  # E03 is assumed to be thousands of metric tons. So will divide E03 units by 1000 to get all in metric tons
  # start with SSB
  ssb_changed = subset(units, SSB_UNIT=="E03")$ASSESSID
  fish$SSB_changed <- rep(NA, nrow(fish))
  fish$SSB_MT <- rep(NA, nrow(fish))
  fish$SSBMSY_MT <- rep(NA, nrow(fish))
  
  for(i in 1:length(ssb_changed)){
    # change time series value
    fish$SSB_MT[which(fish$ASSESSID==ssb_changed[i])] <- fish$SSB[which(fish$ASSESSID==ssb_changed[i])]/1000
    fish$SSBMSY_MT[which(fish$ASSESSID==ssb_changed[i])] <- fish$SSBMSY[which(fish$ASSESSID==ssb_changed[i])]/1000
    # confirm has been changed
    fish$SSB_changed[which(fish$ASSESSID==ssb_changed[i])] <- "yes"
  }


##################################################
# create stocks.csv with a row for each stock (n=331 rows).
#These will be our primary table that we perform stats on.

# add first year of time series for TOTAL and SSB
# will generate warnings because for some there are no entries for TOTAL or SSB. 
int_ts_TOTAL <- subset(fish, TOTAL > 0) # there needs to be a value for one of these
int_ts_SSB <- subset(fish, SSB > 0)
min_TOTAL <- ddply(int_ts, .(ASSESSID), summarize, firstYR_TOTAL = min(TSYEAR))
min_SSB <- ddply(int_ts, .(ASSESSID), summarize, firstYR_SSB = min(TSYEAR))

stocks <- merge(stocks, min_TOTAL, by="ASSESSID", all.x=T)
stocks <- merge(stocks, min_SSB, by="ASSESSID", all.x = T)



# constrain ADMB_BMSY to be twice maximum observered biomass
#-------THIS STILL NEEDS TO BE FIXED: needs to be constrained at 2(max observed biomass)*.1 rather than just 2(max obs. biomass)
# for stocks with a ADMB estimated, check to see if ADMB is more the 2x max observed biomass. 

# subset to just stocks with ADMB

admb_only <- subset(fish, admb_bmsy > 0)

# find maximum observed biomass, find the upper threshold, compare to admb estimate, return whether this bmsy needs to be constrained. 

eval_admb <- ddply(admb_only, .(ASSESSID), summarize, max_total = max(TOTAL, na.rm=T), upper_thresh = 2*max_total, admb_est = mean(admb_bmsy, na.rm=T), need_constraint = ifelse(admb_est > upper_thresh, "yes","no") )

cat(round(length(which(eval_admb$need_constraint=="yes"))/nrow(eval_admb),2)*100,"% of ADMB estimated stocks need to have B_msy constrained, or ",length(which(eval_admb$need_constraint=="yes"))," stocks",sep="")

# merge this into stock info
subset_constrain <- subset(eval_admb, need_constraint == "yes") # subset only upper thresholds if they are constraining
merge_subset <- select(subset_constrain, ASSESSID, upper_thresh)

stocks <- merge(stocks, merge_subset, by="ASSESSID",all.x=TRUE)

# need to merge in cmsy values
C_msy <- read.csv2(
  file="./CatchMSY/Output/CSV/RC_CMSY_Output.csv",
  header=T, sep = ";", dec=".", stringsAsFactors=F)
C_msy <- select(C_msy, stock, median.msy.)
colnames(C_msy) <- c("ASSESSID","C_msy")

stocks <- merge(stocks, C_msy, by="ASSESSID",all.x=TRUE)

# now evaluate whether stock has ever collapsed
# there are 4 possible thresholds for whether a stock has collapsed. will calculate for each
# there are: brp_bmsy; brp_ssbmsy; admb_bmsy; catch_bmsy
eval_collapse <- function (time_series, stock_info, metric){
  
  which_stocks = which(!is.na(stock_info[,metric])) # which stocks to evaluate
  num_stocks = length(which_stocks)
  collapsed = rep(NA, nrow(stock_info)) # make vector to record 0-1s
  
  for(i in 1:num_stocks){
    ind = which_stocks[i]
    sub_ts = subset(time_series, ASSESSID==stock_info$ASSESSID[ind])
    
    if(metric=="ADMB_BMSY"){
      cat("we're evaluating admb constraints\n")
      metric = ifelse(!is.na(stock_info$upper_thresh[ind]), "upper_thresh", metric)
    }
    
    collapsed[ind] = ifelse(any(sub_ts$TOTAL < .2*stock_info[,metric][ind],na.rm=T),1,0)
    metric = ifelse(metric=="upper_thresh", "ADMB_BMSY",metric) #re-setting so that test can be evaluated properly
  }
  if(length(which(!is.na(collapsed)))==length(which(!is.na(stock_info[,metric])))){
    cat("test passed!")
  return(collapsed)
  }
}

# brp_bmsy 
  stocks$collapse_brp_bmsy = eval_collapse(time_series = fish, stock_info = stocks, metric = "BRP_BMSY")

# brp_ssb 
  stocks$collapse_brp_ssbmsy = eval_collapse(time_series = fish, stock_info = stocks, metric = "BRP_SSBMSY")

# cmsy
  stocks$collapse_cmsy = eval_collapse(time_series = fish, stock_info = stocks, metric = "C_msy")

# admb_collapse 
  stocks$collapse_admb = eval_collapse(time_series = fish, stock_info = stocks, metric = "ADMB_BMSY")

TS_LENGTH <- vector(length= 0)
for (i in 1:length(stocks$ASSESSID)){
	z <- ifelse (stocks$NUM_TOTAL[i] >= stocks$NUM_SSB[i],stocks$NUM_TOTAL[i], stocks$NUM_SSB[i])
	TS_LENGTH <- append(TS_LENGTH, z)
}
stocks <- cbind(stocks,TS_LENGTH)
stocks <- droplevels(stocks)
#write.csv(stocks, file="data_tables/stocks.csv")

########################################################
# create LMEs.csv with a row for each Large Marine Ecosystem (n=31)

LMEs <- as.data.frame(levels(stocks$LME_NAME))
colnames(LMEs) = "LME_NAME"

#add LME number
LMEs <- merge(LMEs,refs, by="LME_NAME",all.x = TRUE, all.y=FALSE)


##add bounding box coordinates for each LME (assigned by NOAA)
require(maptools); require(rgdal); require(RColorBrewer)

# read in shapefile
shape <- readShapePoly("original_data/LME66/LME66.shp")
# assign projection
proj4string(shape) <- CRS("+proj=longlat")

# let's rename the data to be better, subset just to what we're interested in
attributes <- c("OBJECTID","LME_NUMBER","LME_NAME")
newNames <- c("number","area","name")
# subset
shape_subset <- shape[,attributes]
# rename
names(shape_subset) <- newNames

# assign to new .Rdata 
data_name <- "LMES_new"
assign(data_name, spTransform(shape_subset, CRS("+proj=longlat")))
#save(list=c(data_name),file="sandbox/LME_explore.Rdata") # have already saved, not necessary to do again

# extract the bounding box coordinates for each LME

xmin <- vector(length=0)
ymin <- vector(length=0)
xmax <- vector(length=0)
ymax <- vector(length=0)
LME_NUMBER <- vector(length=0)
for(i in 1:length(shape)){
  one_lme <- LMES_new[LMES_new$number == i, ]
  xmn <- one_lme@bbox[1]
  xmin <- append(xmin, xmn)
  ymn <- one_lme@bbox[2]
  ymin <- append(ymin, ymn)
  xmx <- one_lme@bbox[3]
  xmax <- append(xmax, xmx)
  ymx <- one_lme@bbox[4]
  ymax <- append(ymax, ymx)
  LME_NUMBER <- append(LME_NUMBER, i)
}

bboxes <- as.data.frame(cbind(LME_NUMBER,xmin,xmax, ymin, ymax))

LMEs <- merge(LMEs,bboxes, by = "LME_NUMBER", all.x=T, all.y=F)


#calculate centriod for each LME 
centroids <- as.data.frame(coordinates(shape))
names <- shape$LME_NUMBER
centroids <- cbind(centroids, names)
colnames(centroids) = c("x_centroid", "y_centroid", "LME_NUMBER")


LMEs <- merge(LMEs, centroids, by = "LME_NUMBER", all.x=T, all.y=F)

#add in High Seas info from FAO
FAO <- read.csv("original_data/FAO_areas/high_seasBB.csv")
FAO <- FAO[order(FAO$LME_NUMBER),]
FAO <- FAO[,c("x_min", "xmax","ymin","ymax", "centroid_x","centroid_y")]
LMEs[1:4,3:8] = FAO[1:4,1:6]

#synthesis: merge data from Transboundary Water Assessment Program (TWAP)
nuts <- read.table("original_data/Environmental_data/LME_NEWSRH2000_Nutrients_and_ICEP.csv", sep=";", header=T)

nuts <- nuts[,c(1,11:14)]
colnames(nuts) = c("LME_NUMBER", "N.P.molar.ratio","icep","icep_n","icep_p")

LMEs <- merge(LMEs,nuts, by="LME_NUMBER",all.x = TRUE, all.y=FALSE)

#write.csv(LMEs, file="data_tables/LMEs.csv")
