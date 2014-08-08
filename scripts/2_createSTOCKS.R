# this merges the estimated parameters from ADMB and CMSY, returns the final stocks.csv

#Please set your working directory to .../Regional_Collapse"
rm(list=ls())
if(basename(getwd())!="Regional_Collapse"){cat("Plz change your working directory. It should be 'Regional_Collapse'")}
require(plyr); require(dplyr)

# define warning function
check_rows <- function(stocks){
  if(nrow(stocks)==330){
    cat("All good, 330 rows")}else{
      warning("Something's wrong, there should be 330 rows in stocks")} 
}

# load intermediate stocks
stocks <- read.csv("data_tables/intermediate_stocks.csv", stringsAsFactors = F)

# load timeseries
fish <- read.csv('data_tables/fish.csv', stringsAsFactors=F)

#######################
# add admb bmsy
#####################
  admb <- read.csv("ADMB/ADMBrescale/BMSY.csv", stringsAsFactors=F)
  admb <- select(admb, ASSESSID, BMSY)
  colnames(admb) <- c("ASSESSID", "ADMB_BMSY")
  stocks <- merge(stocks, admb, by = "ASSESSID", all.x=T)
  check_rows(stocks)
######################
# add in Cmsy
##################
C_msy <- read.csv2(file="CatchMSY/Output/CSV/RC_CMSY_Output.csv", sep=";",stringsAsFactors=F)
C_msy <- select(C_msy, stock, median.msy.)
colnames(C_msy) <- c("ASSESSID","C_msy")
C_msy$C_msy <- as.numeric(C_msy$C_msy)

stocks <- merge(stocks, C_msy, by="ASSESSID", all.x=T)
check_rows(stocks)
#################
# add a column that constrains ADMB
###############


# for each of these stocks, will constrain admb estimate of K to be no more than 2 observed biomass
# so need to find twice max biomass. The estimated bmsy is half K. If the estimated k is greater than 2
# times max biomass, then need to not use that but instead a bmsy taht is max biomass/2. 
# need to find which admb_bmsy*2 => 2*max biomass. Or admb_bmsy > max biomass. 
# and remove those so that max biomass is the bmsy. 
# then take .2 of either admb_bmsy or max biomass for collapse threshold. 
constrained_bmsy <- ddply(fish, .(ASSESSID), summarize, constrained = max(TOTAL, na.rm = T))

stocks <- merge(stocks, constrained_bmsy, by="ASSESSID", all.x = T)
# take whichever is lower, either admb estimate or constrained value
stocks$admb_constrained <- ifelse(stocks$constrained<=stocks$ADMB_BMSY, 
                                  stocks$constrained, stocks$ADMB_BMSY)

# but if estimate is less than 1 MT, then do constraint
stocks$admb_constrained <- ifelse(stocks$ADMB_BMSY<1, stocks$constrained, stocks$admb_constrained)

# if there is no value for 
###############
# evaluate collapse for brp_bmsy
################
# now evaluate whether stock has ever collapsed

# for a given stock, has the time series gone below the threshold for any of 
# provided thresholds
eval_collapse <- function(stockID, time_series,stocks){
  # subset time series
    sub_ts <- subset(time_series, ASSESSID==stockID & TSYEAR > 1944) # start in 1945
    sub_ts$X <- NULL
  # pull out collapse thresholds
    collapse_vals <- stocks[stocks$ASSESSID==stockID, 
                          c("BRP_BMSY","BRP_SSBMSY","C_msy","admb_constrained")]
  
  # check for each value
    sub_ts$collapse_BRP_BMSY <- sub_ts$TOTAL < .2 * collapse_vals$BRP_BMSY
    sub_ts$collapse_BRP_SSBMSY <- sub_ts$SSB < .2 * collapse_vals$BRP_SSBMSY
    sub_ts$collapse_admb <- sub_ts$TOTAL < .2 * collapse_vals$admb_constrained
    sub_ts$collapse_Cmsy <- sub_ts$TOTAL < .2 * collapse_vals$C_msy
  
  # change to 0-1 to count
    sub_ts[sub_ts==TRUE] <- 1
    sub_ts[sub_ts==FALSE] <- 0
  
  # remove NAs because they propogate in cumsum()
    sub_ts[is.na(sub_ts)] <- 0
  
  # count number of times in a row each happens
    sub_ts$cBRP_BMSY <- c(NA, NA, diff(cumsum(sub_ts$collapse_BRP_BMSY),lag=2))
    sub_ts$cBRP_SSBMSY <- c(NA, NA, diff(cumsum(sub_ts$collapse_BRP_SSBMSY), lag=2))
    sub_ts$ccollapse_admb <- c(NA, NA,diff(cumsum(sub_ts$collapse_admb),lag=2))
    sub_ts$cC_msy <- c(NA,NA,diff(cumsum(sub_ts$collapse_Cmsy),lag=2))
  
  # see if any collapsed
    did_collapse <- collapse_vals
    did_collapse$BRP_BMSY <- ifelse(any(sub_ts$cBRP_BMSY>1,na.rm=T),1,0)
    did_collapse$BRP_SSBMSY <- ifelse(any(sub_ts$cBRP_SSBMSY>1, na.rm=T),1,0)
    did_collapse$admb_constrained <- ifelse(any(sub_ts$ccollapse_admb>1,na.rm=T), 1,0)
    did_collapse$C_msy <- ifelse(any(sub_ts$cC_msy > 1, na.rm=T),1,0)
  
  # first year for each collapse, if any
    y1_BRP_BMSY <- ifelse(did_collapse$BRP_BMSY==1
      ,min(sub_ts$TSYEAR[which(sub_ts$cBRP_BMSY==2)],na.rm=T)-1,NA)
    y1_BRP_SSBMSY <- ifelse(did_collapse$BRP_SSBMSY==1,
                            min(sub_ts$TSYEAR[which(sub_ts$cBRP_SSBMSY==2)], na.rm=T)-1,NA)
    y1_admb <- ifelse(did_collapse$admb_constrained==1,
                      min(sub_ts$TSYEAR[which(sub_ts$ccollapse_admb==2)], na.rm=T)-1,NA)
    y1_cC_msy <- ifelse(did_collapse$C_msy==1,
                        min(sub_ts$TSYEAR[which(sub_ts$cC_msy==2)], na.rm=T)-1,NA)
  
  years <- data.frame(y1_BRP_BMSY, y1_BRP_SSBMSY, y1_admb, y1_cC_msy)
  
  did_collapse <- cbind(did_collapse,years)
  
  # status of stock in final step
  
  # if didn't collapse, what's that last year
  
  final_BRP_BMSY <- ifelse(did_collapse$BRP_BMSY==0,max(sub_ts$TSYEAR)-1,NA)
  final_BRP_SSBMSY <- ifelse(did_collapse$BRP_SSBMSY==0,max(sub_ts$TSYEAR)-1,NA)
  final_admb <- ifelse(did_collapse$admb_constrained==0,max(sub_ts$TSYEAR)-1,NA)
  final_Cmsy <- ifelse(did_collapse$C_msy==0,max(sub_ts$TSYEAR)-1,NA)
  
  final_year <- data.frame(final_BRP_BMSY,final_BRP_SSBMSY,final_admb, final_Cmsy)
  did_collapse <- cbind(did_collapse, final_year)

  return(did_collapse)
}

# set up dataframe
  collapse_df <- data.frame(ASSESSID=stocks$ASSESSID)
  collapse_df$collapse_BRP_BMSY_collapse <- NA
  collapse_df$collapseBRP_SSBMSY_collapse <- NA
  collapse_df$collapse_C_msy <- NA
  collapse_df$collapse_admb_collapse <- NA
  collapse_df$y1_BRP_BMSY <- NA
  collapse_df$y1_BRP_SSBMSY <- NA
  collapse_df$y1_admb <- NA
  collapse_df$y1_cC_msy <- NA
  collapse_df$final_BRP_BMSY <-NA
  collapse_df$final_BRP_SSBMSY <- NA
  collapse_df$final_admb <- NA
  collapse_df$final_Cmsy <- NA

# go through stocks
  for(i in 1:nrow(collapse_df)){
    collapse_df[i,2:13] <- eval_collapse(collapse_df$ASSESSID[i], fish, stocks)
  }

# merge with stocks
  stocks <- merge(stocks, collapse_df, by = "ASSESSID", all.x = T)
######
# prioritize collapse: BRP_SSB exists, then BRP_BMSY, then ADMB
######
  stocks$final_collapse <- NA
  stocks$final_collapse_metric <- NA
  stocks$final_threshVale <- NA

  inds1 = which(!is.na(stocks$BRP_SSBMSY) & !is.na(stocks$firstYR_SSB))
  stocks$final_collapse[inds1] <- stocks$collapseBRP_SSBMSY_collapse[inds1]
  stocks$final_collapse_metric[inds1] <- "BRP_SSBMSY"
  stocks$final_threshVale[inds1] <- stocks$BRP_SSBMSY[inds1]*.2

  # if NA still in final_collapse, and BRP_BMSY is not NA, then put in collapse
  inds2 = which(is.na(stocks$final_collapse) & !is.na(stocks$BRP_BMSY) & !is.na(stocks$firstYR_TOTAL))
  stocks$final_collapse[inds2] <- stocks$collapse_BRP_BMSY_collapse[inds2]
  stocks$final_collapse_metric[inds2] <- "BRP_BMSY"
  stocks$final_threshVale[inds2] <- stocks$BRP_BMSY[inds2]*.2

  # if NA still in final_collapse, then us ADMB
  inds3 = which(is.na(stocks$final_collapse) & !is.na(stocks$admb_constrained) & !is.na(stocks$firstYR_TOTAL))
  stocks$final_collapse[inds3] <- stocks$collapse_admb_collapse[inds3]
  stocks$final_collapse_metric[inds3] <- "admb"
  stocks$final_threshVale[inds3] <- stocks$admb_constrained[inds3]*.2

#######
# find year stock 'ended'
################
# for those with stocks based on SSB BRP
last_year <- rep(NA,nrow(stocks))
  
  ind = which(stocks$final_collapse==0 & stocks$final_collapse_metric=="BRP_SSBMSY")
  last_year[ind] <- stocks$final_BRP_SSBMSY[ind]
  
  ind = which(stocks$final_collapse==1 & stocks$final_collapse_metric=="BRP_SSBMSY")
  last_year[ind] <- stocks$y1_BRP_SSBMSY[ind]

# for those with stocks based on BMSY BRP
  ind = which(stocks$final_collapse==0 & stocks$final_collapse_metric=="BRP_BMSY")
  last_year[ind] <- stocks$final_BRP_BMSY[ind]
  
  ind = which(stocks$final_collapse==1 & stocks$final_collapse_metric=="BRP_BMSY")
  last_year[ind] <- stocks$y1_BRP_BMSY[ind]

# for thsoe with stocks based on ADMB
  ind = which(stocks$final_collapse==0 & stocks$final_collapse_metric=="admb")
  last_year[ind] <- stocks$final_admb[ind]
  
  ind = which(stocks$final_collapse==1 & stocks$final_collapse_metric=="admb")
  last_year[ind] <- stocks$y1_admb[ind]

stocks$last_year <- last_year

### need to know which year to use for start
first_year <- rep(NA, length=nrow(stocks))

  ind1 = which(stocks$final_collapse_metric=="BRP_SSBMSY")
  first_year[ind1] <- stocks$firstYR_SSB[ind1]
    
  ind2 = which(stocks$final_collapse_metric=="BRP_BMSY")
  first_year[ind2] = stocks$firstYR_TOTAL[ind2]

  ind3 = which(stocks$final_collapse_metric=="admb")
  first_year[ind3] = stocks$firstYR_TOTAL[ind3]

stocks$first_year <- first_year




#####
stocks_final <- select(stocks, ASSESSID, COMMON, Scientific_name, MGMT, AREA, LME_NAME, LME_NUMBER, NUM_LMEs, final_collapse, final_collapse_metric, first_year, last_year, final_threshVale, mean_ut,median_ut,q90_ut,mean_umsy,median_umsy,q90umsy)
###############
# write csv
#############
write.csv(stocks_final, "data_tables/stocks.csv",row.names=F)
# should change to stocks.csv when sure it works. 