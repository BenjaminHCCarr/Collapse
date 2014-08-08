setwd("~/Regional_Collapse")
require(plyr);require(dplyr); require(reshape2)

########################
# load time series data
tms <- read.csv2(
  file="./original_data/timeseries_values_view.csv",
  header=T, sep = ",", dec=".",stringsAsFactors=F)

# load catch MSY data
cmsy_data <- read.csv2(
  file="./CatchMSY/Output/CSV/RC_CMSY_Output.csv",
  header=T, sep = ";", dec=".", stringsAsFactors=F)

# load biomass MSY data
bmsy_data <- read.csv(
  file = "data_tables/stocks.csv", header=T, stringsAsFactors=F, sep = ",")
bmsy_data$X.1 <- NULL
bmsy_data$X <- NULL

# merge cmsy thresholds with bmsy 
sub_cmsy <- select(cmsy_data, stock, median.msy.)

names(sub_cmsy) <- c("ASSESSID","C_msy")
sub_cmsy$C_msy <- sub_cmsy$C_msy* 1000
merged_df <- merge(bmsy_data, sub_cmsy, by= "ASSESSID", all.x=TRUE, all.y=FALSE)

# total number of stocks
num_stocks <- nrow(merged_df)

# make dataframe to record 0-1 for collapse, number of years collapsed 
msy_collapse<- data.frame(matrix(NA, nrow=num_stocks, ncol=5))
colnames(msy_collapse)<-c(
  "assessid", "if_collapse_cmsy","nyr_collapse_cmsy", 
  "if_collapse_bmsy", "nyr_collapse_bmsy")
msy_collapse$assessid <- unique(tms$ASSESSID)

##################################
# collapses

# write function to provide stock biomass threshold for collapse depending on 
# whether we want from the catch or biomass msy. 
find_collapse_threshold <- function(stock_id, type, msy_df){
  # inputs: stock_id = assessID 
  #       : msy_df = data frame that contains msy values
  #       : type = whether we use "catch_msy" or "biomass_msy", 
  if(type=="catch_msy"){
    msy_value <- msy_df$C_msy[which(msy_df$ASSESSID==stock_id)]* .2
    return(msy_value)
  }
  if(type=="biomass_msy"){
    msy_value <- msy_df$B_msy[which(msy_df$ASSESSID==stock_id)] * .2
    return(msy_value)
  }
  if(type!="catch_msy" & type!="biomass_msy"){
    warning("the type of msy you put in is incorrect. it needs to be either 'biomass_msy' or 'catch_msy'")
  }
}


identify_collapse <- function(type, df, tms, stock_id){
  # check for collapse: pseudo code
  #   make a time series of 0-1 if each year is below the collapse threshold
  #   count how many years collapsed total
  #   determine if collapse happens twice in a row
  
  # type= looking for bmsy or cmsy collapse; num_stocks the number of stocks to loop through
  # the appropriate dataframe for cmsy or bmsy; tms = time series from Ram Myers database
  
  # subset stock info
  tmp_tms <- subset(tms, ASSESSID %in% stock_id)
  collapse_thresh <- find_collapse_threshold(stock_id, type, df)
  
  # add column for timeseries of collapse
  tmp_tms$collapse <- 0
  
  # add 1s if TOTAL < collapse_thresh
  tmp_tms$collapse[tmp_tms$TOTAL<collapse_thresh] <- 1
  
  # any collapsed years twice in a row?
  two_years = any(diff(cumsum(tmp_tms$collapse),lag=2)==2)
  # if yes, at a 1 else 0
  if_collapse <- ifelse(two_years==TRUE, 1, 0)
  
  # record total number of years collapsed
  nyear_collapse <- sum(tmp_tms$collapse)   
  
  return(list(if_collapse, nyear_collapse))
}

df = merged_df

for(i in 1:num_stocks){
  type = "catch_msy"
  msy_collapse[i,2:3] <- identify_collapse(type, df, tms, msy_collapse$assessid[i])
  type = "biomass_msy"
  msy_collapse[i,4:5] <- identify_collapse(type, df, tms, msy_collapse$assessid[i])
}

# merge collapse information to stock data_table

total_df <- merge(merged_df, msy_collapse, by.x = "ASSESSID", by.y="assessid")

## some graphing -- extra, should go to sandbox probably. 
meh <- ddply(total_df, .(LME_NAME), summarize, cmsy_collapsed = sum(if_collapse_cmsy)/length(if_collapse_cmsy),
             bmsy_collapsed=sum(if_collapse_bmsy)/length(if_collapse_bmsy))

barplot_df <- t(meh[,2:3])
colnames(barplot_df) <- meh[,1]
barplot(barplot_df, bor=F, beside = F, las=2)

write.csv(total_df, file="sandbox/cmsy_collapse.csv", row.names=FALSE)

tmptmp <- data.frame(merged_df$ASSESSID, merged_df$B_msy, merged_df$C_msy)
colnames(tmptmp) <- c("ASSESSID","B_msy","C_msy")
plot(tmptmp$B_msy,tmptmp$C_msy,xlab="Catch MSY",ylab="Biomass MSY")
hist(tmptmp$C_msy,xlab="Catch MSY", main=NULL, breaks=20)
hist(tmptmp$B_msy,xlab="Biomass MSY", main=NULL, breaks=20)

C_msy_max <- max(tmptmp$C_msy, na.rm=TRUE)
B_msy_may <- max(tmptmp$B_msy, na.rm=TRUE)

B_msy_40cmsy <- subset(tmptmp, B_msy > (40*C_msy))
B_msy_400cmsy <- subset(tmptmp, B_msy > (400*C_msy))

hist(B_msy_40cmsy$C_msy,xlab="Biomass MSY greater than 40x Cmsy", main=NULL, breaks=20)
hist(B_msy_400cmsy$C_msy,xlab="Biomass MSY greater than 400x Cmsy", main=NULL, breaks=20)

plot(B_msy_40cmsy$C_msy,B_msy_40cmsy$B_msy,xlab="Catch MSY",
     ylab="Biomass MSY", main="Biomass MSY greater than 40x Cmsy")
plot(B_msy_400cmsy$C_msy,B_msy_400cmsy$B_msy,xlab="Catch MSY",
     ylab="Biomass MSY", main="Biomass MSY greater than 400x Cmsy")
