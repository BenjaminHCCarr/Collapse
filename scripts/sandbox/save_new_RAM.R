###########################################################
#  Regional Collapse Code (NCEAS/RENCI OSS Workshop 2014) #
###########################################################
cat ("Please set your working directory to .../Regional_Collapse")
require(plyr); require(dplyr)

#read in timeseries_values_view.csv (time series of stock biomass)
data <- read.csv("original_data/timeseries_values_view.csv", header=T)
str(data)
length(levels(data$ASSESSID))

# read in reference_point_values_view.csv
reference <- read.csv("original_data/reference_point_value_view.csv", header=T,stringsAsFactors=F)
reference$BMSY <- as.numeric(reference$BMSY) # should introduce NAs
reference <- reference[,c(1:6,11,12)]
str(reference)

# add columns for "common name" (COMMON) and "area" (AREA)
fish <- merge(data,reference, by="ASSESSID",all.x = TRUE, all.y=FALSE)
str(fish)

# read in assessor.csv

assessor <- read.csv("original_data/assessor.csv", header=T)
str(assessor)
assessor = assessor[-35,] #RAM was making a big problem

# add a column for "management area" (MGMT)
grabmgmt = vector(length=length(data$ASSESSID))
for(i in 1:length(assessor$ASSESSORID))
{
  if(length(grep(assessor$ASSESSORID[i],data$ASSESSID))>0)
  {
    rows = grep(assessor$ASSESSORID[i],data$ASSESSID)
    grabmgmt[rows] = as.character(assessor $MGMT[i])
  }
}
str(grabmgmt)
MGMT <- as.factor(grabmgmt)
str(MGMT)
fish <- cbind(fish,MGMT)
str(fish)

# read in taxanomy.csv

taxa = read.csv("original_data/taxonomy.csv", header=T)
str(taxa)

# add a column for "latin name" (LATINNAME)
grabIDs = vector(length=length(data$ASSESSID))
for(i in 1:length(taxa$SCIENTIFICNAME))
{
  if(length(grep(taxa$MYERSNAME[i],data$ASSESSID))>0)
  {
    rows = grep(taxa$MYERSNAME[i],data$ASSESSID)
    grabIDs[rows] = as.character(taxa$SCIENTIFICNAME[i])
  }
}
LATINNAME <- as.factor(grabIDs)
str(LATINNAME)
fish <- cbind(fish,LATINNAME)
str(fish)

# add ADMB estimated brps
admb <- read.csv("ADMB/BMSY.csv", stringsAsFactors=F)
# remain bmsy so we don't get confused
admb$X <- NULL
admb$short_id <- NULL
colnames(admb) <- c("ASSESSID","admb_bmsy")

fish <- merge(fish, admb, by="ASSESSID", all.x=TRUE)

# want stocks that have brps, or at least 20 years of data in total biomass
summary_table <- ddply(fish, .(ASSESSID), summarize, 
                       mean_ssb = mean(SSB, na.rm=T), 
                       mean_total = mean(TOTAL, na.rm=T), 
                       mean_catch = mean(CATCH_LANDINGS, na.rm=T), 
                       mean_admb_bmsy = mean(admb_bmsy, na.rm=T), 
                       mean_brp_bmsy = mean(BMSY, na.rm=T), 
                       mean_brp_ssbmsy = mean(SSBMSY, na.rm=T), 
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


##############Decided this wasn't necessary#########
# Remove all stocks that have no TOTAL ever
#id <- levels(fish$ASSESSID)
#USE = vector(length = 0)
#USE2 = vector(length = length(fish$ASSESSID))
#for(i in 1:length(id))
#    {
#   a <- subset(fish,ASSESSID == id[i])
#    if(length(unique(a$TOTAL)) == 1)  {USE = append(USE,"NO")}
#    else {USE = append(USE,"YES")}
#    }
#stocks = as.data.frame(cbind(id,USE))
#for(i in 1:length(fish$ASSESSID))
#{
#    for(j in 1:length(id))
#    {
#        if(USE[j] == "NO" && id[j] == fish$ASSESSID[i])
#        {
#            USE2[i] = 1
#        }
#    }
#}
#fish = cbind(fish,USE2)
#fish = subset(fish,USE2 == 0)
#fish = droplevels(fish)

attach(fish)
fish <- fish[order(ASSESSID, TSYEAR),]
detach(fish)
str(fish)

#write.csv(fish, file="data_tables/fish.csv",row.names=T)

##########################################################
# Make the figures
#par(mfrow=c(4,1))
#par(mar = c(2,2,1,0)+0.1)
#time series for each stock
#id <- levels(fish$ASSESSID)
#for(i in 1:length(id))
#   {
#	a <- subset(fish,ASSESSID ==id[i])
#	ylim1 = c(0,max(a$TOTAL,na.rm=TRUE))
#	plot(a$TSYEAR, a$TOTAL, pch=19, main=id[i], type="o", ylim
#ylim1,xlim=c(1900,2010), bty="l")
#    if (length(unique(a$SSB))==1 & length(unique(a$TOTAL))==1) {cat #("EMPTY SSB",i,id[i],"\n")} else {points(a$TSYEAR, a$SSB, type="o", #lty=2)}
#}

##########################################
# summary table for each species - how many areas and management regions is each species measured in?
spp <- levels(fish$COMMON)
n.areas <- vector(length=0)
n.mgmt <- vector(length=0)
for(i in 1:length(spp)) { 
  a <- subset(fish,COMMON ==spp[i])
  a <- droplevels(a)
  areas <- length(levels(a$AREA))
  mgmts <- length(levels(a$MGMT))
  n.areas <-append(n.areas,areas) 
  n.mgmt <-append(n.mgmt,mgmts) 
}
length(n.areas)
length(n.mgmt)
summary1 <- as.data.frame(cbind(spp,n.areas,n.mgmt))
summary1

# summary table for each area - how many spp measured in each area?
areas <- levels(fish$AREA)
n.spp <- vector(length=0)
for(i in 1:length(areas)) { 
  a <- subset(fish, AREA ==areas[i])
  a <- droplevels(a)
  spps <- length(levels(a$COMMON))
  n.spp <-append(n.spp,spps) 
}
length(n.spp)
summary2 <- as.data.frame(cbind(areas,n.spp))
summary2


# summary table for each management region - how many areas and spp per mgmt region?
mgmts <- levels(fish$MGMT)
n.areas <- vector(length=0)
n.spp <- vector(length=0)
for(i in 1:length(mgmts)) { 
  a <- subset(fish, MGMT ==mgmts[i])
  a <- droplevels(a)
  spps <- length(levels(a$COMMON))
  n.spp <-append(n.spp,spps) 
  areas <- length(levels(a$AREA))
  n.areas <-append(n.areas,areas) 
}
length(n.spp)
length(n.areas)
summary3 <- as.data.frame(cbind(mgmts,n.areas,n.spp))
summary3




#########################################################
# create stocks.csv with a row for each stock (n=331 rows).
#These will be our primary table that we perform stats on.

#create a vector of all 331 stocks
stocks <- summary_table[,c("ASSESSID","mean_admb_bmsy","mean_brp_bmsy","mean_brp_ssbmsy","num_total","num_ssb")]
colnames(stocks) <- c("ASSESSID","ADMB_BMSY","BRP_BMSY","BRP_SSBMSY","NUM_TOTAL","NUM_SSB")

#assign an LME to each stock using RAM_Legacy_database_summary.csv

lmes <- read.csv("original_data/RAM_Legacy_database_summary.csv", header=T)
str(lmes)

stocks <- merge(stocks, lmes[,c(1,3:4)], by="ASSESSID", all.x=T, all.y=F)
str(stocks)
stocks <- merge(stocks,reference[,c("ASSESSID","UMSY","FMSY")], by="ASSESSID",all.x = TRUE, all.y=FALSE)
str(stocks)

#add LME_NUMBER from lmerefs.csv
refs <- read.csv("original_data/lmerefs.csv", header=T)
str(refs)
stocks <- merge(stocks,refs, by="LME_NAME",all.x = TRUE, all.y=FALSE)
str(stocks)

#add a column for the number of LMEs each stock has been assigned to (ranges from promary (1 LME) to quintary (5 LMEs)).

lmetostocks <- read.csv("original_data/lmetostocks.csv", header=T)
str(lmetostocks)
length(levels(lmetostocks$STOCKID))

n.lmes <- tapply(lmetostocks$STOCKTOLMERELATION, lmetostocks$STOCKID, length)
the_stock <- as.data.frame(rownames(n.lmes))
n.lmes <- as.vector(as.numeric(n.lmes))
oi <- cbind(the_stock, n.lmes)
colnames(oi) = c("STOCKID", "LME_COUNT")
str(oi)

#merge that into stocks file by searching for STOCKID in ASSESSID
grab <- vector(length=length(stocks$ASSESSID))
for(i in 1:length(oi$STOCKID))
{
  if(length(grep(oi$STOCKID[i], stocks$ASSESSID))>0)
  {
    rows = grep(oi$STOCKID[i], stocks$ASSESSID)
    grab[rows] = as.numeric(oi$LME_COUNT[i])
  }
}
str(grab)
LME_COUNT <- grab
stocks = (cbind(stocks, LME_COUNT))
str(stocks)

stocks$final_analysis <- rep(0, nrow(stocks))
stocks$final_analysis[which(as.character(stocks$ASSESSID) %in% as.character(final_stocks$ASSESSID))] = 1

# constrain ADMB_BMSY to be twice maximum observered biomass

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

# remove duplicate randomly -- if no duplicates, won't affect anything
C_msy <- C_msy[-which(duplicated(C_msy$ASSESSID)),]

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

write.csv(stocks, file="data_tables/stocks.csv")

########################################################
# create LMEs.csv with a row for each Large Marine Ecosystem (n=32)

LMEs <- as.data.frame(levels(stocks$LME_NAME))
colnames(LMEs) = "LME_NAME"
str(LMEs)

LMEs <- merge(LMEs,refs, by="LME_NAME",all.x = TRUE, all.y=FALSE)
str(LMEs)

#some more summary tables:
# summary table for each LME - how many areas and spp per LME?

lmes <- levels(stocks$LME_NAME)
n.areas <- vector(length=0)
n.spp <- vector(length=0)
for(i in 1:length(lmes)) { 
  a <- subset(stocks, LME_NAME ==lmes[i])
  a <- droplevels(a)
  spps <- length(levels(a$COMMON))
  n.spp <-append(n.spp,spps) 
  areas <- length(levels(a$AREA))
  n.areas <-append(n.areas,areas)
  print (paste("###########",lmes[i],"############"))
  print(levels(a$AREA)) 
}
length(n.spp)
length(n.areas)
summary3 <- as.data.frame(cbind(lmes,n.areas,n.spp))
summary3
sum(n.areas)
levels(a$AREA)


#add bounding box coordinates for each LME (assigned by NOAA)
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
str(bboxes)

LMEs <- merge(LMEs,bboxes, by = "LME_NUMBER", all.x=T, all.y=F)


#calculate centriod for each LME 
centroids <- as.data.frame(coordinates(shape))
str(centroids)
names <- shape$LME_NUMBER
centroids <- cbind(centroids, names)
colnames(centroids) = c("x_centroid", "y_centroid", "LME_NUMBER")
str(centroids)

LMEs <- merge(LMEs, centroids, by = "LME_NUMBER", all.x=T, all.y=F)

#add in High Seas info from FAO
FAO <- read.csv("original_data/FAO_areas/high_seas_BB.csv")

str(FAO)
LMEs[1:5,3:6] = FAO[1:5,5:8]

#synthesis: merge data from Transboundary Water Assessment Program (TWAP)
nuts <- read.table("original_data/Environmental_data/LME_NEWSRH2000_Nutrients_and_ICEP.csv", sep=";", header=T)

nuts <- nuts[,c(1,11:14)]
colnames(nuts) = c("LME_NUMBER", "N.P.molar.ratio","icep","icep_n","icep_p")
str(nuts)
head(nuts)
LMEs <- merge(LMEs,nuts, by="LME_NUMBER",all.x = TRUE, all.y=FALSE)

#write.csv(LMEs, file="data_tables/LMEs.csv")


