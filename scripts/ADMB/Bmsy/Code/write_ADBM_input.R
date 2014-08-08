# Goal: Write matrices of landings and biomass to a .dat file in plain text titled with stock ASSESSID

setwd("/Users/efuller/Documents/Projects/Regional_Collapse/")
require(dplyr)
# load data
timeseries <- read.csv("data_tables/fish.csv",stringsAsFactors=F)
colnames(timeseries) <- tolower(colnames(timeseries))
timeseries$x <- NULL

# can only use data in which we have the same units for catch and total
  timeseries <- subset(timeseries, units_total==units_catch)

df <- select(timeseries, assessid, total, catch_landings, tsyear)

# remove any rows that have NA in either total or catch_landings row
  df <- subset(df, !is.na(total) | !is.na(catch_landings))

# for only stocks that have total biomas and catch landings 
  # split by assessID
  s_as <- split(df, df$assessid)

  # find any that do not have 20 years of data
  ts_length <- lapply(s_as, function(x) nrow(x)>=20)

  # remove stocks that have fewer than 20 years
  long_ts <- s_as[unlist(ts_length)]

  # add column that does diff(year)
  for(i in 1:length(long_ts)){
    long_ts[[i]]$diff_yr <- c(diff(long_ts[[i]]$tsyear),1)
  }

  # make a vector that tells me whether or not years are going by
  reg_int <- rep(NA, length(long_ts))
  
  for(i in 1:length(long_ts)){
    reg_int[i]=all(long_ts[[i]]$diff_yr==1)
  }

  which(reg_int!=TRUE) # none, all these stocks should be good.
  
  # make a lookup table for shortened stock name because ADMB doesn't like long names
  names <- data.frame(assessid = names(long_ts),short_id=rep(NA, length(names(long_ts))))
  for(i in 1:length(long_ts)){
  names$short_id[i] <-paste(strsplit(as.character(long_ts[[i]]$assessid[1]),"-")[[1]][1],i,sep="")
  }
  # write out files with the number of rows, the maximum biomass, then the landings and biomass
  path <- "Bmsy/Input/"
  for(j in 1:length(long_ts)){
    file_name <- paste(path, names$short_id[j],".dat",sep="")
    fileConn<-file(file_name)
    writeLines(as.character(c(nrow(long_ts[[j]]),max(long_ts[[j]]$total))), fileConn)
    close(fileConn)
    
    mat <- t(rbind(long_ts[[j]]$catch_landings, long_ts[[j]]$total))
    write.table(mat, file=file_name,append=TRUE,col.names=FALSE, quote=FALSE, row.names=FALSE)
  }
  
  # write the lookup table 
  write.csv(names, file="Bmsy/Input/lookup_table.csv")
  
