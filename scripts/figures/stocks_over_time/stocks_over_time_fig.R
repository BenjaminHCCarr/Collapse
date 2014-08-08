# read data
fish <- read.csv("data_tables/fish.csv", stringsAsFactors=F)
stocks <- read.csv("data_tables/stocks2.csv", stringsAsFactors=F)
# should re-do with stocks that are useable. 

# subset time series to those that are useable
useable <- stocks$ASSESSID[which(stocks$final_analysis==1)]
ts_use <- subset(fish, ASSESSID %in% useable)

require(plyr)

min_year <- ddply(fish, .(ASSESSID), summarize , min_year = min(TSYEAR))
# order by min_year
min_year <- min_year[order(min_year$min_year),]

num_year <- ddply(min_year, .(min_year), summarize, num_est = length(ASSESSID))

num_year$cumulative = cumsum(num_year$num_est)/sum(num_year$num_est)
plot(num_year$min_year,num_year$cumulative,type='l',lwd=2, bty="n")


abline(v = 1945, col="indianred",lty=2,lwd=2)
abline(v = 1940, col="steelblue",lty=2,lwd=2)
abline(v = 1920, col = "green", lty=2, lwd=2)
abline(v = 1970, col = "purple", lty=2, lwd=2)

hist(min_year$min_year,breaks=60,col="steelblue",bor=F, xlab="Year",main="",freq=T)
abline(v = 1946, lty=2, lwd=4, col="indianred")
abline(v = 1970, lty=2, lwd=4, col="red")
