#cat("Please set your working directory to the git folder "Regional_Collapse"")
setwd("~/Regional_Collapse/")
# read data
#fish <- read.csv("data_tables/fish.csv", stringsAsFactors=F)
fish <- read.csv("original_data/timeseries_values_view.csv", stringsAsFactors=F)
stocks <- read.csv("data_tables/stocks.csv", stringsAsFactors=F)
# should re-do with stocks that are useable. 

#st2 <-subset(stocks, !is.na(stocks$final_collapse))
st2<-stocks
fsh2 <- merge(fish, st2, by = "ASSESSID", all.x=F, all.y=T)

require(plyr)

min_year <- ddply(fsh2, .(ASSESSID), summarize , min_year = min(TSYEAR))
# order by min_year
min_year <- min_year[order(min_year$min_year),]
num_year <- ddply(min_year, .(min_year), summarize, num_est = length(ASSESSID))
num_year$cumulative = cumsum(num_year$num_est)/sum(num_year$num_est)

#set your working directory to somewhere sensible before running this.
setwd("~/Regional_Collapse/sandbox/figures/png")

output_png <- paste("StocksRelative", ".png", sep="")
#png(output_png, width=7,height=4,units="in",res=128)
plot(num_year$min_year,num_year$cumulative,
     main="Relative Number of Stocks Being Harvested",
     xlab="Year", ylab="Proportion of Stocks",
     xlim=c(1800,2000), 
     type='l',lwd=2, bty="n",
     width=7,height=4,units="in",res=128)
abline(v = 1920, col="green",lty=2,lwd=2)
text(1920+5, 0.5, srt=90, col="green", cex=0.8,
     "Introduction of Steam Trawlers")
abline(v = 1945, col="indianred",lty=2,lwd=2)
text(1945+5, 0.5, srt=90, col="indianred", cex=0.8,
     "End of WWII")
abline(v = 1970, col="purple",lty=2,lwd=2)
text(1970+5, 0.5, srt=90, col="purple", cex=0.8,
     "Start of 200nm EEZs")
#abline(h=0.5)
dev.off()

output_png <- paste("StocksHistogram", ".png", sep="")
png(output_png, width=7,height=4,units="in",res=128)
hist(min_year$min_year,2000,breaks=60,
     col="steelblue",bor=F, freq=T,
     xlab="Year",main="First Year Stock was Reported as Exploited or Surveyed",
     width=7,height=4,units="in",res=128)
abline(v = 1920, col="green",lty=2,lwd=2)
text(1920-5, 20, srt=90, col="green", cex=0.8,
     "Introduction of Steam Trawlers")
abline(v = 1945, col="indianred",lty=2,lwd=2)
text(1945-5, 20, srt=90, col="indianred", cex=0.8,
     "End of WWII")
abline(v = 1970, col="purple",lty=2,lwd=2)
text(1970-5, 20, srt=90, col="purple", cex=0.8,
     "Start of 200nm EEZs")
dev.off()
