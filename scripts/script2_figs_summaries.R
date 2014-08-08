#2. Make the figures & summary tables

cat("Please set your working directory to the git folder "Regional_Collapse"")
LMEs <- read.csv("data_tables/stocks.csv")
stocks <- read.csv("data_tables/stocks.csv")
fish <- read.csv("data_tables/fish.csv")

#First, plot time series of TOTAL BIOMASS for each stock that was measured this way (n=288 stocks):
st2 <- subset(stocks, NUM_TOTAL>0)
fsh2 <- merge(fish, st2, by = "ASSESSID", all.x=F, all.y=T)

#set your working directory to somewhere sensible before running this.
for(i in 1:length(st2$ASSESSID)) 
   {
	a <- subset(fish,ASSESSID ==st2$ASSESSID[i])
	ylim1 = c(0,max(a$TOTAL,na.rm=TRUE))
	pdf(file= paste(st2$LME_NAME[i],st2$SCIENTIFICNAME[i],".pdf"), width = 7, height=5)
	plot(a$TSYEAR, a$TOTAL, pch=19, main=paste(st2$SCIENTIFICNAME[i],"in the", st2$LME_NAME[i]), type="o", ylim=ylim1, xlim=c(1900,2010), bty="l",xlab="", ylab = "Total biomass")
    if (length(unique(a$SSB))==1 & length(unique(a$TOTAL))==1) {cat ("EMPTY SSB",i,id[i],"\n")} else {points(a$TSYEAR, a$SSB, type="o", lty=2)}
    abline(h=st2$ADMB_BMSY[i], lty=3, lwd=2)
    dev.off()
}

#Then, plot time series of SSB BIOMASS for each stock that does not have TOTAL BIOMASS reported (n=):
st2 <- subset(stocks, NUM_TOTAL>0)
fsh2 <- merge(fish, st2, by = "ASSESSID", all.x=F, all.y=T)
for(i in 1:length(st2$ASSESSID)) 
   {
	a <- subset(fish,ASSESSID ==st2$ASSESSID[i])
	ylim1 = c(0,max(a$TOTAL,na.rm=TRUE))
	pdf(file= paste(st2$LME_NAME[i],st2$SCIENTIFICNAME[i],".pdf"), width = 7, height=2.5)
	plot(a$TSYEAR, a$TOTAL, pch=19, main=paste(st2$SCIENTIFICNAME[i],"in the", st2$LME_NAME[i]), type="o", ylim=ylim1, xlim=c(1900,2010), bty="l")
    if (length(unique(a$SSB))==1 & length(unique(a$TOTAL))==1) {cat ("EMPTY SSB",i,id[i],"\n")} else {points(a$TSYEAR, a$SSB, type="o", lty=2)}
    abline(h=st2$ADMB_BMSY[i], lty=3)
    dev.off()
}

# summary table for each species - how many LMEs is each species measured in?
#first, merge LME into fish.csv
str(fishes)
str(stocks)
fishes <- merge(fish,stocks[,c(1,2,4,7)],by="ASSESSID", all.x=T, all.y=F)

spp <- levels(fishes$Scientific_name)
n.lmes <- vector(length=0)
for(i in 1:length(spp)) { 
	a <- subset(fishes,Scientific_name ==spp[i])
	a <- droplevels(a)
	lmes <- length(unique(a$LME_NUMBER))
	n.lmes <-append(n.lmes,lmes)  
	}
length(n.lmes)
summary1 <- as.data.frame(cbind(spp,n.lmes))
summary1
hist(n.lmes, breaks=20)

# summary table for each management region - how many areas and spp per mgmt region?
mgmts <- levels(fish$MGMT)
n.areas <- vector(length=0)
n.spp <- vector(length=0)
n.stocks <- vector(length=0)
for(i in 1:length(mgmts)) { 
	a <- subset(fish, MGMT ==mgmts[i])
	a <- droplevels(a)
	spps <- length(unique(a$LATINNAME))
	n.spp <-append(n.spp,spps) 
	areas <- length(unique(a$AREA))
	n.areas <-append(n.areas,areas) 
	stks <- length(unique(a$ASSESSID))
	n.stocks <-append(n.stocks,stks) 
	}
summary2 <- as.data.frame(cbind(mgmts,n.areas,n.spp, n.stocks))
summary2

# summary table for each LME - how many stocks and spp per LME?
lmes <- levels(stocks$LME_NAME)
n.stocks <- vector(length=0)
n.spp <- vector(length=0)
for(i in 1:length(lmes)) { 
	a <- subset(stocks, LME_NAME ==lmes[i])
	a <- droplevels(a)
	spps <- length(unique(a$SCIENTIFICNAME))
	n.spp <-append(n.spp,spps) 
	stks <- length(levels(a$ASSESSID))
	n.stocks <-append(n.stocks,stks)
		}
#length(n.spp)
summary4 <- as.data.frame(cbind(lmes,n.spp, n.stocks))
summary4


