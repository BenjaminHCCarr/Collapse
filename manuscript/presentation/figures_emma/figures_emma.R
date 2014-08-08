### msy slide 3
################
N = 0:100
r = .7
K = 100
dN = r*N*(1-N/K)
plot(N,dN,type='l',lwd=3,bty="n", axes=F,ylim=c(0,20))
abline(h=0,lwd=2)
abline(v=0, lwd=2)
abline(v = K/2, lty=2)
n = 1
for(t in 1:20){
n[t+1] = n[t] + r*n[t]*(1-n[t]/K)
}

require(wesanderson)

paint = wes.palette(name= "Zissou", n = 4)

pdf(file="../presentation/figures_emma/slide3.pdf",width=8,height=6)
par(mfrow=c(1,2),mai=c(0.5,0.5,0,.25),xpd=FALSE,las=1, oma=c(.25, 1,0.2,0))

plot(n,type='o',pch=19,bty="n",xlab="",ylab="", axes=F,col=paint[1],lwd=4,cex=.65)
abline(h=0, col=paint[2],lwd=3)
abline(v=1, lwd=3, col=paint[2])
abline(h=K/2,lty=2,lwd=3,col=paint[4])
mtext("Time", 1, 0.5,col=paint[1])
mtext("N", 2, 1,col=paint[1])
mtext("K", 2, .25, at=K, col=paint[1],cex=.75)
mtext("0", 2, .25, at=0, col=paint[1],cex=.75)
#text(20, K/2+5, expression(over(K,2)))

plot(N,dN,type='l',ylim=c(0,20),bty="n", ylab="",axes=F, xlab="",col=paint[1],lwd=4)

abline(v=K/2, lty=2, col=paint[4],lwd=3)
abline(h=0, col=paint[2], lwd=3)
abline(v=0,col=paint[2],lwd=3)

mtext(expression(over(dN,dt)),2, line=0,col=paint[1])
mtext("N", 1, line=.5,col=paint[2])
mtext("0", 1, line=0, col=paint[2], at=0,cex=.75)
mtext("K", 1, line=0, col=paint[2], at=K,cex=.75)
text(K/2-2, 20, expression(B[msy] == over(K,2)),col=paint[1])
dev.off()
##############

#### species differences slide 12 
load("results/spp_random_BLUP.Rdata")
species <- BLUP2
sp <- as.data.frame(species[[1]])
colnames(sp) <- "species_effect"
sp$sci_name <- row.names(sp)
row.names(sp) <- NULL


# load stock data
require(plyr);require(dplyr)
stocks <- read.csv("data_tables/stocks.csv",stringsAsFactors=F)
common <- select(stocks, Scientific_name, COMMON)

sp_new <- merge(sp, common, by.x = "sci_name", by.y="Scientific_name", all.x=T,all.y=F)
sp_new <- sp_new[!duplicated(sp_new[,c(1:2)]),]

# how many stocks fall into each of these species?

#num_stocks <- ddply(stocks, .(Scientific_name), summarize, ns = length(unique(ASSESSID)))

#sp_newer <- merge(sp_new, num_stocks, by.x = "sci_name", by.y="Scientific_name", all.x=T,all.y=F)
sp_newer <- sp_new[order(sp_new$species_effect, decreasing=T),]

require(RColorBrewer)

#paint = colorRampPalette(brewer.pal(9, "YlGnBu"))(23)
#paint = rev(paint)

pdf(file="presentation/figures_emma/fig12.pdf",width=12,height=8)

  plot(sp_newer$species_effect, col="steelblue",pch=19,type="h", cex=1.75,bty="n",lwd=7, ylab="Random effect coefficient", xlab="",xaxt = "n",cex.lab=1.5)
  mtext("Species",1,.2, cex=1.5)
  
  text(12,2.2, "Atlantic cod")
  arrows(12,2.10, 2,1.9,lwd=2,length=.15)
  
  text(100, .5, "Pollock")
  arrows(104,.4, 110, 0.1, lwd=2, length=.15)

  text(30, 2.5, "South African west coast rock lobster")
  arrows(13, 2.5, 2, 2.4, lwd=2, length=.15)

  text(20, 1.5, "Atlantic Halibut")
  arrows(15, 1.42, 13, 1.2, lwd=2, length=.15)

  text(90, .5, "Hake")
  arrows(93, .44, 105,0.05, lwd=2, length=.15)
dev.off()
# highlight species of interest

require(rfishbase)
data(fishbase) #create fish.data file from fishbase
loadCache("../original_data/")

myfish <- findSpecies(sp_new$sci_name)
depth_sp <- getDepth(fish.data[myfish])
traits_sp <- getQuantTraits(fish.data[myfish])
size_sp <- getSize(fish.data[myfish],"age")
trophic_sp <- getTrophicLevel(fish.data[myfish]) # server down, can't get
climate_sp <- getEnviroClimateRange(fish.data[myfish]) # server down, can't get

# group by family name
require(stringr)
# make list of family names
sp_new$family_name = NA

for(i in 1:nrow(sp_new)){
  sp_new$family_name[i] = strsplit(sp_new$sci_name[i],split=" ")[[1]][1]
}

sp_new <- sp_new[order(sp_new$species_effect, decreasing=T),]
sp_new$family_num <- as.numeric(as.factor(sp_new$family_name))

sp_means <- ddply(sp_new, .(family_name), summarize, mean_sp = mean(species_effect), fam_num = mean(family_num), num_sp = length(unique(sci_name)))

sp_means <- sp_means[order(sp_means$num_sp, decreasing=T),]

paint = colorRampPalette(wes.palette(name= "Zissou", n = 5))(83)

plot(sp_new$species_effect, col=paint[sp_new$family_num], pch=19)
plot(sp_means$mean_sp,col=paint[sp_means$fam_num], pch=19)
