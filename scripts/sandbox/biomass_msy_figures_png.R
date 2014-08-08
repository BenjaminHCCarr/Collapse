#cat("Please set your working directory to the git folder "Regional_Collapse"")
setwd("~/Regional_Collapse/")

stocks <- read.csv("data_tables/stocks.csv", stringsAsFactors=F)
fish <- read.csv("data_tables/fish.csv", stringsAsFactors=F)

#subset only the species with working collapse estiamtes
st2 <-subset(stocks, !is.na(stocks$final_collapse))
#fsh2 <- merge(fish, st2, by = "ASSESSID", all.x=F, all.y=T)

#set your working directory to somewhere sensible before running this.
setwd("~/Regional_Collapse/sandbox/figures/png")

#output_pdf <- "biomass_msy_figures.pdf"
#pdf(output_pdf)

#i=128
#for(i in 204:224){
for(i in 1:length(st2$ASSESSID)) {
  output_png <- paste(st2$ASSESSID[i], ".png", sep="")
  png(output_png, width=7,height=4,units="in",res=128)
  a <- subset(fish,ASSESSID ==st2$ASSESSID[i])
  a <- subset(a, TSYEAR>=1945 & TSYEAR<3000) #bound years
  if(st2$final_collapse_metric[i]=="admb"){
    ylim1 = c(0,max(a$TOTAL,na.rm=TRUE)+(.1*max(a$TOTAL,na.rm=TRUE)))
    plot(a$TSYEAR, a$TOTAL, pch=19, 
         main=paste(st2$COMMON[i], 
                    "\n in the", st2$LME_NAME[i]), # "\n this is record number", i), 
         type="o", bty="l",
         ylim=ylim1, xlim=c(1945,2010), xlab="Year", ylab = "Biomass (mt)")
  }else if(st2$final_collapse_metric[i]=="BRP_SSBMSY"){
    ylim1 = c(0,max(a$SSB,na.rm=TRUE)+(.1*max(a$SSB,na.rm=TRUE)))
    plot(a$TSYEAR, a$SSB, pch=19, 
         main=paste(st2$COMMON[i], 
                    "\n in the", st2$LME_NAME[i]), # "\n this is record number", i), 
         type="o", bty="l",
         ylim=ylim1, xlim=c(1945,2010), xlab="Year", ylab = "Biomass (mt)")
  }else if(st2$final_collapse_metric[i]=="BRP_BMSY"){
    ylim1 = c(0,max(a$TOTAL,na.rm=TRUE)+(.1*max(a$TOTAL,na.rm=TRUE)))
    plot(a$TSYEAR, a$TOTAL, pch=19, 
         main=paste(st2$COMMON[i], 
                    "\n in the", st2$LME_NAME[i]), # "\n this is record number", i), 
         type="o", bty="l",
         ylim=ylim1, xlim=c(1945,2010), xlab="Year", ylab = "Biomass (mt)")
  }else{
    cat("\n NO SUITABLE BIOMASS FOUND \n")
  }
  lines(a$TSYEAR, a$CATCH_LANDINGS, xlab="", ylab="")
  if (!is.na(st2$final_threshVale[i])){
    abline(h=st2$final_threshVale[i]*5,col="red",lty=3)
    abline(h=st2$final_threshVale[i],col="red",lty=1)
  }
  legend("topright",
         c("Biomass","Catch","MSY","Collapse"), # puts text in the legend
         lty=c(1,1,3,1), # gives the legend appropriate symbols
         lwd=c(1.5,1,1),
         col=c("black","black","red","red"),
         cex=0.4) 
  dev.off() #return from printing to file
}