set.seed(999)  ## for same random sequence
##require(hacks)

## Read Data for stock, year=yr, catch=ct, and resilience=res. Expects space delimited file with header yr  ct and years in integer and catch in real with decimal point
## For example
## stock	res	 yr     ct       
## cap-icel	Medium 1984  1234.32 

setwd('~/OSS2014/Regional_Collapse/CatchMSY/Output')

logfile  <- file("RC_CMSY_data2_log.txt")
sink(logfile, append=TRUE)
sink(logfile, append=TRUE, type="message")

filename <- "../Input/RC_CMSY_data2.csv"
outfile  <- "RC_CMSY_data2_Output.csv"
cdat <- read.csv2(filename, header=T, sep = ",", dec=".")
cat("\n", "File", filename, "read successfully","\n")

stock_id <- unique(as.character(cdat$stock)) 
## stock_id <- "cod-2224" ## for selecting individual stocks

## Loop through stocks
for(stock in stock_id) {
	yr   <- cdat$yr[as.character(cdat$stock)==stock]
	ct   <- as.numeric(cdat$ct[as.character(cdat$stock)==stock])/1000  ## assumes that catch is given in tonnes, transforms to '000 tonnes
	res  <- unique(as.character(cdat$res[as.character(cdat$stock)==stock])) ## resilience from FishBase, if needed, enable in PARAMETER SECTION
	nyr  <- length(yr)    ## number of years in the time series
	
cat("\n","Stock",stock,"\n")
flush.console()
		
## PARAMETER SECTION

## If resilience is to be used, delete ## in rows 1-4 below and set ## in row 5	below
start_r  <- if(res == "Very low"){c(0.015, 0.1)}
            else if(res == "Low") {c(0.05,0.5)}
            else if(res == "High") {c(0.6,1.5)}
            else {c(0.2,1)} ## Medium, or default if no res is found	
## start_r     <- c(0.5,1.5)  ## disable this line if you use resilience
start_k     <- c(max(ct),50*max(ct)) ## default for upper k e.g. 100 * max catch
## startbio 	<- c(0.8,1)   ## assumed biomass range at start of time series, as fraction of k
startbio    <- if(ct[1]/max(ct) < 0.5) {c(0.5,0.9)} else {c(0.3,0.6)} ## use for batch processing
interyr 	<- yr[2]   ## interim year within time series for which biomass estimate is available; set to yr[2] if no estimates are available
interbio 	<- c(0, 1) ## biomass range for interim year, as fraction of k; set to 0 and 1 if not available
## finalbio 	<- c(0.8, 0.9) ## biomass range after last catches, as fraction of k
finalbio    <- if(ct[nyr]/max(ct) > 0.5) {c(0.3,0.7)} else {c(0.01,0.4)} ## use for batch processing
n           <- 100000  ## number of iterations, e.g. 100000
sigR        <- 0.0      ## process error; 0 if deterministic model; 0.05 reasonable value? 0.2 is too high

startbt     <- seq(startbio[1], startbio[2], by = 0.05) ## apply range of start biomass in steps of 0.05	
parbound <- list(r = start_r, k = start_k, lambda = finalbio, sigR)

cat("Last year =",max(yr),", last catch =",1000*ct[nyr],"\n")
cat("Resilience =",res,"\n")
cat("Process error =", sigR,"\n")
cat("Assumed initial biomass (B/k) =", startbio[1],"-", startbio[2], " k","\n")
cat("Assumed intermediate biomass (B/k) in", interyr, " =", interbio[1],"-",interbio[2]," k","\n")
cat("Assumed final biomass (B/k) =", parbound$lambda[1],"-",parbound$lambda[2]," k","\n")
cat("Initial bounds for r =", parbound$r[1], "-", parbound$r[2],"\n")
cat("Initial bounds for k =", format(1000*parbound$k[1], digits=3), "-", format(1000*parbound$k[2],digits=3),"\n")

flush.console()
		
	
## FUNCTIONS
.schaefer	<- function(theta)
{
	with(as.list(theta), {  ## for all combinations of ri & ki
		bt=vector()
		ell = 0  ## initialize ell
		for (j in startbt)
		{
			if(ell == 0) 
			{
				bt[1]=j*k*exp(rnorm(1,0, sigR))  ## set biomass in first year
				for(i in 1:nyr) ## for all years in the time series
				{
					xt=rnorm(1,0, sigR)
					bt[i+1]=(bt[i]+r*bt[i]*(1-bt[i]/k)-ct[i])*exp(xt) ## calculate biomass as function of previous year's biomass plus net production minus catch
				}
		
				#Bernoulli likelihood, assign 0 or 1 to each combination of r and k
				ell = 0
				if(bt[nyr+1]/k>=lam1 && bt[nyr+1]/k <=lam2 && min(bt) > 0 && max(bt) <=k && bt[which(yr==interyr)]/k>=interbio[1] && bt[which(yr==interyr)]/k<=interbio[2]) 
				ell = 1
			}	
		}
		return(list(ell=ell))
		
		
	})
}

sraMSY	<-function(theta, N)
{
	#This function conducts the stock reduction
	#analysis for N trials
	#args:
	#	theta - a list object containing:
	#		r (lower and upper bounds for r)
	#		k (lower and upper bounds for k)
	#		lambda (limits for current depletion)
	
	
	with(as.list(theta), 
	{
		ri = exp(runif(N, log(r[1]), log(r[2])))  ## get N values between r[1] and r[2], assign to ri
		ki = exp(runif(N, log(k[1]), log(k[2])))  ## get N values between k[1] and k[2], assing to ki
		itheta=cbind(r=ri,k=ki, lam1=lambda[1],lam2=lambda[2], sigR=sigR) ## assign ri, ki, and final biomass range to itheta
		M = apply(itheta,1,.schaefer) ## call Schaefer function with parameters in itheta
		i=1:N
		## prototype objective function
		get.ell=function(i) M[[i]]$ell
		ell = sapply(i, get.ell) 
		return(list(r=ri,k=ki, ell=ell))	
	})
}

## MAIN
R1 = sraMSY(parbound, n)  
	
	
## Get statistics on r, k, MSY and determine new bounds for r and k
r1 	<- R1$r[R1$ell==1]
k1 	<- R1$k[R1$ell==1]
msy1  <- r1*k1/4
mean_msy1 <- exp(mean(log(msy1))) 
max_k1a  <- min(k1[r1<1.1*parbound$r[1]]) ## smallest k1 near initial lower bound of r
max_k1b  <- max(k1[r1*k1/4<mean_msy1]) ## largest k1 that gives mean MSY
max_k1 <- if(max_k1a < max_k1b) {max_k1a} else {max_k1b}

if(length(r1)<10) {
cat("Too few (", length(r1), ") possible r-k combinations, check input parameters","\n")
flush.console()
}

if(length(r1)>=10) {

	## set new upper bound of r to 1.2 max r1
	parbound$r[2] <- 1.2*max(r1)
	## set new lower bound for k to 0.9 min k1 and upper bound to max_k1 
	parbound$k 	  <- c(0.9 * min(k1), max_k1)

	
	cat("First MSY =", format(1000*mean_msy1, digits=3),"\n")
	cat("First r =", format(exp(mean(log(r1))), digits=3),"\n")
	cat("New upper bound for r =", format(parbound$r[2],digits=2),"\n")	
	cat("New range for k =", format(1000*parbound$k[1], digits=3), "-", format(1000*parbound$k[2],digits=3),"\n")


## Repeat analysis with new r-k bounds
R1 = sraMSY(parbound, n)

## Get statistics on r, k and msy
r = R1$r[R1$ell==1]
k = R1$k[R1$ell==1]
msy = r * k / 4
mean_ln_msy = mean(log(msy))

	## plot MSY over catch data
  
	output_pdf <- paste(stock, "_Catch_MSY", ".pdf", sep="")
	pdf(output_pdf)
  
	par(mfcol=c(2,3))
	plot(yr, ct, type="l", ylim = c(0, max(ct)), xlab = "Year", ylab = "Catch (1000 t)", main = stock)
	abline(h=exp(mean(log(msy))),col="red", lwd=2)
	abline(h=exp(mean_ln_msy - 2 * sd(log(msy))),col="red")
	abline(h=exp(mean_ln_msy + 2 * sd(log(msy))),col="red")
		
	hist(r, freq=F, xlim=c(0, 1.2 * max(r)), main = "")
	abline(v=exp(mean(log(r))),col="red",lwd=2)
	abline(v=exp(mean(log(r))-2*sd(log(r))),col="red")
	abline(v=exp(mean(log(r))+2*sd(log(r))),col="red")
	
	plot(r1, k1, xlim = start_r, ylim = start_k, xlab="r", ylab="k (1000t)")
	
	hist(k, freq=F, xlim=c(0, 1.2 * max(k)), xlab="k (1000t)", main = "")
	abline(v=exp(mean(log(k))),col="red", lwd=2)	
	abline(v=exp(mean(log(k))-2*sd(log(k))),col="red")
	abline(v=exp(mean(log(k))+2*sd(log(k))),col="red")

	plot(log(r), log(k),xlab="ln(r)",ylab="ln(k)")
	abline(v=mean(log(r)))
	abline(h=mean(log(k)))
	abline(mean(log(msy))+log(4),-1, col="red",lwd=2)
	abline(mean(log(msy))-2*sd(log(msy))+log(4),-1, col="red")
	abline(mean(log(msy))+2*sd(log(msy))+log(4),-1, col="red")

	hist(msy, freq=F, xlim=c(0, 1.2 * max(msy)), xlab="MSY (1000t)",main = "")
	abline(v=exp(mean(log(msy))),col="red", lwd=2)
	abline(v=exp(mean_ln_msy - 2 * sd(log(msy))),col="red")
	abline(v=exp(mean_ln_msy + 2 * sd(log(msy))),col="red")
	
	dev.off() #return from printing to file
	
	cat("Possible combinations = ", length(r),"\n")
	cat("geom. mean r =", format(exp(mean(log(r))),digits=3), "\n")
	cat("r +/- 2 SD =", format(exp(mean(log(r))-2*sd(log(r))),digits=3),"-",format(exp(mean(log(r))+2*sd(log(r))),digits=3), "\n")
	cat("geom. mean k =", format(1000*exp(mean(log(k))),digits=3), "\n")
	cat("k +/- 2 SD =", format(1000*exp(mean(log(k))-2*sd(log(k))),digits=3),"-",format(1000*exp(mean(log(k))+2*sd(log(k))),digits=3), "\n")
	cat("geom. mean MSY =", format(1000*exp(mean(log(msy))),digits=3),"\n")
	cat("MSY +/- 2 SD =", format(1000*exp(mean_ln_msy - 2 * sd(log(msy))),digits=3), "-", format(1000*exp(mean_ln_msy + 2 * sd(log(msy))),digits=3), "\n")

## Write results into outfile, in append mode (no header in file, existing files will be continued)
output = data.frame(stock, sigR, startbio[1], startbio[2], interbio[1], interbio[2], finalbio[1], finalbio[2], min(yr), max(yr), res, max(ct), ct[1], ct[nyr], length(r), exp(mean(log(r))), sd(log(r)), min(r), quantile(r,0.05), quantile(r,0.25), median(r), quantile(r,0.75), quantile(r,0.95), max(r), exp(mean(log(k))), sd(log(k)), min(k), quantile(k, 0.05), quantile(k, 0.25), median(k), quantile(k, 0.75), quantile(k, 0.95), max(k), exp(mean(log(msy))), sd(log(msy)), min(msy), quantile(msy, 0.05), quantile(msy, 0.25), median(msy), quantile(msy, 0.75), quantile(msy, 0.95), max(msy)) 

write.table(output, file = outfile, append = TRUE, sep = ";", dec = ".", row.names = FALSE, col.names = FALSE)

}
}  ## End of stock loop, get next stock or exit
sink()
sink(type=message)
