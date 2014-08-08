#############################################
# 4. Analyses
# Code will run independently if started here.  First, make sure all data tables are up-to-date in Regional_Collapse/data_tables.

#Please set your working directory to .../Regional_Collapse"
rm(list=ls())
if(basename(getwd())!="Regional_Collapse"){cat("Plz change your working directory. It should be 'Regional_Collapse'")}

require(lme4);  require(dplyr)

LMEs <- read.csv("data_tables/LMEs.csv")
stocks <- read.csv("data_tables/stocks.csv")
part <- LMEs[,c(2, 8:9)]
#add the LME centroid to each stock:
stocks <- merge(stocks, part, by = "LME_NUMBER", all.x=T, all.y=F)

#remove stocks with no 'collapse' value
stocks = stocks[-which(is.na(stocks$final_collapse)),]
stocks <- transform(stocks, H_LENGTH = stocks$last_year-stocks$first_year+1)

#Remove High Seas Ecosystems?
stocks <- subset(stocks, LME_NUMBER>0)
stocks<- droplevels(stocks)
str(stocks) #262 stocks; 25 LMEs; 121 spp
summary(stocks)

#Merging in environmental and human covariates from Ingrid's file.
Ingrid <- read.csv("original_data/Environmental_data/Environmental_variables_LMES.csv")
str(Ingrid)
Ingrid <- select(Ingrid, LME_NAME, LME_NUMBER, Socioeconomic_Index_.HDI., Fish_species_richness, SST_Change_...C._1982.2006 , Chl.a.avg, DO_avg, Count_Density_total_.counts.km2.)
colnames(Ingrid) = c("LME_NAME", "LME_NUMBER", "SOCIO", "RICHNESS", "deltaSST", "ChlA", "DO", "PLASTICS")
Ingrid <- transform(Ingrid, SOCIO=as.numeric(SOCIO), RICHNESS=as.numeric(RICHNESS), deltaSST = as.numeric(deltaSST), DO = as.numeric(DO))
stocks<- merge(stocks, Ingrid, by = "LME_NUMBER", all.x=T, all.y=F)
stocks<- droplevels(stocks)
#examine univariate distributions
hist(stocks$SOCIO) #outliers on the left tail... why?
hist(stocks$RICHNESS)
hist(stocks$deltaSST)
hist(stocks$ChlA) ###highly right skewed
stocks <- transform(stocks, ln_Chl = log(ChlA))
hist(stocks$ln_Chl)  #better
hist(stocks$DO)
hist(stocks$PLASTICS) #highly right skewed
stocks <- transform(stocks, ln_PLASTICS = log(PLASTICS))
hist(stocks$ln_PLASTICS) #better
hist(stocks$q90_ut, breaks=22) #highly right skewed
hist(log(stocks$q90_ut)) #better
stocks<- transform(stocks, ln_q90 = log(q90_ut))
str(stocks)
pairs(stocks[,c(24:26, 28, 30:32)])
#DO and ln_CHl appear correlated.  Check it out:
cor.test(stocks$DO,stocks$ln_Chl) #r = 0.78.  Remove DO.

#rescale predictor variables
stocks <- transform(stocks, cSOCIO=scale(SOCIO), cRICHNESS= scale(RICHNESS), cdeltaSST = scale(deltaSST), cln_PLASTICS = scale(ln_PLASTICS), cln_Chl = scale(ln_Chl), cln_q90 = scale(ln_q90) )
summary(stocks)

#TEST ASSUMPTIONS

#1. Do different LMEs and fish spp have different probabilitites of collapse? (using lmer and Laplace approximation)
#First, use LRT to find best random effects structure with the full set of fixed effects included:
#(this is a glmm, and no option to explicitly include spatial autocorrelation in this package).
m1 <- glmer(final_collapse ~ cSOCIO + cRICHNESS + cdeltaSST + cln_PLASTICS + cln_Chl + (1|Scientific_name), family="binomial", data=stocks) 
m2 <- glm(final_collapse ~ cSOCIO + cRICHNESS + cdeltaSST + cln_PLASTICS + cln_Chl, family="binomial", data=stocks)
 #Are all speces and LMEs the same?
AIC(m1, m2)
#anova(m1, m2)   How would I do a LRT if Edgar would let me?
plot(residuals(m1))
summary(m1)

#calculate Moran's I to test for spatial autocorrelation of resids:
require(ape)
#create the distance matrix
xy <- rbind(stocks$x_centroid, stocks$y_centroid)
xy <- t(xy)
dista = as.matrix(dist(xy,diag = T,upper = T))
#run some Moran's I tests
res1 <- residuals(m1)
Moran.I(res1,dista)# #go to #3 because no spatial autocorrelation (p=0.34)

BLUP <- ranef(m1)
#Harvesting pressure
#redo this process with resids from model 3... need to subset stocks to get correct dim on coordinates matrix:
short = stocks[-which(is.na(stocks$ln_q90)),] #n=243
xy <- rbind(short$x_centroid, short$y_centroid)
xy <- t(xy)
distb = as.matrix(dist(xy,diag = T,upper = T))
#run some Moran's I tests
res3 <- residuals(m3)
Moran.I(res3,distb)# #go to #3 because no spatial autocorrelation (p=0.85)
BLUP2 <- ranef(m3)
save(BLUP2, file="results/spp_random_BLUP.Rdata")

#2. If spatial-autocorrelation is present:
#glmmPQL with distance matrix that represents spatial autocorrelation - unfortunatley PQL may be biased for binary errors.
#I am liking this idea less and less because time series length has to be a linear predictor...

#3.  If spatial auto-correlation not present
#Hierarchical 'nest survival' model using JAGS
#based on Andy Royle's code
#Must install program JAGS locally
library(coda); library(rjags); library(R2jags)

#make an integer number for each stock (will be random effect, lke 'plot' in Andy's example)
spp <- levels(stocks$Scientific_name)
spp_code <- seq(1,length(spp),1)
spp1 <- as.data.frame(cbind(spp, spp_code))
stocks <- merge(stocks, spp1, by.x = "Scientific_name", by.y = "spp", all.x=T, all.y=F)

# This code creates the H table of the nest survival model
collapse = as.numeric(stocks$final_collapse)
y_start = as.numeric(stocks$first_year)
y_end = as.numeric(stocks$last_year)
year_first = 1945
year_last = 2009 #change to 2009

years = seq(year_first,year_last,1)
table = matrix(nrow = length(stocks$ASSESSID),ncol = length(years))

for(i in 1:length(stocks$ASSESSID))
    for(j in 1:length(years))
        {
        if(collapse[i] == 0)
            {
            if(years[j] < y_start[i])
                table[i,j] = NA
            if(years[j] >= y_start[i] && years[j] <= y_end[i])
                table[i,j] = 1
            if(years[j] > y_end[i])
                table[i,j] = NA
            }
        if(collapse[i] == 1)
            {
            if(years[j] < y_start[i])
                table[i,j] = NA
            if(years[j] >= y_start[i] && years[j] < y_end[i])
                table[i,j] = 1
            if(years[j] == y_end[i])
                table[i,j] = 0
            if(years[j] > y_end[i])
                table[i,j] = NA
            }
        }
head(table)
tail(table)

#Building standardized YEAR and YEAR2 matrix
time <- seq(1,year_last-year_first+1,1)
time = (time-mean(time))/sd(time)
time2 <- time^2
TIME <- rep(time, length(levels(stocks$ASSESSID)))
TIME2 <- rep(time2, length(levels(stocks$ASSESSID)))

# i = n.stocks (vector of 262)
	#matrix is 262 x 65

#the data
y<-table       #matrix 
first<-stocks$first_year-(year_first-1)  # vector first seen 
last<-stocks$last_year - (year_first-1)	#vector year of last check (either the year of collapse or the last year of the time series if the stock never collapsed.)
n.stocks<-length(levels(stocks$ASSESSID))     #number
TIME<- matrix(TIME, nrow=n.stocks, ncol=length(time),byrow=T)  #matrix 
TIME2<-matrix(TIME2, nrow=n.stocks, ncol=length(time),byrow=T)  #matrix
cln_Chl <- stocks$cln_Chl #vector
SPP_CODE<-stocks$spp_code	 #vector (like xplot)
cdeltaSST <- stocks$cdeltaSST
cSOCIO <- stocks$cSOCIO
cRICHNESS <- stocks$cRICHNESS
cln_PLASTICS <- stocks$cln_PLASTICS
length(levels(stocks$Scientific_name)) #121 random effect (spp) levels


#model 1 -
data <- list ("y","first","last","TIME","TIME2","n.stocks","cln_Chl","SPP_CODE")
inits <- function(){
  list (beta0=rnorm(1,0,.2),beta1=rnorm(1,0,.2) ,yeareff=rnorm(1,0,.2),yeareff2=rnorm(1,0,.2),sigmaplot=runif(1,1,2),alpha=rnorm(121))
}

parameters <- c("beta0","beta1","yeareff","yeareff2","sigmaplot")

#the model
model1 <- jags(model.file="Rscripts/JAGS_model1.txt", data=data, inits=inits, n.chains=3, n.burnin=1000, n.iter=4000, parameters.to.save = parameters)

model1.mcmc <-as.mcmc(model1)
xyplot(model1.mcmc)
densityplot(model1.mcmc)
print(model1)


#Raftery diagostic to calculate required burnin
raftery.diag (model1.mcmc)
gelman.diag(model1.mcmc)
write(model1.mcmc, file="results/model1.txt")
class(model1.mcmc)
str(model1.mcmc)

# if the model does not converge, update it!
model1.1 <-update(model1, n.iter=16000)
print(model1.1)
model1.1.mcmc <-as.mcmc(model1.1)
xyplot(model1.1.mcmc)
densityplot(model1.1.mcmc)
gelman.diag(model1.1.mcmc)
#output parameters:
model1.1_mcmcoutput <- model1.1.mcmc[[1]]
model1.1_mcmcoutput <- as.data.frame(model1.1_mcmcoutput)
write.csv(model1.1_mcmcoutput, file = "results/model1/model1.1_mcmcoutput")
str(model1.1_mcmcoutput)

# if the model does not converge, update it again
model1.2 <-update(model1.1, n.iter=20000)
print(model1.2)
model1.2.mcmc <-as.mcmc(model1.2)
xyplot(model1.2.mcmc)
densityplot(model1.2.mcmc)
gelman.diag(model1.2.mcmc)
#output parameters
model1.2_mcmcoutput <- model1.2.mcmc[[1]]
model1.2_mcmcoutput <- as.data.frame(model1.2_mcmcoutput)
write.csv(model1.2_mcmcoutput, file = "results/model1/model1.2_mcmcoutput")

#model 2 -
data <- list ("y","first","last","TIME","TIME2","n.stocks","cln_Chl","SPP_CODE",  "cdeltaSST", "cSOCIO", "cRICHNESS", "cln_PLASTICS")
inits <- function(){
    list (beta0=rnorm(1,0,.2),beta1=rnorm(1,0,.2) ,beta2=rnorm(1,0,.2),beta3=rnorm(1,0,.2),beta4=rnorm(1,0,.2),beta5=rnorm(1,0,.2),yeareff=rnorm(1,0,.2),yeareff2=rnorm(1,0,.2),sigmaplot=runif(1,1,2),alpha=rnorm(121))
}

parameters <- c("beta0","beta1","beta2","beta3","beta4","beta5","yeareff","yeareff2","sigmaplot")

#the model
model2 <- jags(model.file="Rscripts/JAGS_model2.txt", data=data, inits=inits, n.chains=3, n.burnin=1000, n.iter=5000, parameters.to.save = parameters)

model2.mcmc <-as.mcmc(model2)
xyplot(model2.mcmc)
densityplot(model2.mcmc)
print(model2)
#Raftery diagostic to calculate required burnin
raftery.diag (model2.mcmc)


# if the model does not converge, update it!
model2.1 <-update(model2, n.iter=2000)
print(model2.1)
model2.1.mcmc <-as.mcmc(model2.1)
xyplot(model2.1.mcmc)
densityplot(model2.1.mcmc)

#############
#Redo with only stocks that have harvesting pressure data (dataframe=short)
#model 3 -
short = stocks[-which(is.na(stocks$ln_q90)),] 
str(short)
short = droplevels(short)

spp <- levels(short$Scientific_name)
spp_code <- seq(1,length(spp),1)
spp1 <- as.data.frame(cbind(spp, spp_code))
short <- merge(short, spp1, by.x = "Scientific_name", by.y = "spp", all.x=T, all.y=F)

# This code creates the H table of the nest survival model
collapse = as.numeric(short$final_collapse)
y_start = as.numeric(short$first_year)
y_end = as.numeric(short $last_year)
year_first = 1945
year_last = 2009 #change to 2009

years = seq(year_first,year_last,1)
table = matrix(nrow = length(short $ASSESSID),ncol = length(years))

for(i in 1:length(short $ASSESSID))
    for(j in 1:length(years))
        {
        if(collapse[i] == 0)
            {
            if(years[j] < y_start[i])
                table[i,j] = NA
            if(years[j] >= y_start[i] && years[j] <= y_end[i])
                table[i,j] = 1
            if(years[j] > y_end[i])
                table[i,j] = NA
            }
        if(collapse[i] == 1)
            {
            if(years[j] < y_start[i])
                table[i,j] = NA
            if(years[j] >= y_start[i] && years[j] < y_end[i])
                table[i,j] = 1
            if(years[j] == y_end[i])
                table[i,j] = 0
            if(years[j] > y_end[i])
                table[i,j] = NA
            }
        }
head(table)
tail(table)
table[3:12,14:23]

#the data
y<-table       #matrix 
first<-short$first_year-(year_first-1)  # vector first seen 
last<-short $last_year - (year_first-1)	#vector year of last check (either the year of collapse or the last year of the time series if the stock never collapsed.)
n.stocks<-length(levels(short $ASSESSID))     #number
TIME<- matrix(TIME, nrow=n.stocks, ncol=length(time),byrow=T)  #matrix 
TIME2<-matrix(TIME2, nrow=n.stocks, ncol=length(time),byrow=T)  #matrix
cln_Chl <- short $cln_Chl #vector
SPP_CODE<-short $spp_code	 #vector (like xplot)
cdeltaSST <- short $cdeltaSST
cSOCIO <- short $cSOCIO
cRICHNESS <- short $cRICHNESS
cln_PLASTICS <- short $cln_PLASTICS
cln_q90 <- short$cln_q90
length(levels(short $Scientific_name)) #112 random effect (spp) levels
length(unique(short$LME_NUMBER))

#Building standardized YEAR and YEAR2 matrix
time <- seq(1,year_last-year_first+1,1)
time = (time-mean(time))/sd(time)
time2 <- time^2
TIME <- rep(time, length(levels(short$ASSESSID)))
TIME2 <- rep(time2, length(levels(short$ASSESSID)))

# i = n.stocks (vector of 243)
	#matrix is 243 x 65

#model 3
data <- list ("y","first","last","TIME","n.stocks","cln_Chl","cln_q90", "SPP_CODE")
inits <- function(){
  list (beta0=rnorm(1,0,.2),beta5=rnorm(1,0,.2) ,beta6=rnorm(1,0,.2), yeareff=rnorm(1,0,.2),sigmaplot=runif(1,1,2),alpha=rnorm(112))
}

parameters <- c("beta0","beta5","beta6","yeareff","sigmaplot")

#the model
model3 <- jags(model.file="Rscripts/JAGS_model3.txt", data=data, inits=inits, n.chains=3, n.burnin=1000, n.iter=20000, parameters.to.save = parameters)

model3.mcmc <-as.mcmc(model3)
xyplot(model3.mcmc)
densityplot(model3.mcmc)
print(model3)

model3_mcmcoutput <- model3.mcmc[[1]]
model3_mcmcoutput <- as.data.frame(model3_mcmcoutput)
write.csv(model3_mcmcoutput, file = "results/model1/model3.1_mcmcoutput")

#Raftery diagostic to calculate required burnin
raftery.diag (model3.mcmc)
gelman.diag(model3.mcmc)

# if the model does not converge, update it!
model3.1 <-update(model3, n.iter=16000)
print(model3.1)
model3.1.mcmc <-as.mcmc(model3.1)
xyplot(model3.1.mcmc)
densityplot(model3.1.mcmc)
gelman.diag(model3.1.mcmc)
#output parameters:
model3.1_mcmcoutput <- model3.1.mcmc[[1]]
model3.1_mcmcoutput <- as.data.frame(model3.1_mcmcoutput)
write.csv(model3.1_mcmcoutput, file = "results/model1/model3.1_mcmcoutput")
str(model3.1_mcmcoutput)


#model 4 -
data <- list ("y","first","last","TIME","TIME2","n.stocks","cln_Chl","SPP_CODE",  "cdeltaSST", "cSOCIO", "cRICHNESS", "cln_PLASTICS", "cln_q90")
inits <- function(){
    list (beta0=rnorm(1,0,.2),beta1=rnorm(1,0,.2) ,beta2=rnorm(1,0,.2),beta3=rnorm(1,0,.2),beta4=rnorm(1,0,.2),beta5=rnorm(1,0,.2),beta6=rnorm(1,0,.2),yeareff=rnorm(1,0,.2),yeareff2=rnorm(1,0,.2),sigmaplot=runif(1,1,2),alpha=rnorm(112))
}

parameters <- c("beta0","beta1","beta2","beta3","beta4","beta5","beta6","yeareff","yeareff2","sigmaplot")

#the model
model4 <- jags(model.file="Rscripts/JAGS_model4.txt", data=data, inits=inits, n.chains=3, n.burnin=10, n.iter=100, parameters.to.save = parameters)

model4.mcmc <-as.mcmc(model4)
xyplot(model4.mcmc)
densityplot(model4.mcmc)
print(model4)
#Raftery diagostic to calculate required burnin
raftery.diag (model4.mcmc)


# if the model does not converge, update it!
model4.1 <-update(model4, n.iter=2000)
print(model4.1)
model4.1.mcmc <-as.mcmc(model4.1)
xyplot(model4.1.mcmc)
densityplot(model4.1.mcmc)

sum(short$final_collapse)/length(short$final_collapse)

#####EXTRA JUNK
print(model1)
plot(model1)
traceplot(model1)

# or to use some plots in coda
# use as.mcmc to convert rjags object into mcmc.list
model1.mcmc <-as.mcmc(model1)
## now we can use the plotting methods from coda
xyplot(model1.mcmc)
densityplot(model1.mcmc)
# if the model does not converge, update it!
model1.upd <-update(model1, n.iter=2000)
print(model1.upd)
plot(model1.upd)
model1upd.mcmc <-as.mcmc(model1.upd)
xyplot(model1upd.mcmc)
densityplot(model1upd.mcmc)

# or auto update it until it converges! see ?autojags for details
model1a <-autojags(model1)
print(model1a)
plot(model1a)
model1a.mcmc <-as.mcmc(model1a)
xyplot(model1a.mcmc)
densityplot(model1a.mcmc)
