# purpose: explore logistic regression analyses
# date: 2014-07-30
# set your local path to Regional_Collapse. 

require(lme4); require(lattice); require (nlme); require(MASS); require(geosphere)

stocks <- read.csv("data_tables/stocks.csv")
LMEs <- read.csv("data_tables/LMEs.csv")

#make a figure, for fun.
jc <- jitter(stocks$collapse_admb, amount = 0.06)
codes = seq(1,length(levels(stocks$LME_NAME)),1)
LME_NAME = levels(stocks$LME_NAME)
trick <- as.data.frame(cbind(LME_NAME, as.numeric(codes)))
colnames(trick) = c("LME_NAME", "codes")
newdata <- merge(stocks, trick, by = "LME_NAME")
plot (jc ~ as.numeric(newdata$codes), xlab = "Large Marine Ecosystem", ylab = "Collapse? (yes or no)", bty="l", col=newdata$codes)
tots = tapply(newdata$collapse_admb, newdata$LME_NAME, length)
collapsed = tapply(newdata$collapse_admb, newdata$LME_NAME, sum)
aves = collapsed/tots
points(as.numeric(codes), aves, pch=19, cex=1.5)

#analyses
#consider centering explanatory variables

#TEST ASSUMPTIONS

#1. Do different LMEs and fish spp have different probabilitites of collapse? (using lmer and Laplace approximation)
#(this is a glmm, and no option to explicitly include spatial autocorrelation in this package)
m1 <- glmer(collapse_admb ~ TS_LENGTH + (1| LME_NAME) + (1|SCIENTIFICNAME), family="binomial", data=stocks) 
summary(m1) #full model
m2 <- glmer(collapse_admb ~ TS_LENGTH + (1| LME_NAME), family="binomial", data=stocks)
summary(m2) #do different fish species have different collpase probabilities? (but LMEs all the same)
m3 <- glmer(collapse_admb ~ TS_LENGTH + (1|SCIENTIFICNAME), family="binomial", data=stocks)
summary(m3) #Are all speces and LMEs the same?
m4 <- glm(collapse_admb ~ TS_LENGTH, family="binomial", data=stocks)
summary(m4) #Are all speces and LMEs the same?
#First, use LRT to find best random effects structure with fixed effects included
AIC(m1, m2, m3, m4)
anova(m1, m2) #model 1 is better
anova(m1,m3) #model 3 is better. LMEs not different, or rather, most of what is different about LMEs is that they have different species in them.
#Not sure how to test model with no random effects (m4), but it is clearly no good if use AIC as criteria.

#Next, find best fixed effects structure:
m5 <- glmer(collapse_admb ~ 1 + (1| SCIENTIFICNAME), family="binomial", data=stocks)
summary(m5) #does length of time series matter? 
AIC(m3, m5)
anova(m3,m5) #not too much. If it was monitored >= 20 years, it was monitored long enough.

plot(residuals(m3))
plot(residuals(m5))

#calculate Moran's I to test for spatial autocorrelation of resids:
require(ape)
#create the distance matrix
stocks2 = stocks[-which(is.na(stocks$collapse_admb)),]
stocks2 = droplevels(stocks2)
stocks2 <- merge(stocks2, LMEs, by = "LME_NAME", all.x = T, all.y=F)
jx <- jitter(stocks2$x_centroid,50)
jy <- jitter (stocks2$y_centroid, 50)
xy <- rbind(jx, jy)
xy <- t(xy)
dista = as.matrix(dist(xy,diag = T,upper = T))
#run some Moran's I tests
res3  = residuals(m3)
Moran.I(res3,dista)

res4 <- residuals(m4)
Moran.I(res4,dista)



#2. If spatial-autocorrelation is present:
#glmmPQL with distance matrix that represents spatial autocorrelation - unfortunatley PQL may be biased for binary errors.

nLMEs <- LMEs[,c(1,7:8)]
stocks2 <- merge(stocks, nLMEs, by = "LME_NUMBER", all.x=T, all.y=F)
str(stocks2)

#quick figure and univariate stuff
jc <- jitter(stocks2$collapse_admb, amount = 0.06)
plot (jc ~ stocks2$icep, xlab = "icep", ylab = "Collapse? (yes or no)", bty="l")
 

m6 <- glmmPQL(collapse_admb ~ TS_LENGTH, random = ~ 1|LME_NAME, family="binomial", data=stocks2) 
summary(m6)
plot(residuals(m6))

#add spatial autocorrelation of residuals:
#method follows http://www.ats.ucla.edu/stat/r/faq/spatial_regression.htm
#a. the function glmmPQL requires a random effect, so make a dummy variable with onle one level:
dummy <- rep(1,length(stocks2$ASSESSID))
stocks2 <- cbind(stocks2, dummy)
#b. the function also requires at least a little distance between each stock, so jitter the centroids.
#ISSUE: is this best way? rnorm instead of jitter?
jx <- jitter(stocks2$x_centroid,50)
jy <- jitter (stocks2$y_centroid, 50)
#c choose a correlation structure(I'm thinking spherical):
m7 <- glmmPQL(collapse_admb ~ TS_LENGTH, random = ~ 1|dummy, correlation = corSpher(1, form = ~jx + jy), family="binomial", data=stocks2) 
summary(m7)
plot(residuals(m7))

length(levels(stocks$SCIENTIFICNAME))
###Didn't use/ not sure how to use:
#create distance matrix. 
xy <- rbind(LMEs$x_centroid, LMEs$y_centroid)
xy <- t(xy)
distance_matrix <- distm(xy)

#3.  If spatial auto-correlation not present
#Hierarchical 'nest survival' model using JAGS
#based on Andy Royle's code
#Must install program JAGS locally
library(coda); library(rjags); library(R2jags)

#read in the fake data for stocks and merge some environmental data to go with it
env <- read.csv("original_data/Environmental_data/lme_average_PCO2.csv")
str(env)
ST <- read.csv("sandbox/explore_analyses/fake_stocks.csv")
#add environmental data to stocks table
ST <- merge(ST, env, by.x = "LME_NUMBER", by.y = "LME_NUMBER", all.x=F, all.y=F)
ST <- droplevels(ST)
#make an integer number for each stock (will be random effect, lke 'plot' in Andy's example)
spp <- levels(ST$SCIENTIFICNAME)
spp_code <- seq(1,length(spp),1)
spp1 <- as.data.frame(cbind(spp, spp_code))
str(spp1)
ST <- merge(ST, spp1, by.x = "SCIENTIFICNAME", by.y = "spp", all.x=T, all.y=F)
head(table)


# This code creates the H table of the nest survival model

collapse = as.numeric(ST$collapse_admb)
y_start = as.numeric(ST$YEAR_START)
y_end = as.numeric(ST$YEAR_END)
year_first = 1945
year_last = 2010

years = seq(year_first,year_last,1)
table = matrix(nrow = length(ST$ASSESSID),ncol = length(years))

for(i in 1:length(ST$ASSESSID))
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

#Building standardized YEAR and YEAR2 matrix
time <- seq(1,66,1)
time = (time-mean(time))/sd(time)
time2 <- time^2
TIME <- rep(time, length(levels(ST$ASSESSID)))
TIME2 <- rep(time2, length(levels(ST$ASSESSID)))
#getting the covariates ready:
PCO2t <- log(ST$NUM_PCO2_SW+1)
PCO2tc <- (PCO2t-mean(PCO2t)/sd(PCO2t))
hist(PCO2tc)


# i = n.stocks (vector of 222)
	#matrix is 222 x 66

#the data
y<-table        #matrix 
first<-ST$YEAR_START-1944  # vector first seen 
last<-ST$YEAR_COLLAPSE - 1944	#vector year of last check (either the year of collapse or the last year of the time series if the stock never collapsed.)
n.stocks<-length(levels(ST$ASSESSID))     #number
TIME<- matrix(TIME, nrow=n.stocks, ncol=length(time),byrow=T)  #matrix 
TIME2<-matrix(TIME2, nrow=n.stocks, ncol=length(time),byrow=T)  #matrix
PCO2tc <- PCO2tc #vector
SPP_CODE<-ST$spp_code	 #vector (like xplot)
length(SPP_CODE)
data <- list ("y","first","last","TIME","TIME2","n.stocks","PCO2tc","SPP_CODE")

inits <- function(){
  list (beta0=rnorm(1,0,.2),beta1=rnorm(1,0,.2) ,yeareff=rnorm(1,0,.2),yeareff2=rnorm(1,0,.2),sigmaplot=runif(1,1,2),alpha=rnorm(107))
}

parameters <- c("beta0","beta1","yeareff","yeareff2","sigmaplot")

#the model
model1 <- jags(model.file="sandbox/explore_analyses/collapse_m1.txt", data=data, inits=inits, n.chains=1, n.burnin=100, n.iter=1000, parameters.to.save = parameters)

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