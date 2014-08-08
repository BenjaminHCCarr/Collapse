
####modify Andy Royles model for JAGS:
source("sandbox/explore_analyses/nestdata.list_copy.R")
str(nestdata.list)
#the data
y<-nestdata.list$H
first<-nestdata.list$first
last<-nestdata.list$last
AGE<-nestdata.list$AGE
DAY<-nestdata.list$DAY
DAY2<-nestdata.list$DAY2
nind<-nestdata.list$nind
xdense<-nestdata.list$xdense
xplot<-nestdata.list$xplot
str(DAY)
data <- list ("y","first","last","AGE","DAY","DAY2","nind","xdense","xplot")

inits <- function(){
  list (beta0=rnorm(1,0,.2),beta1=rnorm(1,0,.2) ,deneff=rnorm(1,0,.2),dayeff=rnorm(1,0,.2),dayeff2=rnorm(1,0,.2),
sigmaplot=runif(1,1,2),alpha=rnorm(12))
}

parameters <- c("beta0","beta1","deneff","dayeff","dayeff2","sigmaplot")

#the model
model1 <- jags(model.file="sandbox/explore_analyses/jags_m1.txt", data=data, inits=inits, n.chains=1, n.burnin=100, n.iter=1000, parameters.to.save = parameters)

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
? autojags
