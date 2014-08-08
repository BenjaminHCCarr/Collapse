
`panel11pt3.fn` <-
function(ni=11000,nb=1000,nthin=2,nc=2){
# R script for fitting the nest survival model described in Panel 11.3 
# in WinBUGS. Requires nestdata.list. Results should coincide with 
# Table 11.2.
    
library("R2WinBUGS")

y<-nestdata.list$H
first<-nestdata.list$first
last<-nestdata.list$last
AGE<-nestdata.list$AGE
DAY<-nestdata.list$DAY
DAY2<-nestdata.list$DAY2
nind<-nestdata.list$nind
xdense<-nestdata.list$xdense
xplot<-nestdata.list$xplot

sink("winbugsmodel.txt")
cat("
model {
beta0~dunif(-5,5)
beta1~dunif(-5,5)
deneff~dunif(-5,5)
dayeff~dunif(-5,5)
dayeff2~dunif(-5,5)
sigmaplot~dunif(0,7)
tauplot<-sqrt(1/(sigmaplot*sigmaplot))
for(i in 1:12){
alpha[i]~dnorm(0,tauplot) ###I(-12,12)
}
for(i in 1:nind){
  for(j in (first[i]+1):last[i]){
     logit(phi[i,j])<-beta0 + beta1*((AGE[i,j]-10)/5)  + dayeff*DAY[i,j] + dayeff2*DAY2[i,j] + deneff*xdense[i] + alpha[xplot[i]]
      mu[i,j]<-phi[i,j]*y[i,j-1]
      y[i,j]~dbern(mu[i,j])
}
}
}
",fill=TRUE)
sink()

data <- list ("y","first","last","AGE","DAY","DAY2","nind","xdense","xplot")
inits <- function(){
  list (beta0=rnorm(1,0,.2),beta1=rnorm(1,0,.2) ,deneff=rnorm(1,0,.2),dayeff=rnorm(1,0,.2),dayeff2=rnorm(1,0,.2),
sigmaplot=runif(1,1,2),alpha=rnorm(12))
}
parameters <- c("beta0","beta1","denseff","dayeff","dayeff2","sigmaplot")
out <- bugs (data, inits, parameters, "winbugsmodel.txt", n.thin=nthin,n.chains=nc, n.burnin=nb,n.iter=ni,debug=TRUE)


return(out)


}


