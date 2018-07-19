####Random walk model for LakeHuron B Dataset
library(rjags)

Hu<-read.csv("data/HuronA.csv")

##Working thru a simple random walk with the Hu dataset
## To Do: start working thru a forecast model-- strip off backend of huronb data,
## start getting this working to forecast back half of the period. 
##
RandomWalk = "
model{

#### Data Model
for(t in 1:n){
y[t] ~ dnorm(x[t],tau_obs)
}

#### Process Model
for(t in 2:n){
x[t]~dnorm(x[t-1],tau_add)
}

#### Priors
x[1] ~ dnorm(x_ic,tau_ic)
tau_obs ~ dgamma(a_obs,r_obs)
tau_add ~ dgamma(a_add,r_add)
}
"

data <- list(y=log(HuF$NightTPR),n=length(HuF$NightTPR),x_ic=log(1000),tau_ic=100,a_obs=1,r_obs=1,a_add=1,r_add=1)

nchain = 3
init <- list()
for(i in 1:nchain){
  y.samp = sample(HuF$NightTPR,length(HuF$NightTPR),replace=TRUE)
  init[[i]] <- list(tau_add=1/var(diff(log(y.samp))),tau_obs=5/var(log(y.samp)))
}

j.model   <- jags.model (file = textConnection(RandomWalk),
                         data = data,
                         inits = init,
                         n.chains = 3)
jags.out   <- coda.samples (model = j.model,
                            variable.names = c("tau_add","tau_obs", "x"),
                            n.iter = 10000)
plot(jags.out)



#####Forecast model

##` @param IC    Initial Conditions
##` @param ppt   Precipitation forecast
##` @param Q     Process error (default = 0 for deterministic runs)
##` @param n     Size of Monte Carlo ensemble
forecastN <- function(IC,Q=0,n=Nmc){
  N <- matrix(NA,n,NT)  ## storage
  Nprev <- IC           ## initialize
  for(t in 1:NT){
    # K = pmax(1,Kg + alpha + beta*log(ppt[,t]/800))  ## calculate carrying capacity
    mu = mean(Nprev)   ## calculate mean
    N[,t] <- rnorm(n,mu,Q)                         ## predict next step
    Nprev <- N[,t]                                  ## update IC
  }
  return(N)
}

params <- as.matrix(jags.out) #not needed for univariate
param.mean <- apply(params,2,mean) # not needed as univariate

####This is the issue, need to fix the predict on this!!!!:
IC <- predict(jags.out) #predict #Use for multivariate: IC<-as.matrix(jags.out$predict) 

pred<-as.vector((jags.out[,c(3:52)])) #pulls mcmc outputs for the IC
unl<-unlist(pred) #combines the three mcmc chains
IC<-mean(unl) #gets the mean of the mcmc output


NT<-length(HuF$NightTPR) #because n gets used in the ForecastN model. In future,
##swap out for pulling n from the jags model.
##Forecast:

N.I<-forecastN(IC,n=1)




