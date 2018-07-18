####Random walk model for LakeHuron B Dataset
library(rjags)

#Hu<-read.csv("C:/Users/mwells/Desktop/ForecastingData/Raw/jittered/Dataset_Radar_MWells_2018/HuronB.csv")

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

data <- list(y=log(Hu$NightTPR),n=length(Hu$NightTPR),x_ic=log(1000),tau_ic=100,a_obs=1,r_obs=1,a_add=1,r_add=1)

nchain = 3
init <- list()
for(i in 1:nchain){
  y.samp = sample(Hu$NightTPR,length(Hu$NightTPR),replace=TRUE)
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