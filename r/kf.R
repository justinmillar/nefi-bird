library(tidyverse)
library(rjags)
library(coda)

## Organize data ----

df <- list.files("data", "*.csv", full.names = TRUE) %>%  # Get vector of .csv filenames
  setdiff("data/Locations.csv") %>%                       # Remove location csv
  set_names(nm = gsub("*data/(.*).csv", "\\1", .)) %>%    # Name vector of id field
  map_df(~read_csv(.), .id = "Location") %>%
  select(-NightTPR) %>% 
  mutate(Date = as.Date(Night_Number, origin = '2016-01-01')) %>% 
  filter(NightCount > 0) %>% 
  spread(Location, NightCount)

df

time = as.Date(df$Date)
sites = names(df[,-c(1,2)])
nsites = length(sites)
y = t(df[,sites])

## plot time-series from states
plot(time,1:length(time),type='n',ylab="Nocturnal Migrations",lwd=2,log='y',
     ylim=range(y,na.rm=TRUE))
for(i in 1:nsites){
  lines(time,y[i,],col=i,lwd=2)
}

legend("bottomright",legend=sites,lwd=2,col=1:nsites)

# Spatial relationship ----
adj = matrix(c(
  0,1,0,0,0,0,0,0,0, 
  1,0,0,0,0,0,0,0,0,
  0,0,0,1,0,0,0,0,0,
  0,0,1,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,1,1,1,
  0,0,0,0,0,1,0,1,1,
  0,0,0,0,0,1,1,0,1,
  0,0,0,0,0,0,0,1,0
),nsites,nsites,byrow=TRUE)


 # JAGS ----
SpatialRandomWalk = "
model{

#### Data Model
for(t in 1:n){
for(i in 1:nsites){
y[i,t] ~ dnorm(x[i,t],tau_obs)
}
}

#### Process Model
for(t in 2:n){
for(i in 1:nsites){
mu[i,t] <- x[i,t-1] + alpha * sum(adj[i,1:nsites]*x[1:nsites,t-1])
}
x[1:nsites,t] ~ dmnorm(mu[1:nsites,t],Omega_proc)
}

#### Priors
for(i in 1:nsites){
x[i,1] ~ dnorm(x_ic,tau_ic)
}
tau_obs ~ dgamma(a_obs,r_obs)
Omega_proc ~ dwish(R,k)
alpha ~ dbeta(1,20)
}
"

# Sample JAGS model ----
data <- list(
  y = log(y),
  n =  length(time),
  nsites = nsites, 
  R = diag(1, nsites, nsites), 
  k = nsites+1,
  a_obs = 0.001, 
  r_obs = 0.001,
  x_ic = 100, 
  tau_ic = 10e6, 
  adj = adj
)

j.model <- jags.model(textConnection(SpatialRandomWalk), data = data, n.chains = 3)

## Sample parameters for KF
j.out <- coda.samples(j.model, n.iter = 10000, variable.names = c("tau_obs", "Omega_proc"))

# save.image("spatial-rw.Rdata") # ran before bed

## Looking at the JAGS predictions
j.pred.out <- coda.samples(j.model, n.iter = 10000, variable.names = c("y"))


# Get parameters for KF ----
j.params <- j.out %>% 
  as.matrix() %>% 
  apply(2, quantile, 0.5)

tau_obs <- j.params["tau_obs"]

tau_proc <- j.params[-82] %>% 
  matrix(nsites, nsites, byrow = TRUE, list(sites, sites))

## Observation error
tau_obs

## process error covariance
tau_proc

## process error covariance
cov2cor(tau_proc)

## process error SD
sqrt(diag(tau_proc))

# Kalman Filter functions ----
##'  Kalman Filter
##' @param  M   = model matrix
##' @param  mu0 = initial condition mean vector
##' @param  P0  = initial condition covariance matrix
##' @param  Q   = process error covariance matrix
##' @param  R   = observation error covariance matrix
##' @param  Y   = observation matrix (with missing values as NAs), time as col's
##'
##' @return list
##'  mu.f, mu.a  = state mean vector for (a)nalysis and (f)orecast steps
##'  P.f, P.a    = state covariance matrix for a and f
KalmanFilter <- function(M,mu0,P0,Q,R,Y){
  
  ## storage
  nstates = nrow(Y)  
  nt = ncol(Y)
  mu.f  = matrix(NA,nstates,nt+1)  ## forecast mean for time t
  mu.a  = matrix(NA,nstates,nt)  ## analysis mean for time t
  P.f  = array(NA,c(nstates,nstates,nt+1))  ## forecast variance for time t
  P.a  = array(NA,c(nstates,nstates,nt))  ## analysis variance for time t
  
  ## initialization
  mu.f[,1] = mu0
  P.f[,,1] = P0
  I = diag(1,nstates)
  
  ## run updates sequentially for each observation.
  for(t in 1:nt){
    
    ## Analysis step: combine previous forecast with observed data
    KA <- KalmanAnalysis(mu.f[,t],P.f[,,t],Y[,t],R,I)
    mu.a[,t] <- KA$mu.a
    P.a[,,t] <- KA$P.a
    
    ## Forecast step: predict to next step from current
    KF <- KalmanForecast(mu.a[,t],P.a[,,t],M,Q)
    mu.f[,t+1] <- KF$mu.f
    P.f[,,t+1] <- KF$P.f
  }
  
  return(list(mu.f=mu.f,mu.a=mu.a,P.f=P.f,P.a=P.a))
}

##' Kalman Filter: Analysis step
##' @param  mu.f = Forecast mean (vector)
##' @param  P.f  = Forecast covariance (matrix)
##' @param  Y    = observations, with missing values as NAs) (vector)
##' @param  R    = observation error covariance (matrix)
##' @param  H    = observation matrix (maps observations to states)
KalmanAnalysis <- function(mu.f,P.f,Y,R,H){
  obs = !is.na(Y) ## which Y's were observed?
  if(any(obs)){
    H <- H[obs,]                                              ## observation matrix
    K <- P.f %*% t(H) %*% solve(H%*%P.f%*%t(H) + R[obs,obs])  ## Kalman gain
    mu.a <- mu.f + K%*%(Y[obs] - H %*% mu.f)                  ## update mean
    P.a <- (1-K %*% H)*P.f                                    ## update covariance
  } else {
    ##if there's no data, the posterior is the prior
    mu.a = mu.f
    P.a = P.f
  }
  return(list(mu.a=mu.a,P.a=P.a))
}

##' Kalman Filter: Forecast Step
##' @param mu.a = analysis posterior mean (vector)
##' @param P.a  = analysis posterior covariance (matrix)
##' @param M    = model (matrix)
##' @param  Q   = process error covariance (matrix)
KalmanForecast <- function(mu.a,P.a,M,Q){
  mu.f = M%*%mu.a
  P.f  = Q + M*P.a*t(M)
  return(list(mu.f=mu.f,P.f=P.f))
}

# Kalman inputs ----
## log transform data
Y   = log10(y)

## options for process model 
alpha = 0       ## assume no spatial flux
#alpha = 0.05    ## assume a large spatial flux
M = adj*alpha + diag(1-alpha*apply(adj,1,sum))  ## random walk with flux

## options for process error covariance
Q = tau_proc            ## full covariance matrix
#Q = diag(diag(Q))       ## diagonal covariance matrix

## observation error covariance (assumed independent)  
R = diag(tau_obs,nsites) 

## prior on first step, initialize with long-term mean and covariance
mu0 = apply(Y,1,mean,na.rm=TRUE)
P0 = cov(t(Y),use="pairwise.complete.obs")

## Run Kalman Filter
KF00 = KalmanFilter(M,mu0,P0,Q,R,Y)
