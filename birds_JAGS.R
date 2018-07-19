model{
  
  ## priors
  tau_obs ~ dgamma(1,1)
  tau_proc ~ dgamma(1,1)
  
  ## random effects and initial conditions, s = site
  x[1] <- dnorm(log(1000), 0.001)
  
  ## process model, t = time, s = site
  for(t in 2:NT){
      x[t] <- dnorm(x[t-1], tau_proc)
  }
  ## observation model
  for(t in 1:NT){
      y[t] ~ dnorm(x[t], tau_obs)
    }
}