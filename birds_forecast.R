# Read in data
bird <- read.csv("data/HuronA.csv")
cov <- read.csv("/Users/vahsen/Downloads/nefi-bird-master/data/output/covariates.csv")

cov_sub <- subset(cov, date_j %in% bird$Night_Number & Site == "HuronA")
cov_sub$date_j - bird$Night_Number
# Matches up
bird_cov <- cbind(bird, cov_sub[,3:5])

lny <- log(y)[,1:50]

data <- list(y = lny,
             n = ncol(lny),
             ns = nrow(lny))

# Sampling to get initial values
inits <- list(
  list(tau_obs = 1, tau_proc = 1, tau_alpha = 1)
)

n.adapt = 2000
n.update = 5000
n.iter= 10000

jm.hurb = jags.model(file="/Users/vahsen/Desktop/bird_JAGS.R", data=data, n.adapt = n.adapt, inits=inits, n.chains=length(inits))

jm.hurb.coda = coda.samples(jm.hurb, n.update = n.update, n.iter = n.iter, c("alpha_site", "tau_alpha", "tau_obs","tau_proc", "x"))
plot(jm.hurb.coda)

out <- as.matrix(jm.hurb.coda)
x.cols <- grep("^x",colnames(out)) ## grab all columns that start with the letter x
ci <- apply(exp(out[,x.cols]),2,quantile,c(0.025,0.5,0.975))

####
# This is just the process model from the JAGS code
forecastX <- function(IC,sig_proc=0,n=Nmc){
  X <- matrix(NA,n,NT)  ## storage
  Xprev <- IC           ## initialize
  for(t in 1:NT){
    mu = Xprev   ## calculate mean
    X[,t] <- rnorm(n, mu, sig_proc)                         ## predict next step
    Xprev <- X[,t]                                  ## update IC
  }
  
  return(X)
}

out <- as.matrix(jm.hurb.coda)
x.cols <- grep("^x",colnames(out)) ## grab all columns that start with the letter x
ci <- apply(exp(out[,x.cols]),2,quantile,c(0.025,0.5,0.975))

odds<- seq(1,100,2)
evens <- seq(2,100,2)
ci_1 <- ci[,odds]
ci_2 <- ci[,evens]

plot(1:50,ci_2[2,],type='n',ylab="count", ylim = c(0, 50000), xlim = c(0, 89))
ecoforecastR::ciEnvelope(1:50,ci_2[1,],ci_2[3,],col="lightBlue")
lines(1:50, ci_2[2,])


IC <- mean(out[,104:105])
tau_proc <- mean(out[,"tau_proc"])
sigma_proc <- 1/sqrt(tau_proc)

N.det <- forecastX(IC=IC,
                   sig_proc=sigma_proc,  
                   n=1)

## Plot run
lines(51:80,exp(N.det),col="purple",lwd=3)


