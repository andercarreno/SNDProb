library(mvtnorm)
source("./Scenarios.R")
set.seed(100)

alldata <- matrix(0, ncol=3)[0,]
# Class 1
n.total <- 5000
ncomp <- 4
mu <- c(-2,0)
sigma <- matrix(c(0.4,1,1,3), ncol=2)
data <- mvtnorm::rmvnorm(n = ceiling(n.total/ncomp), mean = mu, sigma = sigma)
mu <- c(-13,0)
sigma <- matrix(c(0.5,-0.5,-0.5,1), ncol=2)
data <- rbind(data, mvtnorm::rmvnorm(n = ceiling(n.total/ncomp), mean = mu, sigma = sigma))
mu <- c(0,2)
sigma <- matrix(c(5,-1,-1,1), ncol=2)
data <- rbind(data, mvtnorm::rmvnorm(n = ceiling(n.total/ncomp), mean = mu, sigma = sigma))
mu <- c(-7,-4)
sigma <- matrix(c(6,0.2,0.2,0.5), ncol=2)
data <- rbind(data, mvtnorm::rmvnorm(n = ceiling(n.total/ncomp), mean = mu, sigma = sigma))
data <- cbind(data, 1)
alldata <- rbind(alldata, data)

# Class 2
ncomp <- 4
mu <- c(7,5)
sigma <- matrix(c(0.4,-1,-1,3), ncol=2)
data <- mvtnorm::rmvnorm(n = ceiling(n.total/ncomp), mean = mu, sigma = sigma)
mu <- c(6,-2)
sigma <- matrix(c(1.2,1,1,1), ncol=2)
data <- rbind(data, mvtnorm::rmvnorm(n = ceiling(n.total/ncomp), mean = mu, sigma = sigma))
mu <- c(4.5,9)
sigma <- matrix(c(1,0.2,0.2,3), ncol=2)
data <- rbind(data, mvtnorm::rmvnorm(n = ceiling(n.total/ncomp), mean = mu, sigma = sigma))
mu <- c(1,12)
sigma <- matrix(c(0.4,-1,-1,3), ncol=2)
data <- rbind(data, mvtnorm::rmvnorm(n = ceiling(n.total/ncomp), mean = mu, sigma = sigma))
data <- cbind(data, 2)
alldata <- rbind(alldata, data)

# TRAIN y TEST - then generate CSV
train <- alldata
strategy.ix <- 1
test <- t(sapply(1:7001, function(itimes){
  x <- runif(1)
  strat <- getStrategies(2)
  ref.probs <- getProbabilities(strat[[strategy.ix]], itimes)
  ix.model <- 1
  while(x >= sum(ref.probs[1:ix.model])){ # Roulette wheel
    ix.model <- ix.model + 1
  }
  ix.mixture.component <- sample(1:4, size=1)
  if(ix.model == 1){
    if(ix.mixture.component == 1){
      mu <- c(-2,0)
      sigma <- matrix(c(0.4,1,1,3), ncol=2)
    }else if(ix.mixture.component == 2){
      mu <- c(-13,0)
      sigma <- matrix(c(0.5,-0.5,-0.5,1), ncol=2)
    }else if(ix.mixture.component == 3){
      mu <- c(0,2)
      sigma <- matrix(c(5,-1,-1,1), ncol=2)
    }else{
      mu <- c(-7,-4)
      sigma <- matrix(c(6,0.2,0.2,0.5), ncol=2)
    }
  }else if(ix.model == 2){
    if(ix.mixture.component == 1){
      mu <- c(7,5)
      sigma <- matrix(c(0.4,-1,-1,3), ncol=2)
    }else if(ix.mixture.component == 2){
      mu <- c(6,-2)
      sigma <- matrix(c(1.2,1,1,1), ncol=2)
    }else if(ix.mixture.component == 3){
      mu <- c(4.5,9)
      sigma <- matrix(c(1,0.2,0.2,3), ncol=2)
    }else{
      mu <- c(1,12)
      sigma <- matrix(c(0.4,-1,-1,3), ncol=2)
    }
  }else{
    if(ix.mixture.component == 1){
      mu <- c(-8,2)
      sigma <- matrix(c(1,1,1,3), ncol=2)
    }else if(ix.mixture.component == 2){
      mu <- c(-17,4)
      sigma <- matrix(c(0.5,0.5,0.5,1), ncol=2)
    }else if(ix.mixture.component == 3){
      mu <- c(-7,5)
      sigma <- matrix(c(5,0.2,0.2,1), ncol=2)
    }else{
      mu <- c(-13,5)
      sigma <- matrix(c(2,.1,.1,.2), ncol=2)
    }
  }
  data <- mvtnorm::rmvnorm(n = 1, mean = mu, sigma = sigma)
  data <- cbind(data, 'class'=ix.model)
  return(data)
}))

write.table(train, file=paste0("../data/Scenario6/Strategy", strategy.ix, "/TRAIN.csv"), sep=',', dec='.', col.names = F, row.names = F)
write.table(test,  file=paste0("../data/Scenario6/Strategy", strategy.ix, "/TEST.csv"), sep=',', dec='.', col.names = F, row.names = F)
