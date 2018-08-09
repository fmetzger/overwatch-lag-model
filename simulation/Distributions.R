library(ggplot2)
library(dplyr)
library(MASS)
library(ggpubr)

#Receive distribution
#####################################################################
location <- 0.0155
scale <- 0.0011

# modeled PDF
receive.density <- function(x){
  if(x > 0 && x <= 0.145){
    return(dcauchy(x, location, scale)^1.17*(0.7*sin((x+0.055)*3120)^4+0.2)/0.9848153)
  }else{
    return(0)
  }
}

# envelope for accept-reject
t <- function(x){
  return(dcauchy(x, location, scale)*3)
}

# create receive times up to "duration"
receive.distr <- function(duration){
  n <- ceiling(duration/location * 2)
  receivs <- rep(NaN, n)
  time <- 0
  
  i <- 1
  while(time <= duration && i <= n){
    u.rnd <- runif(1)
    y.rnd <- rcauchy(1, location, scale)
    if(u.rnd <= receive.density(y.rnd)/t(y.rnd)){
      receivs[i] <- y.rnd
      time <- time + y.rnd
      i <- i + 1
    }
  }
  if(i >= n){print("waring: not enough values created - to less space alloceted")}
  receivs <- pmax(0, receivs)
  return(data.frame(iat = receivs) %>% filter(!is.na(iat)))
}


#send distribution
#####################################################################

# init evalueted parameters for the distributions
fit.short.phases.mean <- 28.85714
fit.short.phases.sd <- 1.216385

fit.long.phases.mean <- 40.83333
fit.long.phases.sd <- 2.054805

fit.short.shape <- 4.437922
fit.short.rate <- 208.366

fit.long.lower.shape <- 49.32119
fit.long.lower.rate <- 3970.154

fit.long.upper.mean <- 0.02359103
fit.long.upper.sd <- 0.001369922

# create send times up to "duration"
send.distr <- function(duration){
  n <- ceiling(duration/location * 2)
  sends <- rep(NaN, n)
  time <- 0
  
  #start with a short phase
  last.phase.change <- 0
  next.phase <- rnorm(1, fit.short.phases.mean, fit.short.phases.sd)
  
  phase <- 0 
  
  i <- 1
  while(time <= duration && i <= n){
    #phase changing
    if(time >= last.phase.change + next.phase){ #time for phase change
      last.phase.change <- time
      if(phase == 0){#we are in a short phase
        next.phase <- rnorm(1, fit.long.phases.mean, fit.long.phases.sd)
        phase <- 1 #next phase is a long one
      } else {#we are in a long phase
        next.phase <- rnorm(1, fit.short.phases.mean, fit.short.phases.sd)
        phase <- 0 #next phase is a short one
      }
    }
    
    #send inter arrival time creation
    if(phase == 0){#we are in a short phase
      time.delta <- rgamma(1, fit.short.shape, fit.short.rate)
    } else {#we are in a long phase
      rnd <- runif(1)
      if(rnd < 0.8){#long.lower case
        time.delta <- rgamma(1, fit.long.lower.shape, fit.long.lower.rate)
      } else {#long.upper case
        time.delta <- rnorm(1, fit.long.upper.mean, fit.long.upper.sd)
      }
    }
    time <- time + time.delta
    sends[i] <- time.delta
    i <- i + 1
  }
  if(i >= n){print("waring: not enough values created - to less space alloceted")}
  sends <- pmax(0, sends)
  return(data.frame(iat = sends) %>% filter(!is.na(iat)))
}


#User input distribution
#####################################################################

# create "n" user input times
input.distr <- function(n){
  inputs <- rexp(n, 102.1824)
  return(data.frame(iat = inputs))
}

#server delay distribution
#####################################################################

# create "n" server delays
server.distr <- function(n){
  delays <- pmax(0, rnorm(n, mean = 0.003, sd = 0.0001))
  return(data.frame(delay = delays))
}


#monitor frame distribution
#####################################################################

# create frame times up to "duration"
frames.distr <- function(duration, mean.framerate = 60){
  n <- ceiling(duration/location * 2)
  frames <- rep(NaN, n)
  time <- 0
  
  i <- 1
  while(time <= duration && i <= n){
    framerate <- pmax(1, rnorm(1, mean = mean.framerate, sd = 10))
    frames[i] <- 1/framerate
    time <- time + 1/framerate
    i <- i + 1
  }
  
  return(data.frame(ift = frames) %>% filter(!is.na(ift)))
}