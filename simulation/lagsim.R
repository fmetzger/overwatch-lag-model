library("parallel")

# set the path to your working directory
source("Distributions.R")

# getting the next value from data after time
##########################################################################################
get.next <- function(data, time){
  return(head(data %>% filter(Time >= time), 1)$Time)  
}

# simulation loop realizing the following model:
#
# Userinput -> nächste Sendezeit -> Nw Delay(send) -> Server Delay -> Nw Delay(receive) -> nächste Ankunftszeit -> nächster Frame
# |																															                                                                 |
# |______________________________________________________End to End Lag__________________________________________________________|
##########################################################################################
sim.loop <- function(i, input, send, receive, frame, total.network.delay, server.delay){
  
  input.time <- input$Time[i];
  
  lag <- (get.next(send, input.time) - input.time) + total.network.delay + server.delay[i]
  
  lag <- get.next(receive, input.time + lag) - input.time
  
  lag <- get.next(frame, input.time + lag) - input.time
  
  return(lag)
}

# initalise and execute one run of the simulation with
##########################################################################################
overwatch.lagsim.round <- function(number.of.inputs, framerate.mean){
  # buffer for the last inputs to run through all simulation stages
  extra.time <- 1 #seconds
  
  #initialise the needed simulation values from the functions from "Distribution.R"
  input <- data.frame(Time = cumsum(input.distr(number.of.inputs)$iat))
  last.input.time <- max(input)
  
  send <- data.frame(Time = cumsum(send.distr(last.input.time + extra.time)$iat))
  
  receive <- data.frame(Time = cumsum(receive.distr(last.input.time + extra.time)$iat))
  
  frame <- data.frame(Time = cumsum(frames.distr(last.input.time + extra.time, framerate.mean)$ift))
  
  server.delay <- server.distr(number.of.inputs)$delay
  
  #network delays in seconds
  send.network.delay <-  0.01496053
  receive.network.delay <- 0.01946559
  
  total.network.delay <- send.network.delay + receive.network.delay
  
  # run the simulation loop on multiple cpu cores
  ###################################################
  
  # register and init parallel executing (possibly needs changes for Linux or Mac)
  nr.of.cores <- detectCores() #- 1 #-1 for saving cpu recources for other tasks if needed
  c1 <- makeCluster(nr.of.cores)
  clusterExport(c1, varlist=c("input", "send", "receive", "frame", "total.network.delay", "server.delay", "get.next"), envir=environment())
  clusterEvalQ(c1, library(dplyr))
  
  #execute the simulation loop
  e2e.lag <- parSapply(c1, X = 1:number.of.inputs, FUN =  sim.loop, input = input, send = send, receive = receive,
                    frame = frame, total.network.delay = total.network.delay, server.delay = server.delay)
  
  # stop parallel executing
  stopCluster(c1)
  
  return(e2e.lag)
}

# execute the simulation for different framerate means (for each "repetions.per.framerate" times) and "number.of.inputs" Inputs per Simulation)
##########################################################################################
overwatch.lagsim <- function(number.of.inputs, repetions.per.framerate){
  
  #choose the framerate means to simulate over:
  
  #framerate.means <- sort(rep(seq(from = 10, to = 120, by = 10), repetions.per.framerate))
  framerate.means <- sort(rep(c(10,15,20,25,30,40,50,60,90,120), repetions.per.framerate))
  
  #execute simulation for every framerate mean * number of repetions per framerate
  results <- NaN
  for(i in 1:length(framerate.means)){
      results <- rbind(results, data.frame(e2e.lag = overwatch.lagsim.round(number.of.inputs, framerate.means[i])) %>%
                         mutate(framerate.mean = as.factor(framerate.means[i])))
  }
  return(results %>% filter(!is.na(e2e.lag)))
}

# execute the simulation
#   7000 might be a good amount of inputs for a single run as the two phases of the send distribution has means about 28 and 40 seconds
#   with the input rate of about 100 inputs per sekond
#   so for 7000 inputs per run we astimated cover both phases in whole
results <- overwatch.lagsim(7000, 10) 


# export results to csv
write.csv(results, "results.csv")

# do boxplot of end to end lags for the different framerate means
ggplot(results) + 
  geom_boxplot(mapping = aes(x = framerate.mean, y = e2e.lag)) + 
  labs(x = "Mittlere Framerate", y = "Ende-zu-Ende-Lag")


#for plotting allready existing simulation data
results <- read.csv("results.csv") %>% mutate(framerate = as.factor(framerate.mean)) %>% dplyr::select(e2e.lag, framerate)

# do boxplot of end to end lags for the different framerate means
ggplot(results) + 
  geom_boxplot(mapping = aes(x = framerate, y = e2e.lag)) + 
  labs(x = "Mittlere Framerate", y = "Ende-zu-Ende-Lag")

