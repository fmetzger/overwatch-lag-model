library(tidyverse)
library(ggpubr)

#################################################################
### simulation results of the frame rate study
#for plotting allready existing simulation data
results <- read.csv("results.csv") %>% 
  mutate(framerate = as.factor(framerate.mean), e2e.lag = e2e.lag * 1000) %>% 
  dplyr::select(e2e.lag, framerate)


ggplot(results) + 
  geom_boxplot(mapping = aes(x = framerate, y = e2e.lag)) + 
  scale_y_log10() +
  labs(x = "mean frame rate", y = "E2E lag [s]")


ggplot(results, aes(x = framerate, y = e2e.lag)) + 
  geom_violin() + 
  geom_boxplot(width = 0.15, outlier.size = 0) +
  scale_y_log10(breaks = c(50, 100, 200, 500, 1000)) +
  labs(x = "mean frame rate [Hz]", y = "E2E lag [ms]") +
  theme(text = element_text(size = 16))
ggsave("lagsim.pdf", width = 6, height = 4)


#################################################################
## command message plots
data <- read.csv("send-09.01.18-QP2.csv") %>% 
  mutate(dataset = as.factor('dataset 2'))

data <- data %>% 
  mutate(iat = (Time - lag(Time)) * 1000) %>% 
  filter(Time >= 100 & Time <= 450)

## density
ggplot(data) +
  geom_density(mapping = aes (x = iat), color = "black", fill = "black") +
  xlim(0, 50) + 
  labs(x = "IAT [ms]", y = "density") +
  theme(text = element_text(size = 16))
ggsave("command-density.pdf", width = 6, height = 4)


data <- read.csv("send-09.01.18-QP2.csv") %>% mutate(dataset = as.factor('dataset 2'))
## time series with phases
send.data <- data %>% mutate(iat = Time - lag(Time)) %>% mutate(send.rate = 1/iat) %>% filter(Time >= 45 & Time <= 429)

#estimated iat peaks
hbreaks <- c(0.01666, 0.012, 0.0235)

#estimated phase separators
vbreaks <- c(45, 65, 109, 127.5, 170, 192.5, 230, 250, 290, 309, 350, 370, 410, 429)

vlines <- data.frame(Time = vbreaks) %>% mutate(phase.length = Time - lag(Time))
vlines <- cbind(vlines,data.frame(index = 1:nrow(x = vlines)))

#separat and group the phases into the shorter, diffuse
#and the longer ones with two peaks
short <- vlines %>% filter(index %% 2==0)
long <- vlines %>% filter(index %% 2==1 & index != 1)

split.up.data <- function(data, breaks){
  temp.list <- vector("list", nrow(breaks))
  for (i in 1:nrow(breaks)) {
    temp <- data %>% filter(Time < breaks$Time[i])
    data <- data %>% filter(Time >= breaks$Time[i]) 
    temp.list[[i]] <- temp
  }
  
  first <- NULL
  second <- NULL
  i <- 2
  while (i <= nrow(breaks)-1){
    first <- rbind(first, temp.list[[i]])
    i <- i + 1
    second <- rbind(second, temp.list[[i]])
    i <- i + 1
  }
  splitted <- list(first, second)
  return(splitted)
}

#split up the data into the two phase types
splitted.data <- split.up.data(data = send.data, breaks = vlines)
short.data <- splitted.data[[1]]
long.data <- splitted.data[[2]]

#splitting the long phases data into the upper and the lower part / peak
long.upper.data <- long.data %>% filter(iat > 0.02)
long.lower.data <- long.data %>% filter(iat <= 0.02)





test.send.data <- send.distr(450)
df <- test.send.data %>% 
  mutate(Time = cumsum(iat), iat = iat * 1000) %>%
  mutate(dataset = "generated") %>%
  mutate(No. = NA, Source = NA, Destination = NA, Protocol = NA, Length = NA, Info = NA, send.rate = NA)

short.data <- short.data %>% 
  mutate(dataset = "measured", iat = iat * 1000) # %>%
  #arrange(desc(dataset))

long.upper.data <- long.upper.data %>% 
  mutate(dataset = "measured", iat = iat * 1000) #%>%
  #arrange(desc(dataset))

long.lower.data <- long.lower.data %>% 
  mutate(dataset = "measured", iat = iat * 1000) #%>%
  #arrange(desc(dataset))

df.combined <- rbind(short.data, long.upper.data, long.lower.data, df)

df.combined <- df.combined %>%
  arrange(desc(dataset))
  #mutate(dataset = factor(dataset, levels = c("measured", "generated"))) %>%
  

ggplot(df.combined, aes(x = Time, y = iat)) + 
  geom_point(alpha = 0.2, color = "black", size = .6) + 
  geom_point(data = short.data, mapping = aes(x=Time, y=iat), alpha = 0.2, color = "black", size = .6) + 
  geom_point(data = long.upper.data, mapping = aes(x=Time, y=iat), alpha = 0.2, color = "dodgerblue", size = .6) + 
  geom_point(data = long.lower.data, mapping = aes(x=Time, y=iat), alpha = 0.2, color = "dodgerblue4", size = .6) +
  scale_x_continuous(breaks = c(45, 109, 170, 230, 290, 350, 410), limits = c(45, 429)) +
  scale_y_continuous(breaks = c(5, 10, 16.7, 20, 30, 40, 50, 60), limits = c(5, 70)) +
  labs(x = "time [s]", y = "IAT [ms]") +
  theme(text = element_text(size = 16)) +
  facet_wrap(~ dataset)
ggsave("command-ts-annotated.png", width = 6, height = 4)


tmp <- rbind(short.data, long.upper.data, long.lower.data)
tmp <- tmp$iat
gen <- head(test.send.data$iat * 1000, n = 20753)
gen <- test.send.data$iat * 1000

SSR <- sum((tmp - gen) ** 2)
SST <- sum((tmp - mean(tmp)) **2)
(r.squared.results <- 1 - (SSR / SST))

x <- lm(tmp ~ gen)

cor(tmp, gen)



#################################################################
## characteristics of packets received from the server

####
## time series
receive.data1 <- read.csv("receive-09.01.18-QP1.csv") %>% mutate(dataset = as.factor('dataset 1'))

receive.data1 <- receive.data1 %>% 
  mutate(iat = (Time - lag(Time)) * 1000)
  
ggplot(receive.data1) +
  geom_point(mapping = aes(x = Time, y = iat), alpha = 0.3, size = 0.6) + 
  scale_y_continuous(breaks = c(0, 10, 16.7, 20, 30, 40, 50), limits = c(0, 50)) +
  labs(x = "time [s]", y = "IAT [ms]") +
  theme(text = element_text(size = 16))
ggsave("update-ts.png", width = 6, height = 4)
  
####
## pdf, measured and generated

receive.data1 <- read.csv("receive-09.01.18-QP1.csv") %>%
  mutate(dataset = as.factor('dataset 1'))

receive.data2 <- read.csv("receive-09.01.18-QP2.csv") %>% 
  mutate(dataset = as.factor('dataset 2'))

receive.data <- receive.data2 %>%
  mutate(iat = (Time - lag(Time)) * 1000) %>%
  filter(!is.na(iat) & iat > 0.004 & Time>100 &Time<450) 

test.receive.data <- receive.distr(450) %>%
  mutate(iat = iat * 1000)

ggplot(data = receive.data) + 
  geom_density(mapping = aes(x = iat, y = ..density.., fill = "black"))+
  geom_density(data = test.receive.data, mapping = aes(x = iat, y = ..density.., fill = "red"), alpha = 0.5) +
  scale_fill_manual(name = '', 
                    values =c('black' = 'black', 'red'='red'), 
                    labels = c('measured', 'generated')) +
  scale_x_continuous(breaks = c(0, 10, 16.7, 20, 30), limits = c(0, 30)) +
  labs(x = "IAT [ms]", y = "density") +
  theme(text = element_text(size = 16), legend.position = "bottom")
ggsave("update-density.pdf", width = 6, height = 4)
