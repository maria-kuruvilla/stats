setwd("~/Documents/code/temp_collective_code")
data <- read.csv("../../data/temp_collective/roi/stats_loom_latency.csv",header=TRUE,na.strings=c("[nan]"))

lat1 <- data$latency
lat <- as.numeric(lat1)
temp <- stats_loom_latency$Temperature
gs <- stats_loom_latency$Groupsize
loom <- stats_loom_latency$loom

n <- length(!is.na(lat1))
data <- as.data.frame(cbind(sample = (1:n),temp,gs,acc,lat))