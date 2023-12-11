library(readxl)
data <- read.csv("D:/Data STATBIS ITS/TUGAS AKHIR/April-July2021_Sunda.csv")
View(data)

input <- data[, c("msg_mmsi", "data_lat", "data_lon", "data_utc", "data_sog", "data_cog", "dt_last", "shipflag_id", "shipkind_id")]
data$critical_zone <- ifelse(data$critical_zone >= 5, "critical", "not critical")
output <- as.factor(data$critical_zone)

library(ggplot2)
library(lattice)
library(caret)

set.seed(123)

trainIndex <- createDataPartition(y = output, p = 0.7, list = FALSE)
training <- input[trainIndex,]
testing <- input[-trainIndex,]
trainingOutput <- output[trainIndex]
testingOutput <- output[-trainIndex]