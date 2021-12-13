#PRACTICAL 4

library(forecast)
library(ggplot2)

#QUESTION 1

datap4q1 <- read.csv("data_occupancy.csv")
head(datap4q1)
data41.ts <- ts(datap4q1$Occupancy_Rate,start =2006,frequency = 4)

#(a)

#3MA
MA3 <- ma(data41.ts, order = 3)
data_MA3 <- cbind(Value = data41.ts, MA3 = MA3)
head(data_MA3)

#5MA
MA5 <- ma(data41.ts, order = 5)
data_MA5 <- cbind(Value = data41.ts, MA5 = MA5)
head(data_MA5)

#(b)

#4MA
MA4 <- ma(data41.ts, order = 4)
data_MA4 <- cbind(Value = data41.ts, MA4 = MA4)
head(data_MA4)

#2x4MA
MA2x4 <- ma(data41.ts, order = 4, centre = TRUE)
data_MA2x4 <- cbind(Value = data41.ts, MA2x4 = MA2x4)
head(data_MA2x4)

#(c)

#MSE
MSE3 <- mean((data41.ts - MA3)^2, na.rm = TRUE)
MSE5 <- mean((data41.ts - MA5)^2, na.rm = TRUE)
MSE4 <- mean((data41.ts - MA4)^2, na.rm = TRUE)
MSE2x4 <- mean((data41.ts - MA2x4)^2, na.rm = TRUE)

#MAD
MAD3 <- mean(abs(data41.ts - MA3), na.rm = TRUE)
MAD5 <- mean(abs(data41.ts - MA5), na.rm = TRUE)
MAD4 <- mean(abs(data41.ts - MA4), na.rm = TRUE)
MAD2x4 <- mean(abs(data41.ts - MA2x4), na.rm = TRUE)

#table for comparison
tab <- data.frame(Value = c("MA3", "MA5", "MA4", "MA2x4"), MSE = c(MSE3, MSE5, MSE4, MSE2x4), MAD = c(MAD3, MAD5, MAD4, MAD2x4))
tab
#CONCLUSION: Since the values of MSE and MAD are both minimum for MA3, so Moving Average - 3 (MA-3) is the best model for the data.

#(d)
autoplot(data41.ts, series = "Data") +
  autolayer(MA3, series = "3MA") +
  autolayer(MA5, series = "5MA") +
  autolayer(MA4, series = "4MA") +
  autolayer(MA2x4, series = "2x4MA")


########################


#QUESTION 2

datap4q2 <- read.table("SOI.txt")
head(datap4q1)

#(a)
data42.ts <- ts(data = datap4q2, start = 1866, frequency = 12)


#(b)

#2x4MA
MA2x4 <- ma(data42.ts, order = 4, centre = TRUE)
data_MA2x4 <- cbind(Value = data42.ts, MA2x4 = MA2x4)
head(data_MA2x4)

#2x12MA
MA2x12 <- ma(data42.ts, order = 12, centre = TRUE)
data_MA2x12 <- cbind(Value = data42.ts, MA2x12 = MA2x12)
head(data_MA2x12, 14)

#(c)

#MSE
MSE2x4 <- mean((data42.ts - MA2x4)^2, na.rm = TRUE)
MSE2x12 <- mean((data42.ts - MA2x12)^2, na.rm = TRUE)

#MAD
MAD2x4 <- mean(abs(data42.ts - MA2x4), na.rm = TRUE)
MAD2x12 <- mean(abs(data42.ts - MA2x12), na.rm = TRUE)

#table for comparison
tab <- data.frame(Value = c("2x4-MA", "2x12-MA"), MSE = c(MSE2x4, MSE2x12), MAD = c(MAD2x4, MAD2x12))
tab
#CONCLUSION: Since the values of both MSE and MAD are minimum for 2x4-MA, it is the best model for the data

#(d)

autoplot(data42.ts, series = "Data") +
  autolayer(MA2x4, series = "2x4-MA") +
  autolayer(MA2x12, series = "2x12-MA")


########################


#QUESTION 3

datap4q3 <- read.csv("Manf.csv")
head(datap4q3)

data43.ts <- ts(datap4q3[,2], frequency = 4, start = 2000)

#Additive Model
DA_Add <- decompose(data43.ts, type = "additive")
plot(DA_Add)
#INTERPRETATION: The graph for seasonal component proves seasonality since it is repetitive in its movement

#Multiplicative Model
DA_Mul <- decompose(data43.ts, type = "multiplicative")
plot(DA_Mul)
#INTERPRETATION: The graph for seasonal component proves seasonality since it is repetitive in its movement


########################


#QUESTION 4

data4=read.csv("elec.csv")
head(data4)
data4_ts=ts(data4[,2],start=1958,frequency = 12)

#Additive Model
DA=decompose(data4_ts,type="additive")
plot(DA)
#INTERPRETATION:

#Multiplicative Model
DP=decompose(data4_ts,type="multiplicative")
plot(DP)
#INTERPRETATION:


########################
