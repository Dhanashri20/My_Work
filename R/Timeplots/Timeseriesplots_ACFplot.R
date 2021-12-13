#########Q1##########
library(forecast)
data = read.csv("AUS_Arrival.csv",header = TRUE)
head(data)
unique(data$Origin)

#Convert data into time series format
data_japan=data[which(data$Origin=="Japan"),]
Japan.ts=ts(data_japan$Arrivals,start = c(1981,1),frequency = 4)
data_NZ=data[which(data$Origin=="NZ"),]
NZ.ts=ts(data_NZ$Arrivals,start = c(1981,1),frequency = 4)
data_UK=data[which(data$Origin=="UK"),]
UK.ts=ts(data_UK$Arrivals,start = c(1981,1),frequency = 4)
data_US=data[which(data$Origin=="US"),]
US.ts=ts(data_US$Arrivals,start = c(1981,1),frequency = 4)

#Time plot
plot(Japan.ts, main = "Time plot")
#The Japan arrivals have an increasing trend till 1995 and then we can observe a downfall.
plot(NZ.ts, main = "Time plot")
#The New Zealand arrivals have an increasing trend with a specific pattern.
plot(UK.ts, main = "Time plot")
#The United Kingdom arrival has an upward trend till 2005. Then the arrivals started slightly decreasing.
plot(US.ts, main = "Time plot")
#An increasing trend in the US arrivals can be observed.

#Seasonal plot
ggseasonplot(Japan.ts, year.labels=TRUE, continuous=TRUE)
ggseasonplot(NZ.ts, year.labels=TRUE, continuous=TRUE)
ggseasonplot(UK.ts, year.labels=TRUE, continuous=TRUE)
ggseasonplot(US.ts, year.labels=TRUE, continuous=TRUE)

#Seasonal polar plot
ggseasonplot(Japan.ts, year.labels=TRUE, continuous=TRUE,polar = TRUE)
ggseasonplot(NZ.ts, year.labels=TRUE, continuous=TRUE,polar = TRUE)
ggseasonplot(UK.ts, year.labels=TRUE, continuous=TRUE,polar = TRUE)
ggseasonplot(US.ts, year.labels=TRUE, continuous=TRUE,polar = TRUE)

#Seasonal subseries plot
ggsubseriesplot(Japan.ts,year.labels=TRUE)
ggsubseriesplot(NZ.ts,year.labels=TRUE)
ggsubseriesplot(UK.ts,year.labels=TRUE)
ggsubseriesplot(US.ts,year.labels=TRUE)



######Q2#########
library(forecast)
data1 = read.csv("Canda_gas.csv",header = TRUE)
head(data1)
#Convert data into time series format
data1.ts=ts(data1$Volume,start = c(1960,1),frequency = 12)
#Time plot
plot(data1.ts, main = "Time plot")
#There was an upward trend till 1973. Then there was a constant trend for approximately 13-14 years and after 1988 we can observe the increasing trend again.

#Seasonal plot
ggseasonplot(data1.ts, year.labels=TRUE, continuous=TRUE)
#Seasonal polar plot
ggseasonplot(data1.ts, year.labels=TRUE, continuous=TRUE,polar = TRUE)
#Seasonal subseries plot
ggsubseriesplot(data1.ts,year.labels=TRUE)

#Lag plot with maximum lag=12
lag.plot(data1.ts,12,labels = FALSE,do.lines = FALSE,layout = c(2,2))
#The data follows a specific pattern with maximum lag 12. Therefore, autocorrelation is significant.

#Correlogram and autocorrelation coefficients with maximum lag = 48
ACFdata=acf(data1.ts,lag.max =48,main="ACF Plot for Canada Gas data")
ACFdata$acf
#Autocorrelation is significant for all lag values up to 48. Therefore, autocorrelation exists in the data.