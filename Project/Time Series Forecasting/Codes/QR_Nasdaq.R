stocks <- read.csv("Stocks.csv") 
str(stocks)

# Loading the packages
library(quantreg)
library(dplyr)
library(ggplot2)
library(caret)

# Model: Quantile Regression for Nifty 50 and Nasdaq
nas_fit10 <- rq( Nifty50 ~ Nasdaq, data = stocks, tau = 0.1)
nas_fit10
summary(nas_fit10)

nas_fit20 <- rq( Nifty50 ~ Nasdaq, data = stocks, tau = 0.2)
nas_fit20
summary(nas_fit20)

nas_fit30 <- rq( Nifty50 ~ Nasdaq, data = stocks, tau = 0.3)
nas_fit30
summary(nas_fit30)

nas_fit40 <- rq( Nifty50 ~ Nasdaq, data = stocks, tau = 0.4)
nas_fit40
summary(nas_fit40)

nas_fit50 <- rq( Nifty50 ~ Nasdaq, data = stocks, tau = 0.5)
nas_fit50
summary(nas_fit50)

nas_fit60 <- rq( Nifty50 ~ Nasdaq, data = stocks, tau = 0.6)
nas_fit60
summary(nas_fit60)

nas_fit70 <- rq( Nifty50 ~ Nasdaq, data = stocks, tau = 0.7)
nas_fit70
summary(nas_fit70)

nas_fit80 <- rq( Nifty50 ~ Nasdaq, data = stocks, tau = 0.8)
nas_fit80
summary(nas_fit80)

nas_fit90 <- rq( Nifty50 ~ Nasdaq, data = stocks, tau = 0.9)
nas_fit90
summary(nas_fit90)

nas_fit95 <- rq( Nifty50 ~ Nasdaq, data = stocks, tau = 0.95)
nas_fit95
summary(nas_fit95)

#linear regression
nas_lm <- lm(Nifty50 ~ Nasdaq, data = stocks)
nas_lm
summary(nas_lm)

#Best Fit: HIGHEST SLOPE - 95th Percentile

nas_fit95 <- rq( Nifty50 ~ Nasdaq, data = stocks, tau = 0.95)
nas_fit95
summary(nas_fit95)

# Plot
plot(Nifty50 ~ Nasdaq, data = stocks, pch = 16, main = "Plot")
abline(lm(Nifty50 ~ Nasdaq, data = stocks), col = "red", lty = 2)
abline(rq(Nifty50 ~ Nasdaq, data = stocks, tau = 0.95), col = "blue", lty = 2)

