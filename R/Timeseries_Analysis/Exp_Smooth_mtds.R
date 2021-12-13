#PRACTICAL 5

#install.packages("fpp2")
library(fpp2)

#QUESTION 1

datap5q1 <- read.csv("Gasoline_sales.csv")
head(datap5q1)

y=datap5q1[,2]
n=length(y)

#alpha = 0.25

#Single Exponential Smoothing
SES25 <- ses(y, alpha = 0.25)

#Fitted Value
fit25 <- SES25$fitted

#MSE
MSE25 <- mean((y - fit25)^2, na.rm = TRUE)

#MAD
MAD25 <- mean(abs(y - fit25), na.rm = TRUE)

#alpha = 0.5

#Single Exponential Smoothing
SES50 <- ses(y, alpha = 0.5)

#Fitted Value
fit50 <- SES50$fitted

#MSE
MSE50 <- mean((y - fit50)^2, na.rm = TRUE)

#MAD
MAD50 <- mean(abs(y - fit50), na.rm = TRUE)

#alpha = 0.75

#Single Exponential Smoothing
SES75 <- ses(y, alpha = 0.75)

#Fitted Value
fit75 <- SES75$fitted

#MSE
MSE75 <- mean((y - fit75)^2, na.rm = TRUE)

#MAD
MAD75 <- mean(abs(y - fit75), na.rm = TRUE)

#Table for Comparison

Alpha = c("0.25", "0.50", "0.75")
MSE = c(MSE25, MSE50, MSE75)
MAD = c(MAD25, MAD50, MAD75)

tab <- data.frame(Alpha, MSE, MAD)
tab

#Plots

index=1:n
plot(index,y,type="l",col="red", lwd = 2, xlab = "Months", ylab = "Y")
lines(datap5q1$X,fit25,col="blue",type = "b",lty=2)
lines(datap5q1$X,fit50,col="green",type = "b",lty=2)
lines(datap5q1$X,fit75,col="yellow",type = "b",lty=2)

#CONCLUSION: MSE and MAD minimum for alpha=0.25. Therefore, alpha=0.25 is optimal choice.


######################


#QUESTION 2

datap5q2 <- read.csv("MorgStan.csv", header = TRUE)
head(datap5q2)

y=datap5q2[,2]
n=length(y)
months=c(1:n)

beta = 0.2

#(a)

#Single Adaptive Exponential Smoothing with alpha intial value = 0.4

Fr = c()
At = c()
Mt = c()
Er = c()

alpha = rep(0.4, 4)

Fr[1] = y[1]

Fr[2] = Fr[1]
Er[2] = y[2] - Fr[2]
At[2] = beta*Er[2]
Mt[2] = abs(At[2])

Fr[3] = alpha[3]*y[2] + (1 - alpha[2])*Fr[2]
Er[3] = y[3] - Fr[3]
At[3] = beta*Er[3] + (1 - beta)*At[2]
Mt[3] = beta*abs(Er[3]) + (1 - beta)*Mt[2]

Fr[4] = alpha[4]*y[3] + (1-alpha[3])*Fr[3]
Er[4] = y[4] - Fr[4]
At[4] = beta*Er[4] + (1 - beta)*At[3]
Mt[4] = beta*abs(Er[4]) + (1 - beta)*Mt[3]

for(i in 4:n){
  alpha[i+1] = abs(At[i]/Mt[i])
  Fr[i+1] = alpha[i+1]*y[i] + (1 - alpha[i])*Fr[i]
  Er[i+1] = y[i+1] - Fr[i+1]
  At[i+1] = beta*Er[i+1] + (1 - beta)*At[i]
  Mt[i+1] = beta*abs(Er[i+1]) + (1 - beta)*Mt[i]
}

cbind(yt = c(y,0), Fr = Fr, Er = Er, At = At, Mt = Mt, alphat = alpha)

Fr_40 = Fr[-(n+1)]
MSE_40 = mean((y - Fr_40)^2, na.rm = TRUE)
MAD_40 = mean(abs(y - Fr_40), na.rm = TRUE)

#Single Adaptive Exponential Smoothing with alpha intial value = 0.9

Fr = c()
At = c()
Mt = c()
Er = c()

alpha = rep(0.9, 4)

Fr[1] = y[1]

Fr[2] = Fr[1]
Er[2] = y[2] - Fr[2]
At[2] = beta*Er[2]
Mt[2] = abs(At[2])

Fr[3] = alpha[3]*y[2] + (1 - alpha[2])*Fr[2]
Er[3] = y[3] - Fr[3]
At[3] = beta*Er[3] + (1 - beta)*At[2]
Mt[3] = beta*abs(Er[3]) + (1 - beta)*Mt[2]

Fr[4] = alpha[4]*y[3] + (1-alpha[3])*Fr[3]
Er[4] = y[4] - Fr[4]
At[4] = beta*Er[4] + (1 - beta)*At[3]
Mt[4] = beta*abs(Er[4]) + (1 - beta)*Mt[3]

for(i in 4:n){
  alpha[i+1] = abs(At[i]/Mt[i])
  Fr[i+1] = alpha[i+1]*y[i] + (1 - alpha[i])*Fr[i]
  Er[i+1] = y[i+1] - Fr[i+1]
  At[i+1] = beta*Er[i+1] + (1 - beta)*At[i]
  Mt[i+1] = beta*abs(Er[i+1]) + (1 - beta)*Mt[i]
}

cbind(yt = c(y,0), Fr = Fr, Er = Er, At = At, Mt = Mt, alphat = alpha)

Fr_90 = Fr[-(n+1)]
MSE_90 = mean((y - Fr_90)^2, na.rm = TRUE)
MAD_90 = mean(abs(y - Fr_90), na.rm = TRUE)

tab <- data.frame(IntialAlpha = c("0.4", "0.9"), MSE = c(MSE_40, MSE_90), MAD = c(MAD_40, MAD_90))
tab

index=1:n
plot(index,y,xlab="Months",col="red",type="l", lwd = 2)
lines(index,Fr_40,col="blue",type = "b",lty=2)
lines(index,Fr_90,col="green",type = "b",lty=2)
legend("bottomleft", legend=c("Data","Fr_40","Fr_90"),col=c("red","blue","green"),lty=c(1, 2, 2),bty="n")

#CONCLUSION: MSE and MAD minimum for intial alpha=0.90. Therefore, intial alpha=0.90 is optimal choice.

# (b)

#Holt's Linear Method with initial alpha = 0.3

H_30=holt(y,alpha=0.3)
Fr_30=H_30$fitted
MSE_30=mean((y-Fr_30)^2)
MAD_30=mean(abs(y-Fr_30))

#Holt's Linear Method with initial alpha = 0.6

H_60=holt(y,alpha=0.6, na.rm = TRUE)
Fr_60=H_60$fitted
MSE_60=mean((y - Fr_60)^2, na.rm = TRUE)
MAD_60=mean(abs(y - Fr_60), na.rm = TRUE)

tab <- data.frame(IntialAlpha = c("0.3", "0.6"), MSE = c(MSE_30, MSE_60), MAD = c(MAD_30, MAD_60))
tab

plot(index,y,xlab="Months",col="red",type="l", lwd = 2)
lines(index,Fr_30,col="blue",type = "b",lty=2)
lines(index,Fr_60,col="green",type = "b",lty=2)
legend("topleft", legend=c("data","Fr_30","Fr_60"),col=c("red","blue","green"),lty = c(1,2,2),bty="n")

#CONCLUSION: MSE and MAD minimum for intial alpha=0.60. Therefore, intial alpha=0.90 is optimal choice.


#########################


##### QUESTION 3

datap5q3 <- read.csv("CPI.csv")
head(datap5q3)

y = datap5q3[,3]
y.ts = ts(y, start = 1998, frequency = 4)

#(a)

HW_a = HoltWinters(y.ts, alpha = 0.1, beta = 0.2, gamma = 0.1)

#(b)

HW_b = HoltWinters(y.ts, alpha = 0.4, beta = 0.2, gamma = 0.2)

#(c)

HW_c = HoltWinters(y.ts, alpha = 0.6, beta = 0.5, gamma = 0.9)

#Fitted Values
Fr_a <- HW_a$fitted[,"xhat"]
Fr_b <- HW_b$fitted[,"xhat"]
Fr_c <- HW_c$fitted[,"xhat"]

#MSE
MSEa = mean((y.ts - Fr_a)^2, na.rm = TRUE)
MSEb = mean((y.ts - Fr_b)^2, na.rm = TRUE)
MSEc = mean((y.ts - Fr_c)^2, na.rm = TRUE)

#MAD
MADa = mean(abs(y.ts - Fr_a), na.rm = TRUE)
MADb = mean(abs(y.ts - Fr_b), na.rm = TRUE)
MADc = mean(abs(y.ts - Fr_c), na.rm = TRUE)

tab <- data.frame(Combination = c("(a)", "(b)", "(c)"), Alpha = c(0.1, 0.4, 0.6), Beta = c(0.2, 0.2, 0.5), Gamma = c(0.1, 0.2, 0.9), MSE = c(MSEa, MSEb, MSEc), MAD = c(MADa, MADb, MADc))
tab

#CONCLUSION: Since MAD and MSE values are minimum for the second combination (alpha = 0.4, beta = 0.2, gamma = 0.2), the second combination is the optimal choice.