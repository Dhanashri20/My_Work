#PRACTICAL 6

#QUESTION 1

datap6q1 <- read.table("non_farm_income.txt")
head(datap6q1)

y = datap6q1[,1]

#(a)

y.ts <- ts(y, start = 1948, end = 1979, frequency = 4)

#(b)

BPtest <- Box.test(y.ts, lag = 20, type = "Box-Pierce")
BPtest$statistic
BPtest$p.value
#CONCLUSION:

LBtest <- Box.test(y.ts, lag = 20, type = "Ljung-Box")
LBtest$statistic
LBtest$p.value
#CONCLUSION:

#(c)

acfplot <- acf(y.ts)
acfplot

pacfplot <- pacf(y.ts)
pacfplot

#INTERPRETATION:

#(d)

df1 <- diff(y.ts, 1)
head(df1)

df2 <- diff(y.ts, 2)
head(df2)

#(e)

acfdf1 <- acf(df1)
pacfdf1 <- pacf(df1)

acfdf2 <- acf(df2)
pacfdf2 <- pacf(df2)

#INTERPRETATION:


######################################


#QUESTION 2

datap6q2 <- BJsales
head(datap6q2)

#(a)

plot(datap6q2)

#(b)

acfplot <- acf(datap6q2)
acfplot

pacfplot <- pacf(datap6q2)
pacfplot

#CONCLUSIONS: 

#(c)

df1 <- diff(datap6q2, 1)
v_df1 <- var(df1)

df2 <- diff(datap6q2, 2)
v_df2 <- var(df2)

data.frame(DifferenceOrder = c(1, 2), Variance = c(v_df1, v_df2))

#Since First Order Difference gives MINIMUM variance, First Order Difference is more appropriate for the data.

#(d)

acfdf1 <- acf(df1)
pacfdf1 <- pacf(df1)

acfdf2 <- acf(df2)
pacfdf2 <- pacf(df2)




