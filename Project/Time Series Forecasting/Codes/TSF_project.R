library(aprean3)
library(olsrr)
stocks = read.csv('Stocks.csv')
head(stocks)
model_stocks = lm(Nifty50~Nasdaq+Dow.Jones+Nikkei225+SSE+LSE , data = stocks)
model_stocks
summary(model_stocks)
k = ols_step_all_possible(model_stocks)
k2 = cbind(k, k$aic, k$sbic)
k3 = k2[, c(1:5,8,9)]
plot(k)
k_forward = ols_step_forward_aic(model_stocks, details = TRUE)
k_backward = ols_step_backward_aic(model_stocks, details = TRUE)
k_both = ols_step_both_aic(model_stocks, details = TRUE)
