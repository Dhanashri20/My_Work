data1 = read.csv('insurance.csv')
head(data1)
plot(data1$TVadverts, data1$Quotes)
model1 = lm(Quotes~TVadverts, data=data1)
abline(model1)
b = model1$coefficients
e = model1$residuals
sum(e)
s1 = summary(model1)
s1$r.squared
conf_beta1 = confint(model1, level = 0.95)