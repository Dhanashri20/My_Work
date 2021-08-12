d2 = read.csv('soap.csv')
head(d2)
new = d1[,-1]
plot(new)
m3 = lm(y~x1+x2, data = new)
s2 = summary(m3)
s2$r.squared
s2$adj.r.squared
conf_int = confint(m3,interval = 0.90)
new_data = read.csv('test.csv',header=TRUE)
datanew = new_data[,-1]
pred_new = predict(m3,newdata = datanew,interval = 'confidence',level = 0.90)
