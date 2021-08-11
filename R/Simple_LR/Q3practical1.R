data3 = read.table('heights.txt')
plot(data3$Mheight, data3$Dheight)
model3 = lm(Dheight~Mheight, data = data3)
abline(model3)
conf_beta3 = confint(model3, level = 0.99)
s3 = summary(model3)
s3$r.squared
new_data1 = data.frame(Mheight = c(64))
pred = predict(model3, interval = 'prediction', level = 0.99, newdata = new_data1)