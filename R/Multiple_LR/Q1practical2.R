d1 = read.csv('wine.csv')
head(d1)
m1 = lm(Quality~Clarity+Aroma+Body+Flavor+Oakiness, data = d1)
#Testing overall regression
#m2 = lm(Quality~ .,data = d1 )
A1=anova(m1)
SS=A1$'Sum Sq'
SSR=sum(SS[1:5])
MSR=SSR/5
MSE=A1$'Mean Sq'[6]
F1=MSR/MSE
s2 = summary(m1)
F=S2$fstatistic
df1=F[2]
df2=F[3]
F_tab=qf(0.95,df1,df2)
s2$r.squared
s2$adj.r.squared
conf_int = confint(m1,level = 0.90)
