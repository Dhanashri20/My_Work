d3 = read.csv('marks.csv')
head(d3)
m3 = lm(y~x1+x2, data = d3)
s4 = summary(m3)
s4$r.squared
s4$adj.r.squared