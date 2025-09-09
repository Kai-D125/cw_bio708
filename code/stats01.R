#ctrl+shift+n=new script file

library(tidyverse)

# central tendency --------------------------------------------------------
# ctrl+shift+r= section

#arithmetic mean
#calculate the arithmetic mean using length() and sum()
v_x<-rnorm(10)
mu_x<-sum(v_x)/length(v_x)

#geometric mean
#use prod(), length(), and ^
v_y <- runif(10, min = 10, max = 20)
prod(v_y)^(1/length(v_y))

exp(mean(log(v_y)))

#median
v_z<-runif(9, min = 10, max = 20)
v_z<-sort(v_z)
index<-(length(v_z)+1)/2
v_z[index]
median(v_z)

# variance measures -------------------------------------------------------

#variance
#use sum and length to define variance
v_a<-rnorm(100)
s2<-sum((v_a-mean(v_a))^2)/length(v_a)
s <- sqrt(s2)

#interquantile
a_l <-quantile(v_a, probs = 0.25)
a_h <-quantile(v_a, probs = 0.75)
iqr<-abs(a_l-a_h)

 #MAD
median(abs(v_a-median(v_a)))

#coefficient of variation
#use s and mean() of v_a to define CV
v_b<-runif(100, min = 10, max = 20)
s2<-sum((v_b-mean(v_b))^2)/length(v_b)
s<-sqrt(s2)

cv <-s/mean(v_b)

#MAD/Median
mad<-median((abs(v_b-median(v_b))))
med<-median(v_b)
mad2med<-mad/med