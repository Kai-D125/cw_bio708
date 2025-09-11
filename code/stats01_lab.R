library(tidyverse)
z<-exp(rnorm(n = 1000, mean = 0, sd = 0.1))
mean(z)
median(z)
prod(z)^(1/length(z))
dfz<-tibble(z)
z_hist<-dfz %>% 
  ggplot(aes(x = z)) +
  geom_histogram()
z_hist + geom_vline(xintercept = mean(z), color = "salmon") +
  geom_vline(xintercept = median(z), color = "skyblue") +
  geom_vline(xintercept = prod(z)^(1/length(z)), color = "green")
z_rev<--z + max(z) + 0.1
mean(z_rev)
median(z_rev)
prod(z_rev)^(1/length(z_rev))
dfzrev<-tibble(z_rev)
zrev_hist<-dfz %>% 
  ggplot(aes(x = z_rev)) +
  geom_histogram()
zrev_hist + geom_vline(xintercept = mean(z_rev), color = "salmon") +
  geom_vline(xintercept = median(z_rev), color = "skyblue") +
  geom_vline(xintercept = prod(z_rev)^(1/length(z_rev)), color = "green")
#use library(patchwork) to make multi-graphs
w <- rnorm(100, mean = 10, sd = 1)
head(w)
m<-1000*w
s2_w<-sum((w-mean(w))^2)/length(w)
s_w<-sqrt(s2_w)
s2_m<-sum((m-mean(m))^2)/length(m)
s_m<-sqrt(s2_m)
mad_w<-median(abs(w-median(w)))
mad_m<-median(abs(m-median(m)))
cv_w<-s_w/mean(w)
cv_m<-s_m/mean(m)
madr_w<-mad_w/median(w)
madr_m<-mad_m/median(m)