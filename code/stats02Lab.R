#Sampling Lab
library(tidyverse)
library(patchwork)
#100 Datasets
ph_master<-read_csv("~/GitHub/cw_bio708/data_raw/data_plant_height.csv")
mu50_i<-var50_i<-NULL

for (i in 1:100) {
  ph_50 <- ph_master %>% 
    sample_n(size = 50)
  
  mu50_i[i]<-mean(ph_50$height)
  var50_i[i]<-var(ph_50$height)
}
mu100_i<-var100_i<-NULL

for (i in 1:100) {
  ph_100<-ph_master %>% 
    sample_n(size = 100)
  
  mu100_i[i]<-mean(ph_100$height)
  var100_i[i]<-var(ph_100$height)
}

ph_est<-tibble(mu100=mu100_i,
               var100=var100_i,
               mu50=mu50_i,
               var50=var50_i)
#Histograms
mu<-mean(ph_master$height)
var<-var(ph_master$height)
g_ph_50<-ph_est %>% 
  ggplot(aes(x = mu50))+
  geom_histogram()+
  geom_vline(xintercept = mu)+
  scale_x_continuous(limits = c(18, 22))

g_ph_100<-ph_est %>% 
  ggplot(aes(x = mu100))+
  geom_histogram()+
  geom_vline(xintercept = mu)+
  scale_x_continuous(limits = c(18, 22))

g_ph_50v<-ph_est %>% 
  ggplot(aes(x = var50))+
  geom_histogram()+
  geom_vline(xintercept = var)+
  scale_x_continuous(limits = c(18, 35))

g_ph_100v<-ph_est %>% 
  ggplot(aes(x = var100))+
  geom_histogram()+
  geom_vline(xintercept = var)+
  scale_x_continuous(limits = c(18, 35))

g_ph_100/g_ph_50
g_ph_100v/g_ph_50v