library(tidyverse)
library(patchwork)

# Normal Distribution -----------------------------------------------------

norm_dist<-rnorm(50, mean = 100, sd = 5)

norm_min <- floor(min(norm_dist)) 
norm_max <- ceiling(max(norm_dist))
bin <- seq(norm_min, norm_max, by = 1)
mu<-mean(norm_dist)
sigma<-sd(norm_dist)

pnorm(bin[2], mean = mu, sd = sd(norm_dist)) -
        pnorm(bin[1], mean = mu, sd = sd(norm_dist))

p <- NULL
for (i in 1:(length(bin) - 1)) {
  p[i] <- pnorm(bin[i+1], mean = mu, sd = sigma) -
    pnorm(bin[i], mean = mu, sd = sigma)
}

df_prob<- tibble(bin = bin[-length(bin)]+0.5,
       prob = p) %>% 
  mutate(freq = length(norm_dist)*prob)
       
df_norm=tibble(v = norm_dist)

df_norm %>% 
  ggplot(aes(x = v)) + 
  geom_histogram() + 
  geom_point(data = df_prob,
             aes(y = freq,
                 x = bin),
             color = "purple") +
  geom_line(data = df_prob,
            mapping = aes(y = freq,
                          x = bin),
            color = "purple")

# Poisson Distribution ----------------------------------------------------

pois_dist<-rpois(1000, lambda = 10)

bin<-seq(0,
         max(pois_dist)+5,
         by = 1)

prob_pois<-dpois(bin, lambda = mean(pois_dist))

df_pois<- tibble(bin = bin, prob = prob_pois) %>% 
  mutate(freq = length(pois_dist) * prob_pois)

df_base_pois<-tibble(x=pois_dist)

#Genuinely confused about the following, how do I get the raw data to match length with the rest

#df_pois %>% 
 # ggplot(aes(x = df_base_pois)) +
 # geom_histogram(binwidth = 0.5, 
                 #center = 0) +
 # geom_line(data = df_pois,
           # aes(x = df_base_pois,
                #y = freq),
           # linetype = "dashed") +
 # geom_point(data = df_pois,
             #aes(x = prob,
                 #y = freq))
