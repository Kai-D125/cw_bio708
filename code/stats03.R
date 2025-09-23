# load csv data on R
library(tidyverse)

df_h0 <- read_csv(here::here("data_raw/data_plant_height.csv"))
df_h0 %>% 
  ggplot(aes(x = height)) + 
  geom_histogram(binwidth = 1, # specify bin width
                 center = 0.5) + # bin's center specification
  geom_vline(aes(xintercept = mean(height))) # draw vertical line at the mean
#Normal Distribution
# vector of x values
# seq() generate min to max values with specified numbers of elements or interval
# the following produce 100 elements
x <- seq(min(df_h0$height), max(df_h0$height), length = 100)
mu<-mean(df_h0$height)
sigma<-sd(df_h0$height)
pd<-dnorm(x, mean = mu, sd = sigma)

tibble(y = pd,
       x = x) %>% 
  ggplot(aes(x = x, y = y))+
  geom_line()+
  labs(y = "Probability Density",
       x = "Plant Height")
##probability
p10<-pnorm(10, mean = mu, sd = sigma)
p20<-pnorm(20, mean = mu, sd = sigma)
p20-p10
p20_p10<-p20-p10

x_min <- floor(min(df_h0$height)) # floor takes the integer part of the value
x_max <- ceiling(max(df_h0$height)) # ceiling takes the next closest integer
bin <- seq(x_min, x_max, by = 1) # each bin has 1cm

p <- NULL # empty object for probability
for (i in 1:(length(bin) - 1)) {
  p[i] <- pnorm(bin[i+1], mean = mu, sd = sigma) -
    pnorm(bin[i], mean = mu, sd = sigma)
}
# data frame for probability
# bin: last element [-length(bin)] was removed to match length
# expected frequency in each bin is "prob times sample size"
# "+ 0.5" was added to represent a midpoint in each bin
df_prob <- tibble(p, 
                  bin = bin[-length(bin)] + 0.5) %>% 
  mutate(freq = p * nrow(df_h0))
df_h0 %>% 
  ggplot(aes(x = height)) + 
  geom_histogram(binwidth = 1, # specify bin width; must match the bin width used for probability
                 center = 0.5) + # bin's center position
  geom_point(data = df_prob,
             aes(y = freq,
                 x = bin),
             color = "salmon") +
  geom_line(data = df_prob,
            mapping = aes(y = freq,
                          x = bin),
            color = "salmon")

#Poisson distribution
df_count <- read_csv(here::here("data_raw/data_garden_count.csv"))
print(df_count)

## histogram
df_count %>% 
  ggplot(aes(x = count))+
  geom_histogram(binwidth = 0.5, 
                 center = 0)

# vector of x values
# create a vector of 0 to 10 with an interval one
# must be integer of > 0
x <- seq(0, 10, by = 1)
lambda_hat<-mean(df_count$count)
pm<-dpois(x, lambda = lambda_hat)

# figure
df_prob <- tibble(x = x, y = pm) %>% 
  mutate(freq = y * nrow(df_count)) # prob x sample size

df_count %>% 
  ggplot(aes(x = count)) +
  geom_histogram(binwidth = 0.5, # must be divisible number of one; e.g., 0.1, 0.25, 0.5...
                 center = 0) +
  geom_line(data = df_prob,
            aes(x = x,
                y = freq),
            linetype = "dashed") +
  geom_point(data = df_prob,
             aes(x = x,
                 y = freq))

