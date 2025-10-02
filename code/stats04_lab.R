pacman::p_load(tidyverse,
               patchwork,
               here)

#Make datasets
xs<-rnorm(mean=10, sd = 5, n = 10)
ys<-rnorm(mean=12, sd = 5, n = 10)
xl<-rnorm(mean = 10, sd = 5, n = 100)
yl<-rnorm(mean = 12, sd = 5, n = 100)

#2 sample t-tests
t.test(xs, ys, var.equal = TRUE)
t.test(xl, yl, var.equal = TRUE)

#figure
a1 <- c(13.9, 14.9 ,13.4, 14.3, 11.8, 13.9, 14.5, 15.1, 13.3, 13.9)
a2 <- c(17.4, 17.3, 20.1, 17.2, 18.4, 19.6, 16.8, 18.7, 17.8, 18.9)

b1 <- c(10.9, 20.3, 9.6, 8.3, 14.5, 12.3, 14.5, 16.7, 9.3, 22.0)
b2 <- c(26.9, 12.9, 11.1, 16.7, 20.0, 20.9, 16.6, 15.4, 16.2, 16.2)

fig_dat<-tibble("group" = c(rep("a1", times = length(a1)),
                            rep("a2", times = length(a2)),
                            rep("b1", times = length(b1)),
                            rep ("b2", times = length(b2))),
                "value" = c(a1, a2, b1, b2))


fig_dat_mu <- fig_dat %>% 
  group_by(group) %>% # group operation
  summarize(mu_l = mean(value), # summarize by mean()
            sd_l = sd(value)) # summarize with sd()

fig_dat %>% 
  filter(group %in% c("a1", "a2")) %>% 
  ggplot(aes(x = group,
             y = value)) +
  geom_jitter(width = 0.1, # scatter width
              height = 0, # scatter height (no scatter with zero)
              alpha = 0.25) + # transparency of data points
  geom_segment(data =fig_dat_mu %>% 
                 filter(group %in% c("a1", "a2")), # switch data frame
               aes(x = group,
                   xend = group,
                   y = mu_l - sd_l,
                   yend = mu_l + sd_l)) +
  geom_point(data = fig_dat_mu %>% 
               filter(group %in% c("a1", "a2")),# switch data frame
             aes(x = group,
                 y = mu_l),
             size = 3) +
  labs(x = "Group", # x label
       y = "Value") # Y label

#t-test again
t.test(a1, a2)
t.test(b1, b2)
