pacman::pload(tidyverse,
              patchwork,
              here)

set.seed(1)

#hypothetical sample size
n <- 100

#true intercept and slope
b <- c(0.1, 0.5)

#simulated predictor
x1 <- rnorm(n = n, mean = 0, sd = 1)

#design matrix
X <- model.matrix(~ x1)

#get simulated y
y_hat <- X %*% b

plot(y_hat ~ x1)

y <- rnorm(n = n, mean = y_hat, sd = 0.5)

df0 <- tibble(y = y,
              x1 = x1)

ggplot(df0,
       aes(y = y,
           x = x1))+
  geom_point()

## develop linear model
m1 <- lm(y ~ x1, data = df0)
summary(m1)

df0 <- df0 %>% 
  mutate(x2 = rnorm(nrow(.)))

df0 %>%
  ggplot(aes(x = x2,
             y = y))+
  geom_point()

m2 <- lm(y ~ x1 + x2,
         data = df0)
summary(m2)

m1 <- lm(y ~ x1, data = df0)
summary(m1)

m2 <- lm(y ~ x1 + x2,
         data = df0)
summary(m2)

m3 <- lm(y ~ x2,
         data = df0)
summary(m3)

# likelihood ratio --------------------------------------------------------

#deviance
dev_m1 <- -2 *logLik(m1)

dev_m2 <- -2 *logLik(m2)

log_lr <- dev_m1 - dev_m2

anova(m1, m2, test = "Chisq")

# AIC ---------------------------------------------------------------------

AIC(m1)
AIC(m2)

#multimodel inference with AIC

#install.packages("MuMIn")

library(MuMIn)

url <- "https://raw.githubusercontent.com/aterui/public-proj_fish-richness-shubuto/master/data/data_vpart.csv"
df_fish <- read_csv(url)

glm(n_sp ~ distance + cat_area + hull_area,
    data = df_fish,
    family = "poisson")

#magic spell
options(na.action = "na.fail")

#model selection table
mst <- dredge(m_full,
       rank = "AIC")

#don't use AIC for experimental data, best for field observations
#correlation, not causation