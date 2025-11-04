pacman::p_load(tidyverse,
               patchwork,
               here)

# count data --------------------------------------------------------------

df_count <- read_csv(here("data_raw/data_garden_count.csv"))

m_normal <- lm(count ~ nitrate,
   data = df_count)

summary(m_normal)

alpha <- coef(m_normal)[1]
beta <- coef(m_normal)[2]

ggplot(df_count) +
  geom_point(aes(x = nitrate,
                 y = count))+
  geom_abline(intercept = alpha,
              slope = beta)
#can't predict negative populations, but LMs do it anyway

## rng from poisson 
(y <- rpois(n = 10, lambda = 2))

#apply poisson using GLM
m_pois <- glm(count ~ nitrate,
    data = df_count, 
    family = "poisson")

summary(m_pois)

p_alpha <- coef(m_pois)[1]
p_beta <- coef(m_pois)[2]

ggplot(df_count) +
  geom_point(aes(x = nitrate,
                 y = count))+
  geom_abline(intercept = p_alpha,
              slope = p_beta)
#funky figure due to logarithmic function in poisson distribution

#poisson visualization
df_pred <- tibble(nitrate = seq(min(df_count$nitrate),
                     max(df_count$nitrate),
                     length = 100))
y_pred <- predict(m_pois,
        newdata = df_pred) %>% 
  exp()

df_pred <-df_pred %>% 
  mutate(y = y_pred)

ggplot(df_count,
  aes(x = nitrate,
      y = count))+
  geom_point()+
  geom_line(data = df_pred,
          aes(y = y))

## compare summary output from m_normal and m_pois

summary(m_normal)
summary(m_pois)

# proportional data -------------------------------------------------------

df_mussel <- read_csv(here("data_raw/data_mussel.csv"))

df_mussel <- df_mussel %>% 
  mutate(prop_fert = (n_fertilized / n_examined))

ggplot(df_mussel,
         aes(x = density,
             y = prop_fert)) +
  geom_point()

cbind(df_mussel$n_fertilized, df_mussel$n_examined - df_mussel$n_fertilized)

#binomial model
m_binom <- glm(cbind(n_fertilized, n_examined - n_fertilized) ~ density,
    data = df_mussel,
    family = "binomial")

summary(m_binom)

#how logit works
df_test <- tibble(logit_x = seq(-10, 10, length = 100), 
       x = exp(logit_x) / (1 + exp(logit_x)))

df_test %>% 
  ggplot(aes(x = logit_x,
             y = x))+
  geom_point() +
  geom_line()
  