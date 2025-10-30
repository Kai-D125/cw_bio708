pacman::p_load(tidyverse,
               patchwork,
               here)

# Question 1 --------------------------------------------------------------

df_teeth <- ToothGrowth

m_tooth <- lm(len~supp * dose,
              data = df_teeth)

#the shapiro-wilk test should be used on the residuals

eps <- residuals(m_tooth)
shapiro.test(eps)

# Question 2 --------------------------------------------------------------

df_pred <- ToothGrowth %>%
  group_by(supp) %>%
  reframe(dose = seq(min(dose),
                     max(dose),
                     length = 100))

y_pred <- predict(m_tooth,
                  newdata = df_pred)

df_pred <- df_pred %>% 
  mutate(y_pred = y_pred)

ggplot(df_teeth,
       aes(x = dose,
           y = len,
           color = supp)) +
  geom_point(alpha = 0.5)+
  geom_line(data = df_pred,
            aes(y = y_pred))

# Question 3 --------------------------------------------------------------

## variance-covariance matrix
mv <- rbind(c(1, 0.9),
            c(0.9, 1))

## true regression coefficients
b <- c(0.05, 1.00)

## produce simulated data
set.seed(523)
X <- MASS::mvrnorm(100,
                   mu = c(0, 0),
                   Sigma = mv)

df_y <- tibble(x1 = X[,1],
               x2 = X[,2]) %>% 
  mutate(y = rnorm(nrow(.),
                   mean = 1 + b[1] * x1 + b[2] * x2))

ggplot(df_y,
       aes(x = x1,
           y = y,)) +
  geom_point()

ggplot(df_y,
       aes(x = x2,
           y = y,)) +
  geom_point()

lm(y~x1 + x2,
   data = df_y)

ggplot(df_y,
       aes(x = x2,
           y = x1,)) +
  geom_point()

cor(df_y, y = NULL, method =  "pearson")

# Question 4 --------------------------------------------------------------

#B, you can measure the effects of nutrients and predation separately, 
#but not their interaction
