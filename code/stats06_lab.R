pacman::p_load(tidyverse,
               patchwork,
               here)


# dataframe creation ------------------------------------------------------

df_all <- iris

df_setosa <- filter(df_all, Species == "setosa")
df_virginica <- filter(df_all, Species == "virginica")
df_versicolor <- filter(df_all, Species == "versicolor")


# lm analysis -------------------------------------------------------------

m_set <- lm(Sepal.Width~Petal.Width, data = df_setosa)
m_vir <- lm(Sepal.Width~Petal.Width, data = df_virginica)
m_ver <- lm(Sepal.Width~Petal.Width, data = df_versicolor)

summary(m_set)
summary(m_vir)
summary(m_ver)


# exercise 2 --------------------------------------------------------------

m_set_v1 <- lm(Sepal.Width~Petal.Width, data = df_setosa)
m_set_v2 <- lm(Sepal.Width~Petal.Width + Petal.Length, data = df_setosa)

summary(m_set_v1)
summary(m_set_v2)

#m_set_v2 gave a higher R^2 value

# exercise 3 --------------------------------------------------------------

df_x <- rnorm(nrow(iris), mean = 0, sd = 1)
df_all <- df_all %>% 
  mutate(x = df_x)

df_setosa <- filter(df_all, Species == "setosa")

m_set_x <- lm(Sepal.Width~Petal.Width + x, data = df_setosa)

summary(m_set_x)

