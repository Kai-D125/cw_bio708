pacman::p_load(tidyverse,
               patchwork,
               here)

# GLM excersize -----------------------------------------------------------

url <- "https://raw.githubusercontent.com/aterui/public-proj_fish-richness-shubuto/master/data/data_vpart.csv"
df_fish <- read_csv(url)

m_fish <- glm(n_sp ~ distance + cat_area + hull_area,
              data = df_fish, 
              family = "poisson")
summary(m_fish)

m_car <- glm(cbind(am, 1 - am) ~ mpg + hp + wt,
             data = mtcars, 
             family = "binomial")
summary(m_car)

gauss_m_car <- glm(am ~ mpg + hp + wt,
             data = mtcars, 
             family = "gaussian")
summary(gauss_m_car)


# effect size -------------------------------------------------------------

df_fish <- df_fish %>% 
  mutate(std_dist = scale(distance),
         std_cat = scale (cat_area),
         std_hull = scale(hull_area))

std_m_fish <- glm(n_sp ~ std_dist + std_cat + std_hull,
    data = df_fish, 
    family = "poisson")
summary(std_m_fish)

# offset ------------------------------------------------------------------

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_offset.csv"
df_offset <- read_csv(url)

ggplot(df_offset,
       aes(x = count,
           y = nitrate)) +
  geom_point()

ggplot(df_offset,
       aes(x = area,
           y = count)) +
  geom_point()

df_offset <- df_offset %>% 
  mutate(density = count / area)

ggplot(df_offset,
       aes(x = density,
           y = nitrate)) +
  geom_point()

n_glm_1 <- glm(density ~ nitrate,
    data = df_offset,
    family = "poisson")
summary(n_glm_1)

n_glm_2 <- glm(count ~ nitrate,
    data = df_offset,
    family = "poisson")
summary(n_glm_2)

n_glm_3 <- glm(count ~ nitrate + offset(log(area)),
    data = df_offset,
    family = "poisson")
summary(n_glm_3)

# overdispersion ----------------------------------------------------------

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_raw/data_tadpole.csv"
df_tadpole <- read_csv(url)

g_v <- ggplot(df_tadpole,
       aes(x = aqveg,
           y = tadpole)) +
  geom_point()

g_p <- ggplot(df_tadpole,
       aes(x = permanence,
           y = tadpole)) +
  geom_point()

tad_glm <- glm(tadpole ~ aqveg + permanence,
               data = df_tadpole,
               family = "poisson")
summary(tad_glm)

#poisson assumes mean = variance
mean(df_tadpole$tadpole)
var(df_tadpole$tadpole)

#negative binomial regression
m_nb <- MASS::glm.nb(tadpole ~ aqveg + permanence,
             data = df_tadpole)
summary(m_nb)
