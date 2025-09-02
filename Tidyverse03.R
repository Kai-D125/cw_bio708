library(tidyverse)

set.seed(123)

iris_sub<-as_tibble(iris) %>%
  group_by(Species) %>%
  sample_n(3) %>%
  ungroup()

print(iris_sub)

# refresher ---------------------------------------------------------------


# exercise 1
# filter iris_sub to those with Sepal.Length greater than 5
# assign to 'df_g5'
df_g5<-filter(iris_sub, Sepal.Length > 5)
#excercise 2
#select columns of Sepal.Length and Petal.Width from iris_sub
#assign to df_sp
df_sp<-select(iris_sub, c(Sepal.Length, Petal.Width))
#excercise 3
#arrange  rows by Petal.Width in iris_sub
#assign to df_arrange
df_arrange<-arrange(iris_sub, Petal.Width)
#excercise 4
#do excercise 1-3 at once with pipes
#assign to 'df_master'
df_master<-iris_sub%>%
  filter(iris_sub, Sepal.Length > 5)%>%
  select(iris_sub, c(Sepal.Length, Petal.Width))%>%
  arrange(iris_sub, Petal.Width)

#extra
#calculate mean Petal.Width for each species seperately
#use group by and summarize functions
df_mean<-iris_sub%>%
  group_by(Species)%>%
  summarize(mean_pw = mean(iris_sub))


# ggplot ------------------------------------------------------------------

#basic syntax
##without pipe
g_example<-ggplot(data = iris,
       mapping = aes(x = Sepal.Length,
                     y = Sepal.Width)) + 
  geom_point()

##with pipe
g_example<-iris %>%
  ggplot(mapping = aes(x = Sepal.Length,
    y = Sepal.Width) +
      geom_point())

#color
g_col<-iris %>%
  ggplot(mapping = aes(x = Sepal.Length,
                       y = Sepal.Width,
         color = Species)) +
  geom_point()

#pitfall
g_col<-iris %>%
  ggplot(mapping = aes(x = Sepal.Length,
                       y = Sepal.Width),
         color = Species) +
           geom_point()

g_scol<-iris%>%
  ggplot(mapping = aes(x = Sepal.Length,
                       y = Sepal.Width)) +
  geom_point(color = "salmon")

## line plot
df0 <- tibble(x = rep(1:50, 3),
              y = x * 2)

df0 %>%
  ggplot(aes(x=x,
             y=y)) +
  geom_line()

#histogram
iris %>%
  ggplot(aes(x = Sepal.Length)) +
  geom_histogram()

#histogram colored by Species
iris %>%
  ggplot(aes(x = Sepal.Length,
             color=Species)) +
  geom_histogram()

#histogram filled by Species
iris %>%
  ggplot(aes(x = Sepal.Length,
             fill=Species)) +
  geom_histogram()

#boxplot
iris %>%
  ggplot(aes(y = Sepal.Length,
             x = Species)) +
  geom_boxplot()

#boxplot filled by species
iris %>%
  ggplot(aes(y = Sepal.Length,
             x = Species,
             fill = Species)) +
  geom_boxplot()

#use multiple layers
#example 1
iris %>%
  ggplot(aes(y=Sepal.Length,
             x=Species,
             fill = Species)) +
  geom_boxplot() +
  geom_point()
#example 2
iris %>%
  ggplot(aes(y=Sepal.Length,
             x=Species,
             fill = Species)) +
  geom_boxplot() +
  geom_jitter(alpha=0.5)