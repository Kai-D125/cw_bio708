library(tidyverse)

set.seed(123)

iris_sub<-as_tibble(iris) %>%
  group_by(Species) %>%
  sample_n(3) %>%
  ungroup()

print(iris_sub)

#filter rows
filter(iris_sub, Species == "virginica")
filter(iris_sub, species %in% c("virginica", "versicolor"))
filter(iris_sub, Species != "virginica")
filter(iris_sub, !(Species %in% c("virginica", "versicolor")))
filter(iris_sub, Sepal.Length > 5)
filter(iris_sub, Sepal.Length >= 5)
filter(iris_sub, Sepal.Length < 5)
filter(iris_sub, Sepal.Length <= 5)
# Sepal.Length is less than 5 AND Species equals "setosa"
filter(iris_sub,
       Sepal.Length < 5 & Species == "setosa")
# same; "," works like "&"
filter(iris_sub,
       Sepal.Length < 5, Species == "setosa")
# Either Sepal.Length is less than 5 OR Species equals "setosa"
filter(iris_sub,
       Sepal.Length < 5 | Species == "setosa")
arrange(iris_sub, Sepal.Length)
arrange(iris_sub, desc(Sepal.Length))
#excersize
iris3<-filter(iris_sub, Sepal.Length>3.0)
iris_setosa<-filter(iris_sub, Species == setosa)
iris_3_setosa<-filter(iris_sub, Species == setosa, Sepal.Length>3.0)
#column Manipulation
select(iris_sub, Sepal.Length)
select(iris_sub, c(Sepal.Length, Sepal.Width))
select(iris_sub, -Sepal.Length)
select(iris_sub, -c(Sepal.Length, Sepal.Width))
# select columns starting with "Sepal"
select(iris_sub, starts_with("Sepal"))
# remove columns starting with "Sepal"
select(iris_sub, -starts_with("Sepal"))
# select columns ending with "Sepal"
select(iris_sub, ends_with("Width"))
# remove columns ending with "Sepal"
select(iris_sub, -ends_with("Width"))
# nrow() returns the number of rows of the dataframe
(x_max <- nrow(iris_sub))
# create a vector from 1 to x_max
x <- 1:x_max

# add as a new column
# named `x` as `row_id` when added
mutate(iris_sub, row_id = x)
# twice `Sepal.Length` and add as a new column
mutate(iris_sub, sl_two_times = 2 * Sepal.Length)
#Excersize TO BE FINISHED
iris_pw<-select(iris_sub, c(Peltal.Width, Sepal.Length))
#Piping
iris_sub %>% 
  select(Sepal.Length) %>% 
  mutate(twice = 2*Sepal.Length)
