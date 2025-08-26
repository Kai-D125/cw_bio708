
# example code ------------------------------------------------------------


x <- c(1, 2)
x

y <- c(3, 4)
y

## produce 100 random numbers that follows a normal distribution
x <- rnorm(100, mean = 0, sd = 1)

## estimate mean
mean(x)

## estimate SD
sd(x)

# excersize ---------------------------------------------------------------

#Create a vector with length 10 assign it to z
z<-1:10

#Create a numeric matrix with 2 rows 2 columns, assign it to m
m<-matrix(data=1:4, nrow = 2, ncol = 2)

#data frame
df0<-data.frame(name = c("Smith", "John", "Kate", "Akira"), height = c(154, 170, 156, 175))
