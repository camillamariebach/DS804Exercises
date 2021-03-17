#### Exerccise 6.6

vector <- c(1,-2,3,-4,5)

mean <- mean(vector)
max <- max(vector)
min <- min(vector)
abs <- mean(abs(vector))

vector[3] <- 42

new_vector <- c(6,7,8,9,11)
sum <- vector + new_vector

rand_vec <- c(rnorm(50))
rand_mean <- mean(rand_vec)

rand_last <- rand_vec[46:50]

## Exercise 6.7

a <- c(5,9)
b <- c(7,3)
A <- rbind(a, b)

B <- -A

nullify <- A + B

d <- c(2,0)
e  <- c(0,2)
D <- rbind(d, e)

doubleA <- A %*% D

## Exercise 6.8

help("AirPassengers")

data <- AirPassengers
plot <- plot(AirPassengers)
hist <- hist(AirPassengers, breaks = 28)

class(AirPassengers)
mode(AirPassengers)

help("Titanic")

mosaicplot(Titanic)



### Exercise 6.9

dataframe <-iris

help(iris)

attributes(dataframe)

k <- kmeans(dataframe[, c(1,2,3,4)], centers= 3, iter.max = 20, algorithm = "Lloyd")

labels <- k["cluster"]

dataframe$Species <- data.frame(labels)

dataframe

# find mean and variance for the diffferent species

setosa_df <-subset(iris, Species=="setosa") 
versicolor_df <- subset(iris, Species=="versicolor")
virginica_df <- subset(iris, Species=="virginica")

setosa_sepal_length <- mean(setosa_df$Sepal.Length)
setosa_sepal_width <- mean(setosa_df$Sepal.Width)
setosa_petal_length <- mean(setosa_df$Petal.Length)
setosa_petal_width <- mean(setosa_df$Petal.Width)

setosa_sepal_length_sd <- sd(setosa_df$Sepal.Length)
setosa_sepal_width_sd <- sd(setosa_df$Sepal.Width)
setosa_petal_length_sd <- sd(setosa_df$Petal.Length)
setosa_petal_width_sd <- sd(setosa_df$Petal.Width)

setosa_sepal_l <- rnorm(50, mean = setosa_sepal_length, sd = setosa_sepal_length_sd)
setosa_sepal_w <- rnorm(50, mean = setosa_sepal_width, sd = setosa_sepal_width_sd)
setosa_petal_l <- rnorm(50, mean = setosa_petal_length, sd = setosa_petal_length_sd)
setosa_petal_w <- rnorm(50, mean = setosa_petal_width, sd = setosa_petal_width_sd)
setosa_species <- rep('setosa', 50)

versi_sepal_length <- mean(versicolor_df$Sepal.Length)
versi_sepal_width <- mean(versicolor_df$Sepal.Width)
versi_petal_length <- mean(versicolor_df$Petal.Length)
versi_petal_width <- mean(versicolor_df$Petal.Width)

versi_sepal_length_sd <- sd(versicolor_df$Sepal.Length)
versi_sepal_width_sd <- sd(versicolor_df$Sepal.Width)
versi_petal_length_sd <- sd(versicolor_df$Petal.Length)
versi_petal_width_sd <- sd(versicolor_df$Petal.Width)

versi_sepal_l <- rnorm(50, mean = versi_sepal_length, sd = versi_sepal_length_sd)
versi_sepal_w <- rnorm(50, mean = versi_sepal_width, sd = versi_sepal_width_sd)
versi_petal_l <- rnorm(50, mean = versi_petal_length, sd = versi_petal_length_sd)
versi_petal_w <- rnorm(50, mean = versi_petal_width, sd = versi_petal_width_sd)
versicolor_species <- rep('versicolor', 50)

virgi_sepal_length <- mean(virginica_df$Sepal.Length)
virgi_sepal_width <- mean(virginica_df$Sepal.Width)
virgi_petal_length <- mean(virginica_df$Petal.Length)
virgi_petal_width <- mean(virginica_df$Petal.Width)

virgi_sepal_length_sd <- sd(virginica_df$Sepal.Length)
virgi_sepal_width_sd <- sd(virginica_df$Sepal.Width)
virgi_petal_length_Sd <- sd(virginica_df$Petal.Length)
virgi_petal_width_sd <- sd(virginica_df$Petal.Width)

virgi_sepal_l <- rnorm(50, mean = virgi_sepal_length, sd = virgi_sepal_length_sd)
virgi_sepal_w <- rnorm(50, mean = virgi_sepal_width, sd = virgi_sepal_width_sd)
virgi_petal_l <- rnorm(50, mean = virgi_petal_length, sd = virgi_petal_length_Sd)
virgi_petal_w <- rnorm(50, mean = virgi_petal_width, sd = virgi_petal_width_sd)
virginica_species <- rep('virginica', 50)

fake_setosa = cbind(setosa_sepal_l, setosa_sepal_w, setosa_petal_l, setosa_petal_w)
fake_versicolor <- cbind(versi_sepal_l, versi_sepal_w, versi_petal_l, versi_petal_w)
fake_virginica <- cbind(virgi_sepal_l, virgi_sepal_w, virgi_petal_l, virgi_petal_w)
spec <- (c(setosa_species, versicolor_species, virginica_species))

fake_flowers <- rbind(fake_setosa, fake_versicolor, fake_virginica)

fake_df <- data.frame(fake_flowers)
fake_df$species <- spec
names(fake_df) <- c('SepalLength', 'SepalWidth', 'PetalLength', 'PetalWidth', 'Species')


# add noise
dataframe_noise <- addNoise(fake_df[, c(1,2,3,4)], noise = 10, method = "correlated")

noise <- dataframe_noise$xm
noise$Species <- fake_df$Species


# D
## Dataframe with kmeans- generated labels
nor <-function(x) {(x -min(x))/(max(x)-min(x))}

iris_norm <- as.data.frame(lapply(dataframe[,c(1,2,3,4)], nor))

samplesize <- floor(0.8 * nrow(iris_norm))
ran <- sample(seq_len(nrow(dataframe)), size = samplesize) 

iris_train <- iris_norm[ran,]
iris_test <- iris_norm[-ran,]

iris_train
iris_test

## Set target from known labels
iris_train_labels <- dataframe$Species[ran,1]
iris_train_labels
## Set test target from known labels
iris_test_labels <- dataframe$Species[-ran,1]
iris_test_labels
length(iris_test_labels)

library(class)
## Run KNN, K = 3

k3 <- knn(iris_train,iris_test,cl=iris_train_labels,k=3)

tab <- table(k3,iris_test_labels)

tab

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)

## Run KNN, K = 6
k6 <- knn(iris_train,iris_test,cl=iris_train_labels,k=6)

tab <- table(k6,iris_test_labels)


accuracy(tab)

## Run KNN, K = 10
k10 <- knn(iris_train,iris_test,cl=iris_train_labels,k=10)

tab <- table(k10,iris_test_labels)

accuracy(tab)


# With real iris data
df <- iris
ran <- sample(1:nrow(df), 0.9 * nrow(df)) 

iris_norm <- as.data.frame(lapply(df[,c(1,2,3,4)], nor))


##extract training set
iris_train <- iris_norm[ran,] 
##extract testing set
iris_test <- iris_norm[-ran,] 
##extract 5th column of train dataset because it will be used as 'cl' argument in knn function.
iris_target_category <- iris[ran,5]
##extract 5th column if test dataset to measure the accuracy
iris_test_category <- iris[-ran,5]

## Run KNN, K = 3
pr_real_5 <- knn(iris_train,iris_test,cl=iris_target_category,k=5)

tab <- table(pr_real_5,iris_test_category)

accuracy(tab)

## Run KNN, K = 7
pr_real_7 <- knn(iris_train,iris_test,cl=iris_target_category,k=7)

tab <- table(pr_real_7,iris_test_category)
tab

accuracy(tab)

pr_real_7

# Knn on fake data set
fake_norm <- as.data.frame(lapply(fake_df[,c(1,2,3,4)], nor))

fake_train <- fake_norm[ran,]
fake_test <- fake_norm[-ran,]

fake_train_labels <- fake_df[ran, 5]
fake_test_labels <- fake_df[-ran, 5]

# Run Knn, k = 5
fake_knn_5 <- knn(fake_train, fake_test, cl=fake_train_labels, k = 5)
tab <- table(fake_knn_5, fake_test_labels)
tab
accuracy(tab)


# Run Knn, k = 10
fake_knn_10 <- knn(fake_train, fake_test, cl=fake_train_labels, k = 10)
tab <- table(fake_knn_10, fake_test_labels)
tab
accuracy(tab)

# Run Knn, k = 3
fake_knn_3 <- knn(fake_train, fake_test, cl=fake_train_labels, k = 3)
tab <- table(fake_knn_3, fake_test_labels)
tab
accuracy(tab)
