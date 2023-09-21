# Load necessary libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(caTools)
library(randomForest)

# Read the Iris dataset from a CSV file
iris <- read.csv("internship tasks/task 2/Iris Dataset/iris.csv")

# Explore the dataset's structure
str(iris)

dim(iris)
# View the first and last few rows of the dataset
head(iris)
tail(iris)

# Check for any missing values in the dataset
any(is.na(iris))

#Finding the summary of the data
table(iris$sepal_length)
summary(iris)
names(iris)
sd(iris$petal_length)
var(iris$sepal_width)

# Create a histogram of sepal width, colored by species
ggplot(iris, aes(x = sepal_width, fill = species)) + 
  geom_histogram()

# Create a plot of petal length
plot(iris$petal_length)


# Create a box plot of sepal length by species with jittered points
ggplot(data = iris) +
  aes(x = species, y = sepal_length, color = species) +
  geom_boxplot() +
  geom_jitter(position = position_jitter(0.2))

# Create a density plot of petal length by species
ggplot(data = iris) +
  aes(x = petal_length, fill = species) +
  geom_density(alpha = 0.3) 


# Create a density plot of sepal width by species, faceted by species
ggplot(data = iris) +
  aes(x = sepal_width, fill = species) +
  geom_density(alpha = 0.3) +
  facet_wrap(~species, nrow = 3)

# Create a box plot of sepal width by species
ggplot(data = iris) +
  aes(x = species, y = sepal_width, color = species) +
  geom_boxplot()

# Convert the "species" column to a factor (categorical) variable
iris$species <- as.factor(iris$species)

# Train a random forest classifier using specified features
RandomForest_model <- randomForest(species ~ sepal_length + sepal_width + petal_length 
                                   + petal_width, data = train_data)

# Make predictions on the test data
prediction <- predict(RandomForest_model, newdata = test_data)
# Calculate the accuracy of the random forest model
accuracy <- mean(prediction == test_data$species)
print(paste("Random Forest Accuracy: ", accuracy))

#Model the data to train and test data
set.seed(123)
data_sample = sample.split(iris$species, SplitRatio=0.80)
train_data = subset(iris,data_sample==TRUE)
test_data = subset(iris,data_sample==FALSE)
dim(train_data)
dim(test_data)

# Train a logistic regression model
Logistic_Model<-glm(species~ sepal_width + sepal_length + petal_width + petal_length, train_data, family = binomial())
summary(Logistic_Model)






