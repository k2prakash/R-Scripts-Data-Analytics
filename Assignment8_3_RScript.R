#Load the data from the file
library(ggplot2)
library(dplyr)
library(tidyverse)

iris_ds <- read.csv(
  "c:/Users/k2pra/Documents/CIS-541/Week4/Iris.csv",
  sep=",", header=TRUE)


# Plot all the relationships 
pairs(iris_ds[,2:5], panel = panel.smooth, upper.panel=NULL)

iris_df <- select(iris_ds, c('SepalLengthCm', 'SepalWidthCm', 'PetalLengthCm', 'PetalWidthCm'))

# Scatter plot of Petal Length vs Sepal Length
g <- ggplot(iris_df, aes(x=PetalLengthCm, y=SepalLengthCm)) + 
  geom_point() + 
  geom_smooth(method="lm") +
  labs(title = "Petal Length vs Sepal Length", 
       y = "Sepal Length in cm",
       x = "Petal Length in cm")
plot(g)


# Scatter plot of Sepal Length vs Sepal Width
g <- ggplot(iris_df, aes(x=SepalWidthCm, y=SepalLengthCm)) + 
  geom_point() + 
  geom_smooth(method="lm") +
  labs(title = "Sepal Width vs Sepal Length", 
       y = "Sepal Length in cm",
       x = "Sepal Width in cm")
plot(g)


# Scatter plot of Sepal Length vs Sepal Width
g <- ggplot(iris_df, aes(x=PetalWidthCm, y=SepalLengthCm)) + 
  geom_point() + 
  geom_smooth(method="lm") +
  labs(title = "Petal Width vs Sepal Length", 
       y = "Sepal Length in cm",
       x = "Petal Width in cm")
plot(g)


# Box Plot of Iris characteristics
par(mfrow=c(1, 4))  # divide graph area in 4 columns
boxplot(iris_df$SepalLengthCm, data=iris_df, main="Sepal Length",
        ylab= "Sepal Length in Cm", col = "light blue")

boxplot(iris_df$SepalWidthCm, data=iris_df, main="Sepal Width",
        ylab= "Sepal Width in Cm", col = "light blue")

boxplot(iris_df$PetalLengthCm, data=iris_df, main="Petal Length",
        ylab= "Petal Length in Cm", col = "orange")

boxplot(iris_df$PetalWidthCm, data=iris_df, main="Petal Width",
        ylab= "Petal Width in Cm", col = "orange")

RegressionResults = lm(scale(SepalLengthCm) ~ scale(SepalWidthCm) + 
                         scale(PetalLengthCm) + 
                         scale(PetalWidthCm), 
                         data = iris_df)

summary(RegressionResults)
RegressionSummary=summary(RegressionResults)
RegressionSummary$coefficients
write.csv(RegressionSummary$coefficients, 
          'C:/Users/k2pra/Documents/CIS-541/Week4/regression_results.csv')

par(mfrow=c(1, 1))  # divide graph area back to 1 column
plot(RegressionResults)