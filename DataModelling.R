install.packages("stats")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggfortify")

library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(rstudioapi)

# Getting the path of your current open file
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
# Load CSV
data = read.csv("./data.csv")

# Linear regression

summary(data)

hist(data$givenScores)

hist(data$calculatedScores)

plot(calculatedScores ~ givenScores, data = data)

givenScores.calculatedScores.lm <- lm(calculatedScores ~ givenScores, data = data)

summary(givenScores.calculatedScores.lm)

par(mfrow=c(2,2))
plot(givenScores.calculatedScores.lm)
par(mfrow=c(1,1))


# K-Means Clustering

data = select(data,c(1,2))

wssplot <- function(data, nc = 15, seed = 1234) {
  wss <- numeric(nc)
  for (i in 1:nc) {
    set.seed(seed)
    kmeans_result <- kmeans(data, centers = i)
    wss[i] <- sum(kmeans_result$withinss)
  }
  plot(1:nc, wss, type = "b", xlab = "Number of Clusters", ylab = "Within Groups Sum of Squares")
}

data <- data[, 1:4]
wssplot(data, nc=15, seed=1234)

KM = kmeans(data,2)

autoplot(KM,data,frame=TRUE)

KM$centers
