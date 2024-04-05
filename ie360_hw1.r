hw1data <- read.csv("/Users/serratopaloglu/Desktop/360hw1/DENEME2.csv", sep=";")
colnames(hw1data) <- c("Date","Fiyat","Watsons","Maskara","Dolar","İndirim","Sampuan","Parfüm","Sephora")
head(hw1data)


plot(hw1data$Fiyat,type="l")
plot(hw1data$Watsons,type="l")
plot(hw1data$Maskara, type="l")
plot(hw1data$Dolar,type="l")
plot(hw1data$İndirim, type="l")
plot(hw1data$Sampuan,type="l")
plot(hw1data$Parfüm, type="l")
plot(hw1data$Sephora, type="l")

hw1data$Fiyat <- 100*((hw1data$Fiyat -min(hw1data$Fiyat))/(max(hw1data$Fiyat)-min(hw1data$Fiyat)))
summary(hw1data)

num.cols2 <- sapply(hw1data, is.numeric)
cor.data2 <- cor(hw1data[,num.cols2])
print(cor.data2)

library(corrplot)
print(corrplot(cor.data2,method="color", tl.cex = 1.1, cl.cex=0.8))
print(corrplot(cor.data2,method="number", tl.cex = 1.1, cl.cex=0.8))

hwdata3 <- read.csv("/Users/serratopaloglu/Desktop/360hw1/deneme2.1.csv", sep=";")
colnames(hwdata3) <- c("Date","Fiyat","Watsons","Dolar","İndirim","Sampuan","Sephora")
hwdata3$Fiyat <- 100*((hwdata3$Fiyat -min(hwdata3$Fiyat))/(max(hwdata3$Fiyat)-min(hwdata3$Fiyat)))
head(hwdata3)
# Calculating Correlations on the new data set
num.cols2 <- sapply(hwdata3, is.numeric)
cor.data2 <- cor(hwdata3[,num.cols2])
print(cor.data2)
library(corrplot)
print(corrplot(cor.data2,method="color", tl.cex = 1.1, cl.cex=0.8))
print(corrplot(cor.data2,method="number", tl.cex = 1.1, cl.cex=0.8))

hw11data <- read.csv("/Users/serratopaloglu/Desktop/360hw1/deneme33.csv", sep=";")
colnames(hw11data) <- c("Date","Fiyat","Dolar","Sephora","Ramazan","İssizlik","CiltBakım")
head(hw11data)

plot(hw11data$Ramazan,type="l")
plot(hw11data$İssizlik,type="l")
plot(hw11data$CiltBakım, type="l")

hw11data$Fiyat <- 100*((hw11data$Fiyat -min(hw11data$Fiyat))/(max(hw11data$Fiyat)-min(hw11data$Fiyat)))
summary(hw11data)
head(hw11data)

pairs(hw11data[, 2:6], 
      main = "Scatterplot Matrix")


num.cols2 <- sapply(hw11data, is.numeric)
cor.data2 <- cor(hw11data[,num.cols2])
print(cor.data2)


library(corrplot)
print(corrplot(cor.data2,method="color", tl.cex = 1.1, cl.cex=0.8))
print(corrplot(cor.data2,method="number", tl.cex = 1.1, cl.cex=0.8))

model_2 <- lm(Fiyat ~ Dolar+CiltBakım+İssizlik+Sephora, data=hw11data)
summary(model_2)
residuals <- residuals(model_2)
plot(residuals)
acf(residuals)
hist(residuals, main = "Histogram of Residuals", xlab = "Residuals")

library(lattice)

xyplot(partial_resid ~ Dolar, data = hw11data, 
       xlab = "Dolar", ylab = "Partial Residuals",
       main = "Partial Residual Plot for Dolar")
xyplot(partial_resid ~ Sephora, data = hw11data, 
       xlab = "Sephora", ylab = "Partial Residuals",
       main = "Partial Residual Plot for Sephora")
xyplot(partial_resid ~ Ramazan, data = hw11data, 
       xlab = "Ramazan", ylab = "Partial Residuals",
       main = "Partial Residual Plot for Ramazan")
xyplot(partial_resid ~ CiltBakım, data = hw11data, 
       xlab = "CiltBakım", ylab = "Partial Residuals",
       main = "Partial Residual Plot for CiltBakım")
xyplot(partial_resid ~ İssizlik, data = hw11data, 
       xlab = "İssizlik", ylab = "Partial Residuals",
       main = "Partial Residual Plot for İssizlik")


acf(hw11data$Dolar)
acf(hw11data$Sephora)
acf(hw11data$İssizlik)


# Modifying "Sephora" to make it stationary, we will use log transformation
hw11data_copy1$sepho <- log(hw11data_copy1$Sephora)
model_2_1 <- lm(Fiyat ~ Dolar+İssizlik+Ramazan+CiltBakım+sepho, data=hw11data_copy1)
summary(model_2_1)

hw11data_copy1$sepho <- log(hw11data_copy1$Sephora)
mean <- mean(hw11data_copy1$Sephora)
dif <- c(mean, hw11data_copy1$Sephora[1:120])
model_2_1 <- lm(Fiyat ~ Dolar+İssizlik+Ramazan+CiltBakım+sepho+dif, data=hw11data_copy1)
summary(model_2_1)

hw11data_copy2 <- data.frame(hw11data)
hw11data_copy2$sepho <- log(hw11data_copy2$Sephora)
mean <- mean(hw11data_copy2$Sephora)
dif <- c(mean, hw11data_copy2$Sephora[1:120])

hw11data_copy2$trend <- 1:121
model_trend <- lm(Fiyat ~ sepho+dif+Dolar+Ramazan+İssizlik+CiltBakım+trend, data=hw11data_copy2)
summary(model_trend)

residuals <- residuals(model_trend)
plot(residuals)
acf(residuals)
hist(residuals, main = "Histogram of Residuals", xlab = "Residuals")

mean <- mean(hw11data_copy1$Sephora)
dif <- c(mean, hw11data_copy1$Sephora[1:120])
hw1_copy2$trend <- 1:121
model_new <- lm(log(Fiyat+1) ~ dif+log(Sephora)+Dolar+Ramazan+İssizlik+CiltBakım+trend, data=hw1_copy2)
summary(model_new)
residuals <- residuals(model_new)
plot(residuals)
acf(residuals)
hist(residuals, main = "Histogram of Residuals", xlab = "Residuals")

plot(model_new)

res <- resid(model_new)
plot(fitted(model_new), res)
abline(0,0)
plot(model_new)


ramazan_month <- as.integer(hw1_copy2$Ramazan)

# Creating dummy variables for monthly seasonality of Ramazan
ramazan_dummies <- model.matrix(~ factor(ramazan_month) - 1)
mean <- mean(hw11data_copy1$Sephora)
dif <- c(mean, hw11data_copy1$Sephora[1:120])
hw1_copy2$trend <- 1:121
model_new2 <- lm(log(Fiyat + 1) ~ dif + log(Sephora) + Dolar + İssizlik + CiltBakım + trend + ramazan_dummies, data = hw1_copy2)
summary(model_new2)
residuals <- residuals(model_new2)
plot(residuals)
acf(residuals)
hist(residuals, main = "Histogram of Residuals", xlab = "Residuals")

plot(model_new2)
