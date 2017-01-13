set.seed(500)
library(MASS)
data <- read.csv("FINALDATA.csv")
data <- data[,2:7]
data <- subset(data, data$Postcode.Area < 7)

apply(data,2,function(x) sum(is.na(x)))

index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(Policy.Price~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$Policy.Price)^2)/nrow(test)

maxs <- apply(data, 2, max)
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]

library(neuralnet)
n <- names(train_)
f <- as.formula(paste("Policy.Price ~", paste(n[!n %in% "Policy.Price"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)

plot(nn)
