setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(glmnet)
housingDF <- read.csv("clean_house.csv")
x <- model.matrix(SalePrice ~ ., data = housingDF)[,-1]
y <- housingDF$SalePrice

#train/test
train <- sample(1:nrow(x), 3 * nrow(x)/4)
test <- (-train)
y.train <- y[train]
y.test <- y[test]


ridge<-cv.glmnet(x[train, ], y.train, alpha=0)
plot(ridge)
best.lambda <- cv.out$lambda.min
best.lambda


mspe_ridge = list()
for (i in 1:100){
ridge.pred <- predict(ridge, s = best.lambda, newx = x[test,])
mspe.ridge <- mean((ridge.pred - y.test)^2)
mspe_ridge <- c(mspe_ridge, sqrt(mspe.ridge))
}

mean(unlist(mspe_ridge))



final_ridge <- glmnet(x,y, alpha = 0, lambda = best.lambda)
#get coefficients
coef_ridge <- coef(final_ridge)



# OOS Predictions - Morty
new_data <- read.csv('Morty.txt')
new_data <- new_data[,2:length(colnames(new_data))]

new_data <- rbind(housingDF, new_data)
x_morty <- model.matrix(SalePrice ~ ., data = new_data)[,-1]
x_morty <- as.data.frame(tail(x_morty,1))
y_morty <- tail(new_data$SalePrice,1)
x_morty <- x_morty[,colnames(x[train,])]

predicted_sale_price <- predict(final_ridge, as.matrix(x_morty))
predicted_sale_price