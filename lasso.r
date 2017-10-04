setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(glmnet)
set.seed(10)
housingDF <- read.csv("clean_house.csv")
x <- model.matrix(SalePrice ~ ., data = housingDF)[,-1]
y <- housingDF$SalePrice

### For explanatory, we do not need to split train/test

lasso<-cv.glmnet(x, y, alpha=1)
plot(lasso)
best.lambda <- cv.out$lambda.min
best.lambda
coef(lasso)
length(coef(lasso)[coef(lasso)!= 0])


#### For prediction:
#train/test
train <- sample(1:nrow(x), 3 * nrow(x)/4)
test <- (-train)
y.train <- y[train]
y.test <- y[test]

lasso<-cv.glmnet(x[train, ], y.train, alpha=1)
plot(lasso)
best.lambda <- cv.out$lambda.min
best.lambda


# Validation results:

mspe_lasso = list()
for (i in 1:100){
lasso.pred <- predict(lasso, s = best.lambda, newx = x[test,])
mspe.lasso <- mean((lasso.pred - y.test)^2)
mspe_lasso <- c(mspe_lasso, sqrt(mspe.lasso))
}
mean(unlist(mspe_lasso))


length(coef(lasso)[coef(lasso)!= 0])


final_lasso <- glmnet(x,y, alpha = 1, lambda = best.lambda)
#get coefficients
coef_lasso <- coef(final_lasso)
length(coef_lasso[coef_lasso!= 0])

# Out of sample - Morty results:

# OOS Predictions - Morty
new_data <- read.csv('Morty.txt')
new_data <- new_data[,2:length(colnames(new_data))]

new_data <- rbind(housingDF, new_data)
x_morty <- model.matrix(SalePrice ~ ., data = new_data)[,-1]
x_morty <- as.data.frame(tail(x_morty,1))
y_morty <- tail(new_data$SalePrice,1)
x_morty <- x_morty[,colnames(x[train,])]
predicted_sale_price <- predict(final_lasso, as.matrix(x_morty))
predicted_sale_price

