setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(glmnet)
housingDF <- read.csv("clean_house.csv")
x <- model.matrix(SalePrice ~ ., data = housingDF)[,-1]
y <- housingDF$SalePrice


### For explanatory, we do not need to split train/test

# lasso<-cv.glmnet(x, y, alpha=1)
# plot(lasso)
# best.lambda <- cv.out$lambda.min
# best.lambda
# coef(lasso)
# length(coef(lasso)[coef(lasso)!= 0])
# 

#### For prediction:
#train/test
train <- sample(1:nrow(x), 3 * nrow(x)/4)
test <- (-train)
y.train <- y[train]
y.test <- y[test]

# Train

lambdas = NULL
for (i in 1:100)
{
  fit <- cv.glmnet(x[train, ], y.train, alpha=1)
  errors = data.frame(fit$lambda,fit$cvm)
  lambdas <- rbind(lambdas,errors)
}
# take mean cvm for each lambda
lambdas <- aggregate(lambdas[, 2], list(lambdas$fit.lambda), mean)

# select the best one
bestindex = which(lambdas[2]==min(lambdas[2]))
bestlambda = lambdas[bestindex,1]


# Validation results:

lasso.pred <- predict(fit, s = bestlambda, newx = x[test,])
mspe.lasso <- sqrt(mean((lasso.pred - y.test)^2))
mspe.lasso
length(coef(lasso)[coef(lasso)!= 0])


# OOS Predictions - Morty
new_data <- read.csv('Morty.txt')
new_data <- new_data[,2:length(colnames(new_data))]

new_data <- rbind(housingDF, new_data)
x_morty <- model.matrix(SalePrice ~ ., data = new_data)[,-1]
x_morty <- as.data.frame(tail(x_morty,1))
y_morty <- tail(new_data$SalePrice,1)
x_morty <- x_morty[,colnames(x[train,])]



# and now run glmnet once more with it
fit <- glmnet(x,y, alpha = 1,lambda=bestlambda)
predicted_sale_price <- predict(fit, as.matrix(x_morty))
predicted_sale_price

