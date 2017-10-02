setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(glmnet)

housingDF <- read.csv("clean_house.csv")
x <- model.matrix(SalePrice ~ ., data = housingDF)[,-1]
y <- housingDF$SalePrice

#train/test
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.train <- y[train]
y.test <- y[test]

#find best alpha - 0.5
alphalist<-seq(0,1,by=0.1)
elasticnet<-lapply(alphalist, function(a){cv.glmnet(x[train, ], y.train, alpha=a)})
for (i in 1:11) {print(min(elasticnet[[i]]$cvm))}


#grid.lambda <- 10^seq(10, -2, length = 100)

cv.out <- cv.glmnet(x[train, ], y.train, alpha = 0.5)

plot(cv.out)
best.lambda <- cv.out$lambda.min
best.lambda

#best lambda = 5838.57

#calculate MSPE
EN.pred <- predict(EN.model.train, s = best.lambda, newx = x[test,])
mspe.EN <- mean((EN.pred - y.test)^2)
mspe.EN

#need to CV loop across diff test/train k-fold to get average MSPE?

#code to fit final model
finalmodel <- glmnet(x,y, alpha = 0.5, lambda = best.lambda)
#get coefficients
coef_elastic <- coef(finalmodel)


