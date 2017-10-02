setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(glmnet)

housingDF <- read.csv("clean_house.csv")
x <- model.matrix(SalePrice ~ ., data = housingDF)[,-1]
y <- housingDF$SalePrice

grid.lambda <- 10^seq(10, -2, length = 100)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.train <- y[train]
y.test <- y[test]

cv.out <- cv.glmnet(x[train, ], y.train, alpha = 0.5)
plot(cv.out)
best.lambda <- cv.out$lambda.min
best.lambda

alphalist<-seq(0,1,by=0.1)
elasticnet <- lapply(alphalist, function(x){cv.glmnet(x,y,alpha=x, )})