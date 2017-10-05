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

#find best alpha - 0.5
alphalist<-seq(0,1,by=0.1)

#grid.lambda <- 10^seq(10, -2, length = 100)
elasticnet<-lapply(alphalist, function(a){cv.glmnet(x[train, ], y.train, alpha=a)})
for (i in 1:11) {print(min(elasticnet[[i]]$cvm)) }


lambdas = NULL
for (i in 1:100)
{
cv.out <- cv.glmnet(x[train, ], y.train, alpha = 0.5)
errors = data.frame(cv.out$lambda,cv.out$cvm)
lambdas <- rbind(lambdas,errors)

}

plot(cv.out)
# take mean cvm for each lambda
lambdas <- aggregate(lambdas[, 2], list(lambdas$fit.lambda), mean)

# select the best one
bestindex = which(lambdas[2]==min(lambdas[2]))
bestlambda = lambdas[bestindex,1]


# Validation:
elastic.pred <- predict(cv.out, s = bestlambda, newx = x[test,])
mspe.elastic <- sqrt(mean((elastic.pred - y.test)^2))
mspe.elastic
length(coef(cv.out)[coef(cv.out)!= 0])

#code to fit final model
finalmodel <- glmnet(x,y, alpha = 0.5, lambda = bestlambda)


#### Out of sample predictions:
new_data <- read.csv('Morty.txt')
new_data <- new_data[,2:length(colnames(new_data))]

new_data <- rbind(housingDF, new_data)
x_morty <- model.matrix(SalePrice ~ ., data = new_data)[,-1]
x_morty <- as.data.frame(tail(x_morty,1))
y_morty <- tail(new_data$SalePrice,1)
x_morty <- x_morty[,colnames(x[train,])]


predicted_sale_price <- predict(finalmodel, as.matrix(x_morty))
predicted_sale_price


