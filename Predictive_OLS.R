setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

housingDF <- read.csv("clean_house.csv")
x <- model.matrix(SalePrice ~ ., data = housingDF)[,-1]
y <- housingDF$SalePrice

housingDF_new <- data.frame(x,y)

ols <- lm(y~., data = housingDF_new)

l0 <- step(ols, direction <- 'backward', trace <- F) #aic
l <- step(ols, k <- log(nrow(housingDF_new)), direction = 'backward', trace <- F) #bic

## save this model
save(l0, file = "AIC_model.rda")
save(l, file = "BIC_model.rda")

#load("my_model1.rda") to load
#update with new observations https://stackoverflow.com/questions/5118074/reusing-a-model-built-in-r

####Prediction:

load("AIC_model.rda")
new_data <- read.csv('Morty.txt')

#Remove the first (redundant) column
new_data <- new_data[,2:length(colnames(new_data))]

#Append new data point to housingDF and build matrix containing dummy variables; then extract only the last row
new_data <- rbind(housingDF, new_data)
x_test <- model.matrix(SalePrice ~ ., data = new_data)[,-1]
x_test <- tail(x_test,1)
y_test <- tail(new_data$SalePrice,1)

predicted_sale_price <- predict(l0, data.frame(x_test))

