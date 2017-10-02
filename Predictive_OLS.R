setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

housingDF <- read.csv("clean_house.csv")
x <- model.matrix(SalePrice ~ ., data = housingDF)[,-1]
y <- housingDF$SalePrice

housingDF_new <- data.frame(x,y)

ols <- lm(y~., data = housingDF_new)

l0 <- step(ols, direction = 'backward', trace = F) #aic
l <- step(ols, k = log(nrow(housingDF_new)), direction = 'backward', trace = F) #bic

## save this model
save(l0, file = "AIC_model.rda")
save(l, file = "BIC_model.rda")

#load("my_model1.rda") to load
#update with new observations https://stackoverflow.com/questions/5118074/reusing-a-model-built-in-r


