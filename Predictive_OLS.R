setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

housingDF <- read.csv("clean_house.csv")
x <- model.matrix(SalePrice ~ ., data = housingDF)[,-1]
y <- housingDF$SalePrice

housingDF_new <- data.frame(x,y)

ols <- lm(y~., data = housingDF_new)

l <- step(ols, k = log(nrow(housingDF_new)), direction = 'backward') #bic

aic_step <- step(ols, direction = "both")
bic_step <- step(ols, direction = "both", k = log(length(x)))


