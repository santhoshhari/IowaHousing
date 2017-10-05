if(!require("dplyr")){install.packages("dplyr", repos = "http://cran.us.r-project.org")}
if(!require("rstudioapi")){install.packages("rstudioapi", repos = "http://cran.us.r-project.org")}


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
housingDF <- read.csv("./Data/housing.txt", stringsAsFactors = F)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

## Santhosh
housingDF$PoolQC[is.na(housingDF$PoolQC)] <- '0'
housingDF$MiscFeature[is.na(housingDF$MiscFeature)] <- '0'
housingDF$Alley[is.na(housingDF$Alley)] <- '0'
housingDF$Fence[is.na(housingDF$Fence)] <- '0'
housingDF$FireplaceQu[is.na(housingDF$FireplaceQu)] <- '0'
housingDF$LotFrontage[is.na(housingDF$LotFrontage)] <- mean(housingDF$LotFrontage, na.rm = T)
housingDF$GarageType[is.na(housingDF$GarageType)] <- '0'
housingDF$GarageYrBlt[is.na(housingDF$GarageYrBlt)] <- median(housingDF$GarageYrBlt, na.rm=T)
housingDF$GarageFinish[is.na(housingDF$GarageFinish)] <- '0'
housingDF$GarageQual[is.na(housingDF$GarageQual)] <- '0'
housingDF$GarageCond[is.na(housingDF$GarageCond)] <- '0'
housingDF$BsmtExposure[is.na(housingDF$BsmtExposure)] <- '0'
housingDF$BsmtFinType2[is.na(housingDF$BsmtFinType2)] <- '0'
housingDF$BsmtQual[is.na(housingDF$BsmtQual)] <- '0'
housingDF$BsmtCond[is.na(housingDF$BsmtCond)] <- '0'
housingDF$BsmtFinType1[is.na(housingDF$BsmtFinType1)] <- '0'
housingDF$MasVnrType[is.na(housingDF$MasVnrType)] <- 'None'
housingDF$MasVnrArea[is.na(housingDF$MasVnrArea)] <- 0
housingDF$Electrical <- getmode(housingDF$Electrical)


