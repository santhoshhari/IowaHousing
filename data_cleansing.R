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
housingDF$PoolQC <- factor(housingDF$PoolQC, levels=c('0','Fa','TA', 'Gd','Ex'), labels = c(0,1,2,3,4), ordered=T)
housingDF$MiscFeature[is.na(housingDF$MiscFeature)] <- '0'
housingDF$MiscFeature <- factor(housingDF$MiscFeature, levels=c('0','Elev','Gar2', 'Othr','Shed','TenC'), labels = c(0, 1,2,3,4,5))
housingDF$Alley[is.na(housingDF$Alley)] <- '0'
housingDF$Alley <- factor(housingDF$Alley, levels=c('0','Grvl','Pave'), labels = c(0,1,2), ordered=T)
housingDF$Fence[is.na(housingDF$Fence)] <- '0'
housingDF$Fence <- factor(housingDF$Fence, levels=c('0','MnWw','GdWo', 'MnPrv', 'GdPrv'), labels = c(0,1,2,3,4), ordered=T)
housingDF$FireplaceQu[is.na(housingDF$FireplaceQu)] <- '0'
housingDF$FireplaceQu <- factor(housingDF$FireplaceQu, levels=c('0','TA','Gd', 'Ex'), labels = c(0,1,2,3), ordered=T)
housingDF$LotFrontage[is.na(housingDF$LotFrontage)] <- mean(housingDF$LotFrontage, na.rm = T)
housingDF$GarageType[is.na(housingDF$GarageType)] <- '0'
housingDF$GarageType <- factor(housingDF$GarageType, levels=c('0','Detchd','CarPort', 'BuiltIn', 'Basment', 'Attchd', '2Types'), labels = seq(length(unique(housingDF$GarageType))), ordered=T)
housingDF$GarageYrBlt[is.na(housingDF$GarageYrBlt)] <- median(housingDF$GarageYrBlt, na.rm=T)
housingDF$GarageFinish[is.na(housingDF$GarageFinish)] <- '0'
housingDF$GarageFinish <- factor(housingDF$GarageFinish, levels=c('0','Unf','RFn', 'Fin'), labels = seq(4), ordered=T)
housingDF$GarageQual[is.na(housingDF$GarageQual)] <- '0'
housingDF$GarageQual <- factor(housingDF$GarageQual, levels=c('0','Po', 'Fa','TA', 'Gd','Ex'), labels = seq(6), ordered=T)
housingDF$GarageCond[is.na(housingDF$GarageCond)] <- '0'
housingDF$GarageCond <- factor(housingDF$GarageCond, levels=c('0','Po', 'Fa','TA', 'Gd','Ex'), labels = seq(6), ordered=T)
housingDF$BsmtExposure[is.na(housingDF$BsmtExposure)] <- '0'
housingDF$BsmtExposure <- factor(housingDF$BsmtExposure, levels=c('0','No','Mn', 'Av', 'Gd'), labels = seq(5), ordered=T)
housingDF$BsmtFinType2[is.na(housingDF$BsmtFinType2)] <- '0'
housingDF$BsmtFinType2 <- factor(housingDF$BsmtFinType2, levels=c('0','Unf','LwQ', 'Rec', 'BLQ', 'ALQ', 'GLQ'), labels = seq(7), ordered=T)
housingDF$BsmtQual[is.na(housingDF$BsmtQual)] <- '0'
housingDF$BsmtQual <- factor(housingDF$BsmtQual, levels=c('0','Po', 'Fa','TA', 'Gd','Ex'), labels = seq(6), ordered=T)
housingDF$BsmtCond[is.na(housingDF$BsmtCond)] <- '0'
housingDF$BsmtCond <- factor(housingDF$BsmtCond, levels=c('0','Po', 'Fa','TA', 'Gd','Ex'), labels = seq(6), ordered=T)
housingDF$BsmtFinType1[is.na(housingDF$BsmtFinType1)] <- '0'
housingDF$BsmtFinType1 <- factor(housingDF$BsmtFinType1, levels=c('0','Unf','LwQ', 'Rec', 'BLQ', 'ALQ', 'GLQ'), labels = seq(7), ordered=T)
housingDF$MasVnrType[is.na(housingDF$MasVnrType)] <- 'None'
housingDF$MasVnrType <- factor(housingDF$MasVnrType, levels = unique(housingDF$MasVnrType)[order(unique(housingDF$MasVnrType))] , labels = seq(length(unique(housingDF$MasVnrType))), ordered = T)
housingDF$MasVnrArea[is.na(housingDF$MasVnrArea)] <- 0
housingDF$Electrical <- getmode(housingDF$Electrical)
housingDF$Electrical <- factor(housingDF$Electrical, levels = unique(housingDF$Electrical)[order(unique(housingDF$Electrical))] , labels = seq(length(unique(housingDF$Electrical))), ordered = T)
housingDF$Id <- factor(housingDF$Id)
## Nicha and Keryu
housingDF$Exterior2nd <- factor(housingDF$Exterior2nd, levels = unique(housingDF$Exterior2nd)[order(unique(housingDF$Exterior2nd))] , labels = seq(length(unique(housingDF$Exterior2nd))), ordered = T)
housingDF$ExterQual <- factor(housingDF$ExterQual, levels = c("Fa", "TA", "Gd", "Ex"), labels = seq(4), ordered = T)
housingDF$ExterCond <- factor(housingDF$ExterCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"), labels = seq(5), ordered = T)
housingDF$Foundation <- factor(housingDF$Foundation, levels = unique(housingDF$Foundation)[order(unique(housingDF$Foundation))], labels = seq(length(unique(housingDF$Foundation))), ordered = T)
housingDF$Heating <- factor(housingDF$Heating, levels = unique(housingDF$Heating)[order(unique(housingDF$Heating))], labels = seq(length(unique(housingDF$Heating))), ordered = T)
housingDF$HeatingQC <- factor(housingDF$HeatingQC, levels = c("Po", "Fa", "TA", "Gd", "Ex"), labels = seq(5), ordered = T)
housingDF$CentralAir <- factor(housingDF$CentralAir, levels = c("Y", "N"), labels = seq(2), ordered = T)
housingDF$KitchenQual <- factor(housingDF$KitchenQual, levels = c("Fa", "TA", "Gd", "Ex"), labels = c(1,2,3,4), ordered = TRUE)
housingDF$Functional <- factor(housingDF$Functional,  levels = c("Sev", "Maj2","Maj1", "Mod",  "Min2", "Min1",  "Typ"), labels = c(1,2,3,4,5,6,7), ordered = TRUE)
housingDF$PavedDrive <- factor(housingDF$PavedDrive, levels = c("N", "P", "Y"), labels = c(1,2,3), ordered = TRUE)
housingDF$SaleType <- factor(housingDF$SaleType, levels = unique(housingDF$SaleType)[order(unique(housingDF$SaleType))] , labels = seq(length(unique(housingDF$SaleType))))
housingDF$SaleCondition <- factor(housingDF$SaleCondition, levels = unique(housingDF$SaleCondition)[order(unique(housingDF$SaleCondition))] , labels = seq(length(unique(housingDF$SaleCondition))))
## Neerja
housingDF$MSSubClass <- factor(housingDF$MSSubClass, levels=unique(housingDF$MSSubClass)[order(unique(housingDF$MSSubClass))], ordered=T)
housingDF$MSZoning <- factor(housingDF$MSZoning, levels = unique(housingDF$MSZoning)[order(unique(housingDF$MSZoning))] , labels = seq(length(unique(housingDF$MSZoning))), ordered = T)
housingDF$Street <- factor(housingDF$Street, levels = c("Pave" ,"Grvl"), labels = c(1,2))
housingDF$LotShape <- factor(housingDF$LotShape, levels = unique(housingDF$LotShape)[order(unique(housingDF$LotShape))] , labels = seq(length(unique(housingDF$LotShape))))
housingDF$LandContour <- factor(housingDF$LandContour, levels = unique(housingDF$LandContour)[order(unique(housingDF$LandContour))] , labels = seq(length(unique(housingDF$LandContour))), ordered = T)
housingDF$Utilities <- factor(housingDF$Utilities, levels = unique(housingDF$Utilities)[order(unique(housingDF$Utilities))] , labels = seq(length(unique(housingDF$Utilities))))
housingDF$LotConfig <- factor(housingDF$LotConfig, levels = unique(housingDF$LotConfig)[order(unique(housingDF$LotConfig))] , labels = seq(length(unique(housingDF$LotConfig))))
housingDF$LandSlope <- factor(housingDF$LandSlope, levels = unique(housingDF$LandSlope)[order(unique(housingDF$LandSlope))] , labels = seq(length(unique(housingDF$LandSlope))), ordered = T)
housingDF$Neighborhood <- factor(housingDF$Neighborhood, levels = unique(housingDF$Neighborhood)[order(unique(housingDF$Neighborhood))] , labels = seq(length(unique(housingDF$Neighborhood))))
housingDF$Condition1 <- factor(housingDF$Condition1, levels = unique(housingDF$Condition1)[order(unique(housingDF$Condition1))] , labels = seq(length(unique(housingDF$Condition1))))
housingDF$Condition2 <- factor(housingDF$Condition2, levels = unique(housingDF$Condition2)[order(unique(housingDF$Condition2))] , labels = seq(length(unique(housingDF$Condition2))))
housingDF$BldgType <- factor(housingDF$BldgType, levels = unique(housingDF$BldgType)[order(unique(housingDF$BldgType))] , labels = seq(length(unique(housingDF$BldgType))), ordered = T)
housingDF$HouseStyle <- factor(housingDF$HouseStyle, levels = unique(housingDF$HouseStyle)[order(unique(housingDF$HouseStyle))] , labels = seq(length(unique(housingDF$HouseStyle))), ordered = T)
housingDF$OverallQual <- factor(housingDF$OverallQual, ordered = T)
housingDF$OverallCond <- factor(housingDF$OverallCond, ordered = T)
housingDF$YearBuilt <- factor(housingDF$YearBuilt, ordered = T)
housingDF$YearRemodAdd <- factor(housingDF$YearRemodAdd, ordered = T)
housingDF$RoofStyle <- factor(housingDF$RoofStyle, levels = unique(housingDF$RoofStyle)[order(unique(housingDF$RoofStyle))], labels = seq(length(unique(housingDF$RoofStyle))))
housingDF$RoofMatl <- factor(housingDF$RoofMatl, levels = unique(housingDF$RoofMatl)[order(unique(housingDF$RoofMatl))], labels = seq(length(unique(housingDF$RoofMatl))))
housingDF$Exterior1st <-  factor(housingDF$Exterior1st, levels = unique(housingDF$Exterior1st)[order(unique(housingDF$Exterior1st))], labels = seq(length(unique(housingDF$Exterior1st))))


write.csv(housingDF, file = "clean_house.csv", row.names = F)


