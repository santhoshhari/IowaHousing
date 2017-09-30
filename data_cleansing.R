library(dplyr)


#HousingDF <- read.csv("~/Documents/Coursework/LinearRegression/CaseStudy/housing.txt", stringsAsFactors = T)

HousingDF <- read.csv('~/Downloads/housing.txt', stringsAsFactors = F)



HousingDF$Exterior2nd <- factor(HousingDF$Exterior2nd, levels = unique(HousingDF$Exterior2nd)[order(unique(HousingDF$Exterior2nd))] , labels = seq(length(unique(HousingDF$Exterior2nd))), ordered = T)
HousingDF$ExterQual <- factor(HousingDF$ExterQual, levels = c("Fa", "TA", "Gd", "Ex"), labels = seq(4), ordered = T)
HousingDF$ExterCond <- factor(HousingDF$ExterCond, levels = c("Po", "Fa", "TA", "Gd", "Ex"), labels = seq(5), ordered = T)
HousingDF$Foundation <- factor(HousingDF$Foundation, levels = unique(HousingDF$Foundation)[order(unique(HousingDF$Foundation))], labels = seq(length(unique(HousingDF$Foundation))), ordered = T)
HousingDF$Heating <- factor(HousingDF$Heating, levels = unique(HousingDF$Heating)[order(unique(HousingDF$Heating))], labels = seq(length(unique(HousingDF$Heating))), ordered = T)
HousingDF$HeatingQC <- factor(HousingDF$HeatingQC, levels = c("Po", "Fa", "TA", "Gd", "Ex"), labels = seq(5), ordered = T)
HousingDF$CentralAir <- factor(HousingDF$CentralAir, levels = c("Y", "N"), labels = seq(2), ordered = T)
KitchenQual = factor(KitchenQual, levels = c("Fa", "TA", "Gd", "Ex"), labels = c(1,2,3,4), ordered = TRUE)
Functional = factor(Functional,  levels = c("Sev", "Maj2","Maj1", "Mod",  "Min2", "Min1",  "Typ"), labels = c(1,2,3,4,5,6,7), ordered = TRUE)
PavedDrive = factor(PavedDrive, levels = c("N", "P", "Y"), labels = (1,2,3), ordered = TRUE)
SaleType = factor(SaleType)
SaleCondition = factor(SaleCondition)


