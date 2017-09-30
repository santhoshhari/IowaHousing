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



