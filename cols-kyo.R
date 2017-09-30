rm(list=ls())

data = read.table('/Users/booranium/usf/601_regression/project/housing.txt', sep = ',', header = T, stringsAsFactors = F)
head(data)
#attach(data)

unique(as.numeric(SaleType))
unique(SaleType)

n = ncol(data)
start = ncol(data)-19
start
head(data[,(ncol(data)-19):ncol(data)])

# last 19 cols 

KitchenQual = factor(KitchenQual, levels = c("Fa", "TA", "Gd", "Ex"), labels = c(1,2,3,4), ordered = TRUE)
TotRmsAbvGrd
unique(Functional)
Functional = factor(Functional,  levels = c("Sev", "Maj2","Maj1", "Mod",  "Min2", "Min1",  "Typ"), labels = c(1,2,3,4,5,6,7), ordered = TRUE)
Fireplaces
GarageCars
GarageArea
PavedDrive = factor(PavedDrive, levels = c("N", "P", "Y"), labels = (1,2,3), ordered = TRUE)
WoodDeckSF 
OpenPorchSF
EnclosedPorch
X3SsnPorch
ScreenPorch
PoolArea
MiscVal
MoSold
YrSold
SaleType = factor(SaleType)
SaleCondition = factor(SaleCondition)
SalePrice
       