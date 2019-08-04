################
# File for preprocessing raw data
# Authors: Kevin Thompson, Brandom Croom, Sterling Beason
# Last Updated: August 4, 2019
################
library(readr)
library(forcats)

#TODO: ENSURE ORDERING ON ORDERED FACTORS MAKES SENSE
#TODO: MAP DEFINITIONAL DEPENDENCIES BETWEEN VARIABLES
#TODO: ENGINEER NEW FEATURES
#TODO: CHECK CONSISTENCY AMONG QUALITY INDICATORS
#TODO: CHECK IF CONDITION AND QUALITY GO HAND IN HAND
#TODO: MAKE COLUMN NAMES SYNTACTICALLY CORRECT

housePrices <- read_csv("train.csv",
                        col_types = cols(
                          MSSubClass = col_factor(),
                          MSZoning = col_factor(),
                          LotFrontage = col_double(),
                          LotArea = col_double(),
                          Street = col_factor(),
                          Alley = col_factor(),
                          LotShape = col_factor(),
                          LandContour = col_factor(),
                          Utilities = col_factor(),
                          LotConfig = col_factor(),
                          LandSlope = col_factor(),
                          Neighborhood = col_factor(),
                          Condition1 = col_factor(),
                          Condition2 = col_factor(),
                          BldgType = col_factor(),
                          HouseStyle = col_factor(),
                          OverallQual = col_factor(ordered=TRUE),
                          OverallCond = col_factor(ordered=TRUE),
                          YearBuilt = col_integer(),
                          YearRemodAdd = col_integer(),
                          RoofStyle = col_factor(),
                          RoofMatl = col_factor(),
                          Exterior1st = col_factor(),
                          Exterior2nd = col_factor(),
                          MasVnrType = col_factor(),
                          MasVnrArea = col_double(),
                          ExterQual = col_factor(ordered=TRUE),
                          ExterCond = col_factor(ordered=TRUE),
                          Foundation = col_factor(),
                          BsmtQual = col_factor(ordered=TRUE),
                          BsmtCond = col_factor(ordered=TRUE),
                          BsmtExposure = col_factor(ordered=TRUE),
                          BsmtFinType1 = col_factor(ordered=TRUE),
                          BsmtFinSF1 = col_double(),
                          BsmtFinType2 = col_factor(ordered=TRUE),
                          BsmtFinSF2 = col_double(),
                          BsmtUnfSF = col_double(),
                          TotalBsmtSF = col_double(),
                          Heating = col_factor(),
                          HeatingQC = col_factor(ordered=TRUE),
                          CentralAir = col_factor(),
                          Electrical = col_factor(),
                          `1stFlrSF` = col_double(),
                          `2ndFlrSF` = col_double(),
                          LowQualFinSF = col_double(),
                          GrLivArea = col_double(),
                          BsmtFullBath = col_integer(),
                          BsmtHalfBath = col_integer(),
                          FullBath = col_integer(),
                          HalfBath = col_integer(),
                          BedroomAbvGr = col_integer(),
                          KitchenAbvGr = col_integer(),
                          KitchenQual = col_factor(ordered=TRUE),
                          TotRmsAbvGrd = col_integer(),
                          Functional = col_factor(ordered=TRUE),
                          Fireplaces = col_integer(),
                          FireplaceQu = col_factor(ordered=TRUE),
                          GarageType = col_factor(),
                          GarageYrBlt = col_integer(),
                          GarageFinish = col_factor(ordered=TRUE),
                          GarageCars = col_double(),
                          GarageArea = col_double(),
                          GarageQual = col_factor(ordered=TRUE),
                          GarageCond = col_factor(ordered=TRUE),
                          PavedDrive = col_factor(),
                          WoodDeckSF = col_double(),
                          OpenPorchSF = col_double(),
                          EnclosedPorch = col_double(),
                          `3SsnPorch` = col_double(),
                          ScreenPorch = col_double(),
                          PoolArea = col_double(),
                          PoolQC = col_factor(ordered=TRUE),
                          Fence = col_factor(ordered=TRUE),
                          MiscFeature = col_factor(),
                          MiscVal = col_double(),
                          MoSold = col_factor(ordered=TRUE),
                          YrSold = col_integer(),
                          SaleType = col_factor(),
                          SaleCondition = col_factor(),
                          SalePrice = col_double()
                          ))