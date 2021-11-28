# Code requires these packages: 

library(tidymodels)
library(grid)
library(gridExtra)
library(caret)
library(stringr)
library(ggplot2)
library(recipes)
library(rsample)
library(timetk)
library(kknn)
library(dplyr)
library(pROC)

theme_set(theme_bw() + theme(legend.position = "top"))

# ------------------------------------------------------------------------------

# OKCupid Dataset - stratified sampling - 75% train, 25% test
path0 = "/Users/danielececcarelli/Documents/2 - High Dimensional Data Analysis/final_project_FES/"
file04 = "okc.RData"
file14 = paste0(path0, file04)
load(file14)
okc_df <- rbind(okc_train,okc_test)
set.seed(123)
train.index00 <- createDataPartition(okc_df$Class, p = .75, list = FALSE)
train00 <- okc_df[ train.index00,]
test00  <- okc_df[-train.index00,]
round(table(okc_df$Class)/nrow(okc_data),3)
round(table(train00$Class)/nrow(train00),3)
round(table(test00$Class)/nrow(test00),3)


# AMES real estate market data - quartiles
data("ames")
set.seed(123)
ames_split <- initial_split(ames, prop = 0.75, strata = Sale_Price) #Numeric strata are binned into quartiles 
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

dim(ames_train)
quantile(ames$Sale_Price)
quantile(ames_train$Sale_Price)
quantile(ames_test$Sale_Price)

