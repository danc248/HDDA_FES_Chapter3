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
library(pROC)
library(kernlab)
library(lubridate)
library(reshape2)
library(pls)
library(glmnet)

theme_set(theme_bw() + theme(legend.position = "top"))

# ------------------------------------------------------------------------------

path0 = "/Users/danielececcarelli/Documents/2 - High Dimensional Data Analysis/final_project_FES/"

# Load pre-computed model results for OkC data
file00 = "okc_knn_keyword.RData"
file01 = "okc_mlp_keyword.RData"
file02 = "okc_dff.RData"
file03 = "okc_glm_keyword.RData"
file04 = "okc.RData"
file10 = paste0(path0, file00)
file11 = paste0(path0, file01)
file12 = paste0(path0, file02)
file13 = paste0(path0, file03)
file14 = paste0(path0, file04)
load(file10)
load(file11)
load(file12)
load(file13)
load(file14)

#down sampling applied to "okc_train" dataset to balance the frequency of "stem" and "other" in "Class" variable
levels(okc_train$Class)
str(okc_train)
knn_keyword[["control"]][["sampling"]][["name"]] 
table(okc_train$Class)  #before down-sampling
table(okc_test$Class)
table(okc_down$Class)
table(okc_sampled$Class)
okc_data <- rbind(okc_train, okc_test)
dim(okc_data)
table(okc_data$Class)/nrow(okc_data)
table(okc_down$Class)/nrow(okc_down)
table(okc_sampled$Class)/nrow(okc_sampled)

par(mfrow=c(1, 2))
barplot(table(okc_data$Class)/nrow(okc_data), xlab="Class_Original_Dataset", ylab="proportion")
barplot(table(okc_down$Class)/nrow(okc_down), xlab="Class_Down_Sampled", ylab="proportion")
