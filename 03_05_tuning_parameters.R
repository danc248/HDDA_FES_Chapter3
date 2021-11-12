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

theme_set(theme_bw() + theme(legend.position = "top"))

# ------------------------------------------------------------------------------

data("ames")
# Load ames_knn.RData (chater 3 - github)
path0 = "/Users/danielececcarelli/Documents/2 - High Dimensional Data Analysis/final_project_FES/"
file0 = "ames_knn.RData"
file1 = paste0(path0, file0)
load(file1)
ames_knn_neigh


houses <- ames[which(ames$Latitude > 42.05 & 
                       ames$Latitude < 42.07 & 
                       ames$Longitude < -93.5 & 
                       ames$Longitude > -93.7) , c("Latitude", "Longitude", "Sale_Price")]
str(houses)

blue_point_df <- houses[which(houses$Sale_Price == 175900),] #& houses$Latitude == 42.05848),]
dim(blue_point_df)[1] #number of rows
train0 <- houses[-which(houses$Sale_Price == 175900),] #& houses$Latitude == 42.05848),] 
#m1 <- kknn(Sale_Price~., train0, blue_point_df, k=3)
#RMSE(blue_point_df$Sale_Price, m1[["fitted.values"]])
cv <- trainControl(method = "cv", number = 10, savePredictions = TRUE)
fit.knn <- train(Sale_Price ~ ., train0, method = "knn", trControl = cv, tuneGrid = data.frame(k = 1:10))
plot(fit.knn, main="cross-validation knn")
fit.knn$bestTune
RMSE(blue_point_df$Sale_Price, predict(fit.knn, blue_point_df)) #testing using data points with a Sale_Price of 175900, just like in Figure 3.10
rcv <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
fit.knn_rcv <- train(Sale_Price ~ ., train0, method = "knn", trControl = rcv, tuneLength = 10, tuneGrid = data.frame(k = 1:10))
plot(fit.knn_rcv, main="repeated cross-validation knn")
fit.knn_rcv$bestTune

rmse_list = list()
for (i in 1:10) {
  updated_model <- fit.knn
  updated_model[["finalModel"]][["k"]] <- i
  rmse_list <- append(rmse_list, RMSE(blue_point_df$Sale_Price, predict(updated_model, blue_point_df)))
  }

plot(matrix(rmse_list))

