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



#train.recipe(x = keyword_rec, data = okc_train, method = "glm", 
#             metric = "ROC", trControl = okc_ctrl_keep)

glm_keyword[["call"]]  #view glm_keyword model
mlp_keyword[["call"]]  #view mlp_keyword model
knn_keyword[["call"]]  #view knn_keyword model

#TABLE 3.4
glm_roc <- glm_keyword$resample[, c("ROC", "Resample")]
names(glm_roc)[1] <- "Logistic Regression"
net_roc <- merge(mlp_keyword$resample, mlp_keyword$bestTune)
net_roc <- net_roc[, c("ROC", "Resample")]
names(net_roc)[1] <- "Neural Network"
roc_diffs <- merge(glm_roc, net_roc)
roc_diffs$Difference <- roc_diffs[, "Neural Network"] - roc_diffs[, "Logistic Regression"]
rownames(roc_diffs) <- gsub("Fold0", "Fold  ", roc_diffs$Resample)
rownames(roc_diffs) <- gsub("Fold10", "Fold 10", rownames(roc_diffs))

roc_diffs$Resample <- NULL
roc_diffs$"Neural Network" <- round(roc_diffs$"Neural Network", 3)
roc_diffs$"Logistic Regression" <- round(roc_diffs$"Logistic Regression", 3)
roc_diffs$"Difference" <- round(roc_diffs$"Difference", 3)
roc_diffs


cor(roc_diffs[, "Neural Network"], roc_diffs[, "Logistic Regression"])

okc_dff

t.test(roc_diffs$"Neural Network", roc_diffs$"Logistic Regression", paired = TRUE, alternative = "two.sided")


# ------------------------------------------------------------------------------
# Load logistic model results for OkC data


set.seed(123) 
ctrl_cv10_glm <- trainControl(method = "cv", 
                              number = 10, 
                              savePredictions = TRUE,
                              classProbs = TRUE
                              ,summaryFunction = twoClassSummary
                              ,sampling = "down"
                              )

set.seed(123) 
model_cv10_glm <- train(form = Class ~ ., 
                        data = okc_train[1:2000,], 
                        method = "glm", 
                        metric = "ROC", 
                        maximize = TRUE, 
                        #model = FALSE, 
                        trControl = ctrl_cv10_glm)

set.seed(123) 
ctrl_cv10_knn <- trainControl(method = "cv", 
                              number = 10, 
                              savePredictions = TRUE,
                              classProbs = TRUE
                              ,summaryFunction = twoClassSummary
                              ,sampling = "down"
                              )


set.seed(123) 
model_cv10_knn <- train(form = Class ~ ., 
                        data = okc_train[1:2000,], 
                        method = "knn", 
                        metric = "ROC", 
                        maximize = TRUE, 
                        #model = FALSE, 
                        trControl = ctrl_cv10_knn,
                        tuneLength = 4
                        )

glm_roc <- model_cv10_glm$resample[, c("ROC", "Resample")]
names(glm_roc)[1] <- "Logistic Regression"
#net_roc <- merge(model_cv10_knn$resample, mlp_keyword$bestTune)
net_roc <- model_cv10_knn$resample[, c("ROC", "Resample")]
names(net_roc)[1] <- "KNN"
roc_diffs <- merge(glm_roc, net_roc)
roc_diffs$Difference <- roc_diffs[, "KNN"] - roc_diffs[, "Logistic Regression"]
rownames(roc_diffs) <- gsub("Fold0", "Fold  ", roc_diffs$Resample)
rownames(roc_diffs) <- gsub("Fold10", "Fold 10", rownames(roc_diffs))

roc_diffs$Resample <- NULL
roc_diffs$"KNN" <- round(roc_diffs$"KNN", 3)
roc_diffs$"Logistic Regression" <- round(roc_diffs$"Logistic Regression", 3)
roc_diffs$"Difference" <- round(roc_diffs$"Difference", 3)
roc_diffs

cor(roc_diffs[, "KNN"], roc_diffs[, "Logistic Regression"])

t.test(roc_diffs$"KNN", roc_diffs$"Logistic Regression", paired = TRUE, alternative = "two.sided")

#let's check the analysis and assessment were used to train the two models
model_cv10_glm$pred %>%
  left_join(model_cv10_knn$pred, by = "rowIndex") %>%
  mutate(same = Resample.x == Resample.y) %>%
  {all(.$same)}


#let's check the differences between two different pre-processing techniques applied to the same model
set.seed(123) 
ctrl_cv10_glm <- trainControl(method = "cv", 
                              number = 10, 
                              savePredictions = TRUE,
                              classProbs = TRUE
                              ,summaryFunction = twoClassSummary
                              ,sampling = "down"
                              )

set.seed(123) 
model_cv10_glm1 <- train(form = Class ~ ., 
                        data = okc_train[1:2000,], 
                        method = "glm", 
                        metric = "ROC", 
                        maximize = TRUE, 
                        #model = FALSE, 
                        trControl = ctrl_cv10_glm
                        , preProcess = "BoxCox"
                        
                        )

set.seed(123) 
model_cv10_glm2 <- train(form = Class ~ ., 
                        data = okc_train[1:2000,], 
                        method = "glm", 
                        metric = "ROC", 
                        maximize = TRUE, 
                        #model = FALSE, 
                        trControl = ctrl_cv10_glm
                        , preProcess = c("center", "scale")
                        )

glm_roc <- model_cv10_glm1$resample[, c("ROC", "Resample")]
names(glm_roc)[1] <- "Logistic Regression"
#net_roc <- merge(model_cv10_knn$resample, mlp_keyword$bestTune)
net_roc <- model_cv10_glm2$resample[, c("ROC", "Resample")]
names(net_roc)[1] <- "KNN"
roc_diffs <- merge(glm_roc, net_roc)
roc_diffs$Difference <- roc_diffs[, "KNN"] - roc_diffs[, "Logistic Regression"]
rownames(roc_diffs) <- gsub("Fold0", "Fold  ", roc_diffs$Resample)
rownames(roc_diffs) <- gsub("Fold10", "Fold 10", rownames(roc_diffs))

roc_diffs$Resample <- NULL
roc_diffs$"KNN" <- round(roc_diffs$"KNN", 3)
roc_diffs$"Logistic Regression" <- round(roc_diffs$"Logistic Regression", 3)
roc_diffs$"Difference" <- round(roc_diffs$"Difference", 3)
roc_diffs

cor(roc_diffs[, "KNN"], roc_diffs[, "Logistic Regression"])

t.test(roc_diffs$"KNN", roc_diffs$"Logistic Regression", paired = TRUE, alternative = "two.sided")

