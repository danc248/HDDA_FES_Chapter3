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


# ------------------------------------------------------------------------------

# https://bookdown.org/max/FES/model-optimization-and-tuning.html#fig:review-knn-plot

#train.recipe(x = keyword_norm, data = okc_train, method = "knn", 
#             metric = "ROC", trControl = okc_ctrl_rs, tuneGrid = data.frame(k = seq(1, 201, by = 4)))

knn_keyword[["call"]]  #view knn_keyword model

knn_keyword$results[,c(1,2)]
plot(knn_keyword)

fig_3_11 <- 
  ggplot(knn_keyword) +
  geom_line(data = knn_keyword$resample, aes(group = Resample, col = Resample), alpha = .4) + 
  theme(legend.position = "none")

fig_3_11



# ------------------------------------------------------------------------------

# train.recipe(x = keyword_norm, data = okc_train, method = "mlpKerasDropout", 
#              verbose = 0, epochs = 500, metric = "ROC", trControl = okc_ctrl_rand, 
#              tuneLength = 20)


#TABLE 3.3
mlp_keyword[["call"]]  #view mlp_keyword model

mlp_keyword$results %>% 
  arrange(desc(ROC)) %>% 
  dplyr::select(size, dropout, batch_size, lr, rho, decay, activation, ROC)


# ------------------------------------------------------------------------------

#train.recipe(x = keyword_rec, data = okc_train, method = "glm", 
#             metric = "ROC", trControl = okc_ctrl_keep)

glm_keyword[["call"]]  #view glm_keyword model


#SECTION 3.4.7 What Should Be Included Inside of Resampling?
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

# ------------------------------------------------------------------------------
