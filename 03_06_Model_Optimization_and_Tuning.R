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
file10 = paste0(path0, file00)
file11 = paste0(path0, file01)
file12 = paste0(path0, file02)
file13 = paste0(path0, file03)
load(file10)
load(file11)
load(file12)
load(file13)


# ------------------------------------------------------------------------------

# https://bookdown.org/max/FES/model-optimization-and-tuning.html#fig:review-knn-plot

knn_keyword$results[,c(1,2)]
plot(knn_keyword)

fig_3_11 <- 
  ggplot(knn_keyword) +
  geom_line(data = knn_keyword$resample, aes(group = Resample, col = Resample), alpha = .4) + 
  theme(legend.position = "none")

fig_3_11



# ------------------------------------------------------------------------------

mlp_keyword$results %>% 
  arrange(desc(ROC)) %>% 
  dplyr::select(size, dropout, batch_size, lr, rho, decay, activation, ROC)


# ------------------------------------------------------------------------------

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

cor(roc_diffs[, "Neural Network"], roc_diffs[, "Logistic Regression"])

okc_dff

# ------------------------------------------------------------------------------
