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

theme_set(theme_bw() + theme(legend.position = "top"))

# ------------------------------------------------------------------------------

#range20 <- 1:20
#range1 <- data.frame(range0)

data("swiss")
swiss0 <- swiss[c(1:40),]
range40 <- 1:dim(swiss0)[1]
# Define training control
set.seed(123) 

#cross-validation
train.control <- trainControl(method = "cv", number = 10, savePredictions = TRUE)

model_cv <- train(Fertility ~., 
                  data = swiss0, 
                  method = "glm",
                  trControl = train.control)

assessment_cv <- data.frame()
for (i in 1:length(model_cv[["resample"]][["Resample"]])) {
  names(model_cv[["control"]][["indexOut"]][i])
  trial1 <- data.frame(model_cv[["control"]][["indexOut"]][i])
  trial1$Resample <- names(model_cv[["control"]][["indexOut"]][i])
  trial1$set <- "assessment"
  names(trial1) <- c("rowIndex", "Resample", "set")
  assessment_cv <- rbind(assessment_cv, trial1)
}

assessment_cv

#range40 <- as.character(1:40)
assessment_cv$Resample <- as.factor(assessment_cv$Resample)
unique_resample <- unique(assessment_cv$Resample)
assessment_cv_plot <- ggplot(assessment_cv, aes(x=Resample, y=rowIndex)) + geom_point() +  
  theme(axis.text.x = element_text(face="bold",  size=8),
        axis.text.y = element_text(face="bold", size = 8)) + 
  ggtitle("V-FOLD CV") + 
  labs(subtitle="black dots represent the assessment set") + 
  scale_x_discrete(name = "Resamples", breaks=unique_resample,
                   labels=1:length(unique_resample)) + 
  scale_y_discrete(name ="Training Set Samples", 
                   limits=as.character(range40))

assessment_cv_plot


analysis_cv <- data.frame()
for (i in 1:length(model_cv[["resample"]][["Resample"]])) {
  names(model_cv[["control"]][["index"]][i])
  trial1 <- data.frame(model_cv[["control"]][["index"]][i])
  trial1$Resample <- names(model_cv[["control"]][["index"]][i])
  trial1$set <- "analysis"
  names(trial1) <- c("rowIndex", "Resample", "set")
  trial1$Resample_num <- as.numeric(str_sub(trial1$Resample,-2,-1))
  trial1$Resample_num <- sprintf("Resample%02d", i)
  trial1 <- trial1[,c(1,3,4)]
  names(trial1) <- c("rowIndex", "set", "Resample")
  trial1 <- trial1[,c(1,3,2)]
  analysis_cv <- rbind(analysis_cv, trial1)

}

head(analysis_cv)


complete_cv <- rbind(assessment_cv, analysis_cv)
complete_cv$set <- as.factor(complete_cv$set)

chart1 <- ggplot(complete_cv, aes(x = Resample, y = rowIndex, fill = set)) +
  geom_tile(colour = 'black') +
  scale_fill_manual(values = c('analysis' = 'white', 'assessment' = 'blue')) +
  scale_x_discrete(breaks=unique(complete_cv$Resample),
                   labels=1:length(unique(complete_cv$Resample))) +
  scale_y_discrete(name ="Training Set Samples", 
                   limits=as.character(range40)) +
  labs(title = "V-FOLD CV")

chart1

#monte-carlo cv
swiss0 <- swiss[c(1:40),]  #it doesn't seem to work with only 20 data points
range40 <- 1:dim(swiss0)[1]
train.control <- trainControl(method = "LGOCV", number = 10,savePredictions = TRUE
                              ,p=0.90
                              )

model_mc <- train(Fertility ~., 
                  data = swiss0, 
                  method = "glm",
                  trControl = train.control)

model_mc[["control"]][["p"]]

assessment_mc <- data.frame()
for (i in 1:length(model_mc[["resample"]][["Resample"]])) {
  names(model_mc[["control"]][["indexOut"]][i])
  trial1 <- data.frame(model_mc[["control"]][["indexOut"]][i])
  trial1$Resample <- names(model_mc[["control"]][["indexOut"]][i])
  trial1$set <- "assessment"
  names(trial1) <- c("rowIndex", "Resample", "set")
  assessment_mc <- rbind(assessment_mc, trial1)
}

assessment_mc


assessment_mc$Resample <- as.factor(assessment_mc$Resample)
unique_resample <- unique(assessment_mc$Resample)
assessment_mc_plot <- ggplot(assessment_mc, aes(x=Resample, y=rowIndex)) + geom_point() +  
  theme(axis.text.x = element_text(face="bold",  size=8),
        axis.text.y = element_text(face="bold", size = 8)) + 
  ggtitle("MONTE CARLO CV") + 
  labs(subtitle="black dots represent the assessment set") + 
  scale_x_discrete(name = "Resamples", breaks=unique_resample,
                   labels=1:length(unique_resample)) +
  scale_y_discrete(name ="Training Set Samples", 
                   limits=as.character(range40))

assessment_mc_plot

grid.arrange(assessment_cv_plot, assessment_mc_plot, nrow = 1, ncol = 2,
             top = textGrob("V-FOLD CV (LEFT) & MONTE CARLO CV (RIGHT)",gp=gpar(fontsize=10, fontface="bold")),
             bottom = textGrob("black dots represents the assessment set in each resample",gp=gpar(fontsize=10)))

#let's make figure 3.6


# monte carlo cv - analysis set
analysis_mc <- data.frame()
for (i in 1:length(model_mc[["resample"]][["Resample"]])) {
  names(model_mc[["control"]][["index"]][i])
  trial1 <- data.frame(model_mc[["control"]][["index"]][i])
  trial1$Resample <- names(model_mc[["control"]][["index"]][i])
  trial1$set <- "analysis"
  names(trial1) <- c("rowIndex", "Resample", "set")
  analysis_mc <- rbind(analysis_mc, trial1)
}

analysis_mc

complete_mc <- rbind(assessment_mc, analysis_mc)
complete_mc$set <- as.factor(complete_mc$set)

chart2 <- ggplot(complete_mc, aes(x = Resample, y = rowIndex, fill = set)) +
  geom_tile(colour = 'black') +
  scale_fill_manual(values = c('analysis' = 'white', 'assessment' = 'blue')) +
  scale_x_discrete(breaks=unique(complete_mc$Resample),
                   labels=1:length(unique(complete_mc$Resample))) +
  scale_y_discrete(name ="Training Set Samples", 
                   limits=as.character(range40)) +
  labs(title = "MONTE CARLO CV")

chart2

#let's make figure 3.6
grid.arrange(chart1, chart2, nrow = 1, ncol = 2,
             top = textGrob("V-FOLD CV (LEFT) & MONTE CARLO CV (RIGHT)",gp=gpar(fontsize=10, fontface="bold")))



#bootstrap
swiss0 <- swiss[c(1:20),]  
range20 <- 1:dim(swiss0)[1]
train.control <- trainControl(method = "boot", number = 10,savePredictions = TRUE
                              #,p=0.90
                              )

model_boot <- train(Fertility ~., 
                  data = swiss0, 
                  method = "glm",
                  trControl = train.control)

assessment_boot <- data.frame()
for (i in 1:length(model_boot[["resample"]][["Resample"]])) {
  names(model_boot[["control"]][["indexOut"]][i])
  trial1 <- data.frame(model_boot[["control"]][["indexOut"]][i])
  trial1$Resample <- names(model_boot[["control"]][["indexOut"]][i])
  trial1$set <- "assessment"
  names(trial1) <- c("rowIndex", "Resample", "set")
  assessment_boot <- rbind(assessment_boot, trial1)
}

assessment_boot

analysis_boot <- data.frame()
for (i in 1:length(model_boot[["resample"]][["Resample"]])) {
  names(model_boot[["control"]][["index"]][i])
  trial1 <- data.frame(model_boot[["control"]][["index"]][i])
  trial1$Resample <- names(model_boot[["control"]][["index"]][i])
  trial1$set <- "analysis"
  names(trial1) <- c("rowIndex", "Resample", "set")
  analysis_boot <- rbind(analysis_boot, trial1)
}

analysis_boot
complete_boot <- rbind(assessment_boot, analysis_boot)

#let's make figure 3.7
count_analysis_boot <- analysis_boot %>% count(Resample, rowIndex)
names(count_analysis_boot) <- c("Resample","rowIndex","Count")
count_analysis_boot
count_analysis_boot$Resample <- as.factor(count_analysis_boot$Resample)
ggplot(count_analysis_boot, aes(Resample, rowIndex, fill= Count)) + 
  scale_fill_gradient(low="white", high="blue") +
  scale_x_discrete(name = "Resamples", breaks=unique(count_analysis_boot$Resample),
                   labels=1:length(unique(count_analysis_boot$Resample))) +
  scale_y_discrete(name ="Analysis Set", 
                   limits=as.character(unique(count_analysis_boot$rowIndex))) +
  geom_tile(colour = 'black')





# 3.4.4 Rolling Origin Forecasting
# you could also use the "training" dataset of chicago.RData

# Monthly sales data - rolling origin forecasting resampling with M = 10 and N = 2
data("drinks")
drinks <- drinks[1:20,]
drinks_tbl <- drinks %>%
  rename(sales = S4248SM144NCEN) %>%
  as_tibble()


resample_spec <- rolling_origin(
  drinks_tbl,
  initial    = 10,
  assess     = 2,
  cumulative = FALSE,
  #skip       = 2 * 12,
  #overlap    = 1
)

resample_spec

# Monthly sales data - cumulative rolling origin forecasting resampling with M = 10 and N = 2
data("drinks")
drinks <- drinks[1:20,]
range20 <- 1:dim(drinks)[1]

drinks_tbl <- drinks %>%
  rename(sales = S4248SM144NCEN) %>%
  as_tibble()


resample_spec_cumulative <- rolling_origin(
  drinks_tbl,
  initial    = 10,
  assess     = 2,
  cumulative = TRUE,
  #skip       = 2 * 12,
  #overlap    = 1
)

resample_spec_cumulative

data("drinks")
drinks2 <- drinks[1:40,]
range_skip <- 1:dim(drinks2)[1]
drinks_tbl2 <- drinks2 %>%
  rename(sales = S4248SM144NCEN) %>%
  as_tibble()


resample_spec_skip <- rolling_origin(
  drinks_tbl2,
  initial    = 10,
  assess     = 2,
  cumulative = FALSE,
  skip       = 13,
  #overlap    = 1
)

resample_spec_skip



# let's make figure 3.8

datalist_rolling1 = list()
rolling1_total <- data.frame()
rolling2_total <- data.frame()


for (i in 1:nrow(resample_spec)){
  B <- data.frame(matrix(resample_spec[[1]][[i]][["out_id"]]))
  new1 <- B
  names(new1) <- "rowIndex"
  new1$Resample <- sprintf("Fold%02d", i)
  rolling1_total <- rbind(rolling1_total,new1)
  #datalist_monte00[[i]] <- new1
}

rolling1_total$set <- "assessment"
rolling1_total

for (i in 1:nrow(resample_spec)){
  B <- data.frame(matrix(resample_spec[[1]][[i]][["in_id"]]))
  new1 <- B
  names(new1) <- "rowIndex"
  new1$Resample <- sprintf("Fold%02d", i)
  rolling2_total <- rbind(rolling2_total,new1)
  #datalist_monte00[[i]] <- new1
}

rolling2_total$set <- "analysis"
rolling2_total

rolling3_total <- rbind(rolling1_total, rolling2_total)


rolling4_total <- data.frame()

for (i in unique(rolling3_total$Resample)) {
  B <- rolling3_total[rolling3_total$Resample == i,"rowIndex"]
  new1 <- data.frame(range20[!range20 %in% B])
  names(new1) <- "rowIndex"
  new1$Resample <- i
  rolling4_total <- rbind(rolling4_total,new1)
  #datalist_monte00[[i]] <- new1
}

rolling4_total$set <- "unused"
rolling5_total <- rbind(rolling3_total, rolling4_total)
rolling5_total$set <- as.factor(rolling5_total$set)
rolling5_total_ordered <- rolling5_total[order(rolling5_total$Resample),]

chart3 <- ggplot(rolling5_total_ordered, aes(x = Resample, y = rowIndex, fill = set)) +
  geom_tile(colour = 'black') +
  scale_fill_manual(values = c('analysis' = 'red', 'assessment' = 'blue', 'unused' = 'white')) +
  scale_x_discrete(breaks=unique(rolling5_total_ordered$Resample),
                   labels=1:length(unique(rolling5_total_ordered$Resample))) +
  scale_y_discrete(name ="Training Set Samples", 
                   limits=as.character(range20)) +
  labs(title = "Rolling Origin")

chart3

#cumulative rolling origin
datalist_rolling1 = list()
rolling1_total <- data.frame()
rolling2_total <- data.frame()


for (i in 1:nrow(resample_spec_cumulative)){
  B <- data.frame(matrix(resample_spec_cumulative[[1]][[i]][["out_id"]]))
  new1 <- B
  names(new1) <- "rowIndex"
  new1$Resample <- sprintf("Fold%02d", i)
  rolling1_total <- rbind(rolling1_total,new1)
  #datalist_monte00[[i]] <- new1
}

rolling1_total$set <- "assessment"
rolling1_total

for (i in 1:nrow(resample_spec_cumulative)){
  B <- data.frame(matrix(resample_spec_cumulative[[1]][[i]][["in_id"]]))
  new1 <- B
  names(new1) <- "rowIndex"
  new1$Resample <- sprintf("Fold%02d", i)
  rolling2_total <- rbind(rolling2_total,new1)
  #datalist_monte00[[i]] <- new1
}

rolling2_total$set <- "analysis"
rolling2_total

rolling3_total <- rbind(rolling1_total, rolling2_total)


rolling4_total <- data.frame()

for (i in unique(rolling3_total$Resample)) {
  B <- rolling3_total[rolling3_total$Resample == i,"rowIndex"]
  new1 <- data.frame(range20[!range20 %in% B])
  names(new1) <- "rowIndex"
  new1$Resample <- i
  rolling4_total <- rbind(rolling4_total,new1)
  #datalist_monte00[[i]] <- new1
}

rolling4_total$set <- "unused"
rolling5_total <- rbind(rolling3_total, rolling4_total)
rolling5_total$set <- as.factor(rolling5_total$set)
rolling5_total_ordered <- rolling5_total[order(rolling5_total$Resample),]

chart3 <- ggplot(rolling5_total_ordered, aes(x = Resample, y = rowIndex, fill = set)) +
  geom_tile(colour = 'black') +
  scale_fill_manual(values = c('analysis' = 'red', 'assessment' = 'blue', 'unused' = 'white')) +
  scale_x_discrete(breaks=unique(rolling5_total_ordered$Resample),
                   labels=1:length(unique(rolling5_total_ordered$Resample))) +
  scale_y_discrete(name ="Training Set Samples", 
                   limits=as.character(range20)) +
  labs(title = "Cumulative Rolling Origin")

chart3

#skip rolling origin
datalist_rolling1 = list()
rolling1_total <- data.frame()
rolling2_total <- data.frame()


for (i in 1:nrow(resample_spec_skip)){
  B <- data.frame(matrix(resample_spec_skip[[1]][[i]][["out_id"]]))
  new1 <- B
  names(new1) <- "rowIndex"
  new1$Resample <- sprintf("Fold%02d", i)
  rolling1_total <- rbind(rolling1_total,new1)
  #datalist_monte00[[i]] <- new1
}

rolling1_total$set <- "assessment"
rolling1_total

for (i in 1:nrow(resample_spec_skip)){
  B <- data.frame(matrix(resample_spec_skip[[1]][[i]][["in_id"]]))
  new1 <- B
  names(new1) <- "rowIndex"
  new1$Resample <- sprintf("Fold%02d", i)
  rolling2_total <- rbind(rolling2_total,new1)
  #datalist_monte00[[i]] <- new1
}

rolling2_total$set <- "analysis"
rolling2_total

rolling3_total <- rbind(rolling1_total, rolling2_total)


rolling4_total <- data.frame()

for (i in unique(rolling3_total$Resample)) {
  B <- rolling3_total[rolling3_total$Resample == i,"rowIndex"]
  new1 <- data.frame(range_skip[!range_skip %in% B])
  names(new1) <- "rowIndex"
  new1$Resample <- i
  rolling4_total <- rbind(rolling4_total,new1)
  #datalist_monte00[[i]] <- new1
}

rolling4_total$set <- "unused"
rolling5_total <- rbind(rolling3_total, rolling4_total)
rolling5_total$set <- as.factor(rolling5_total$set)
rolling5_total_ordered <- rolling5_total[order(rolling5_total$Resample),]

chart3 <- ggplot(rolling5_total_ordered, aes(x = Resample, y = rowIndex, fill = set)) +
  geom_tile(colour = 'black') +
  scale_fill_manual(values = c('analysis' = 'red', 'assessment' = 'blue', 'unused' = 'white')) +
  scale_x_discrete(breaks=unique(rolling5_total_ordered$Resample),
                   labels=1:length(unique(rolling5_total_ordered$Resample))) +
  scale_y_discrete(name ="Training Set Samples", 
                   limits=as.character(range_skip)
                   ) +
  labs(title = "Rolling Origin - skip")

chart3



#3.4.6 Variance and Bias in Resampling

#10-fold cross-validation vs. repeated 5-fold cross-validation vs. repeated 10-fold cross-validation
#10-fold cross-validation vs. 5-fold cross-validation
#10-fold cross-validation vs. monte-carlo π = 0.10 and B = 10
#10-fold cross-validation vs. bootstraps

# Load logistic model results for OkC data

path0 = "/Users/danielececcarelli/Documents/2 - High Dimensional Data Analysis/final_project_FES/"
file0 = "lm_date_only.RData"
file00 = "chicago.RData"
file1 = paste0(path0, file0)
file11 = paste0(path0, file00)
load(file1)
load(file11)


# Define training control
set.seed(123) 

ctrl_cv10 <- trainControl(method = "cv", number = 10, savePredictions = TRUE)
ctrl_cv5 <- trainControl(method = "cv", number = 5, savePredictions = TRUE)
ctrl_cv10_repeated10 <- trainControl(method = "repeatedcv", number = 10, repeats = 10, savePredictions = TRUE)
ctrl_cv10_repeated5 <- trainControl(method = "repeatedcv", number = 10, repeats = 5, savePredictions = TRUE)
ctrl_cv5_repeated5 <- trainControl(method = "repeatedcv", number = 5, repeats = 5, savePredictions = TRUE)
ctrl_mc <- trainControl(method = "LGOCV", number = 10, savePredictions = TRUE, p=0.90)
ctrl_bootstraps <- trainControl(method = "boot", number = 10, savePredictions = TRUE)

#lm_date_only[["call"]]
set.seed(123) 
model_cv10 <- train(form = s_40380 ~ ., 
                          data = training[, c("s_40380", var_sets$dates)], 
                          method = "glm", 
                          metric = "RMSE", 
                          maximize = FALSE, 
                          model = FALSE, 
                          trControl = ctrl_cv10)

set.seed(123) 
model_cv5 <- train(form = s_40380 ~ ., 
                            data = training[, c("s_40380", var_sets$dates)], 
                            method = "glm", 
                            metric = "RMSE", 
                            maximize = FALSE, 
                            model = FALSE, 
                            trControl = ctrl_cv5)

set.seed(123) 
model_cv10_repeated10 <- train(form = s_40380 ~ ., 
                            data = training[, c("s_40380", var_sets$dates)], 
                            method = "glm", 
                            metric = "RMSE", 
                            maximize = FALSE, 
                            model = FALSE, 
                            trControl = ctrl_cv10_repeated10)

set.seed(123) 
model_cv10_repeated5 <- train(form = s_40380 ~ ., 
                            data = training[, c("s_40380", var_sets$dates)], 
                            method = "glm", 
                            metric = "RMSE", 
                            maximize = FALSE, 
                            model = FALSE, 
                            trControl = ctrl_cv10_repeated5)


set.seed(123) 
model_cv5_repeated5 <- train(form = s_40380 ~ ., 
                              data = training[, c("s_40380", var_sets$dates)], 
                              method = "glm", 
                              metric = "RMSE", 
                              maximize = FALSE, 
                              model = FALSE, 
                              trControl = ctrl_cv5_repeated5)


set.seed(123) 
model_mc <- train(form = s_40380 ~ ., 
                            data = training[, c("s_40380", var_sets$dates)], 
                            method = "glm", 
                            metric = "RMSE", 
                            maximize = FALSE, 
                            model = FALSE, 
                            trControl = ctrl_mc)

set.seed(123) 
model_bootstraps <- train(form = s_40380 ~ ., 
                            data = training[, c("s_40380", var_sets$dates)], 
                            method = "glm", 
                            metric = "RMSE", 
                            maximize = FALSE, 
                            model = FALSE, 
                            trControl = ctrl_bootstraps)


models_vector <- c("10-fold Cross Validation", 
                   "5-fold Cross Validation", 
                   "10-fold Cross Validation_repeated 10 times", 
                   "10-fold Cross Validation_repeated 5 times", 
                   "5-fold Cross Validation_repeated 5 times",
                   "Monte Carlo_10_fold_p_0.90", 
                   "Bootstraps_10_fold")

rmse_list <- c(model_cv10[["results"]][["RMSE"]], 
               model_cv5[["results"]][["RMSE"]], 
               model_cv10_repeated10[["results"]][["RMSE"]], 
               model_cv10_repeated5[["results"]][["RMSE"]],
               model_cv5_repeated5[["results"]][["RMSE"]],
               model_mc[["results"]][["RMSE"]], 
               model_bootstraps[["results"]][["RMSE"]])


prediction_variance_list <- c(var(model_cv10[["pred"]][["pred"]]), 
                              var(model_cv5[["pred"]][["pred"]]), 
                              var(model_cv10_repeated10[["pred"]][["pred"]]), 
                              var(model_cv10_repeated5[["pred"]][["pred"]]),
                              var(model_cv5_repeated5[["pred"]][["pred"]]),
                              var(model_mc[["pred"]][["pred"]]), 
                              var(model_bootstraps[["pred"]][["pred"]]))

prediction_standard_deviation_list <- c(sd(model_cv10[["pred"]][["pred"]]), 
                                        sd(model_cv5[["pred"]][["pred"]]), 
                                        sd(model_cv10_repeated10[["pred"]][["pred"]]), 
                                        sd(model_cv10_repeated5[["pred"]][["pred"]]),
                                        sd(model_cv5_repeated5[["pred"]][["pred"]]),
                                        sd(model_mc[["pred"]][["pred"]]), 
                                        sd(model_bootstraps[["pred"]][["pred"]]))


df_models <- data.frame(models_vector, rmse_list, prediction_variance_list, prediction_standard_deviation_list)
df_models


# Section 3.4.7 - What Should Be Included Inside of Resampling?

# https://stackoverflow.com/questions/50295233/preprocess-within-cross-validation-in-caret 
# https://stackoverflow.com/questions/68139936/data-leakage-when-feature-scaling-with-k-fold-cross-validation-in-r
# https://github.com/topepo/caret/issues/335
# https://stackoverflow.com/questions/60490750/pca-within-cross-validation-however-only-with-a-subset-of-variables

# preProcess = A string vector that defines a pre-processing of the predictor data. 
#              Current possibilities are "BoxCox", "YeoJohnson", "expoTrans", "center", "scale", 
#              "range", "knnImpute", "bagImpute", "medianImpute", "pca", "ica" and "spatialSign".

# consider also "train.recipe" --> "When using the recipe method, x should be an unprepared recipe object 
#                                    that describes the model terms (i.e. outcome, predictors, etc.) as well as 
#                                     any pre-processing that should be done to the data."

# check out this link from the author of the book about downsampling and cross-validation
# https://topepo.github.io/caret/subsampling-for-class-imbalances.html   --> read 11.1 & 11.2

set.seed(123) 

ctrl_cv10_pre <- trainControl(method = "cv", number = 10, savePredictions = TRUE)

set.seed(123) 
model_cv10_pre <- train(form = s_40380 ~ ., 
                    data = training[, c("s_40380", var_sets$dates)], 
                    method = "glm", 
                    metric = "RMSE", 
                    maximize = FALSE, 
                    model = FALSE, 
                    trControl = ctrl_cv10,
                    preProcess = "center"
                    )

model_cv10_pre

set.seed(123) 
model_cv10_pre <- train(form = s_40380 ~ ., 
                        data = training[, c("s_40380", var_sets$dates)], 
                        method = "glm", 
                        metric = "RMSE", 
                        maximize = FALSE, 
                        model = FALSE, 
                        trControl = ctrl_cv10,
                        preProcess = c("BoxCox", "center", "scale")
                        )


model_cv10_pre

