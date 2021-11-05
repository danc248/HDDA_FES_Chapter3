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

range20 <- 1:20
#range1 <- data.frame(range0)

data("swiss")
swiss0 <- swiss[c(1:20),]
# Define training control
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10, savePredictions = TRUE)
# Train the model
model0 <- train(Fertility ~., data = swiss0, method = "lm",
               trControl = train.control)
# Summarize the results
print(model0)

# let's make figure 3.6
fig3_6 <- model0$pred[, c(3,5)]
number_of_folds <- length(unique(fig3_6$Resample))  #we previously asked for a 10-fold CV in trainControl funtion
print(number_of_folds)
#fig3_6_one_hot <- dummyVars(" ~ .", data=fig3_6)
#fig3_6_one_hot <- data.frame(predict(fig3_6_one_hot, newdata = fig3_6))  #you need to visualize this dataframe

set.seed(123)
monte_carlo_cv0 <- mc_cv(swiss0, prop = 9/10, times = 10)
datalist_monte = list()
monte_carlo_total <- data.frame()


for (i in 1:nrow(monte_carlo_cv0)){
  B <- monte_carlo_cv0[[1]][[i]][["in_id"]]
  new1 <- data.frame(range20[!range20 %in% B])
  names(new1) <- "rowIndex"
  new1$Resample <- sprintf("Fold%02d", i)
  monte_carlo_total <- rbind(monte_carlo_total,new1)
  #datalist_monte00[[i]] <- new1
}

monte_carlo_total
number_of_folds <- length(unique(monte_carlo_total$Resample))  #we previously asked for a 10-fold CV in trainControl funtion
print(number_of_folds)

#monte_carlo_total_one_hot <- dummyVars(" ~ .", data=monte_carlo_total)
#monte_carlo_total_one_hot <- data.frame(predict(monte_carlo_total_one_hot, newdata = monte_carlo_total))  #you need to visualize this dataframe
#monte_carlo_total_one_hot



fig3_6$Resample_num <- as.numeric(str_sub(fig3_6$Resample,-2,-1))
fig3_6$Resample_num
v_fold <- fig3_6
monte_carlo_total$Resample_num <- as.numeric(str_sub(monte_carlo_total$Resample,-2,-1))
monte_carlo_total$Resample_num



v_fold$Resample_num <- as.factor(v_fold$Resample_num)
v_fold$rowIndex <- as.factor(v_fold$rowIndex)
monte_carlo_total$Resample_num <- as.factor(monte_carlo_total$Resample_num)
monte_carlo_total$rowIndex <- as.factor(monte_carlo_total$rowIndex)

# let's make figure 3.6 - black dots represent the assessment set in each resample
v_fold_cv_plot <- ggplot(v_fold, aes(x=Resample_num, y=rowIndex)) + geom_point()
v_fold_cv_plot +  theme(axis.text.x = element_text(face="bold",  size=15),
                         axis.text.y = element_text(face="bold", size = 8))
v_fold_cv_plot + ggtitle("V-FOLD CV") + 
  labs(subtitle="black dots represent the assessment set") + 
  scale_x_discrete(name = "Resamples") + 
  scale_y_discrete(name ="Training Set Samples", 
                   limits=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19", "20"))

monte_carlo_plot <- ggplot(monte_carlo_total, aes(x=Resample_num, y=rowIndex)) + geom_point()
monte_carlo_plot +  theme(axis.text.x = element_text(face="bold",  size=15),
                        axis.text.y = element_text(face="bold", size = 8))
monte_carlo_plot + ggtitle("MONTE CARLO CV") + 
  labs(subtitle="black dots represent the assessment set") + 
  scale_x_discrete(name = "Resamples") + 
  scale_y_discrete(name ="Training Set Samples", 
                   limits=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19", "20"))

grid.arrange(v_fold_cv_plot, montecarlo_plot, nrow = 1, ncol = 2,
             top = textGrob("V-FOLD CV (LEFT) & MONTE CARLO CV (RIGHT)",gp=gpar(fontsize=10, fontface="bold")),
             bottom = textGrob("black dots represents the assessment set in each resample",gp=gpar(fontsize=10)))


# 3.4.3 The Bootstrap

# Define training control
#et.seed(123) 
#train.control_boot <- trainControl(method = "boot", number = 10, savePredictions = TRUE)
# Train the model
#model0_boot <- train(Fertility ~., data = swiss0, method = "lm",
#                trControl = train.control_boot)
# Summarize the results
#print(model0_boot)

#boot1 <- model0_boot$pred[, c(3,5)]
#number_of_folds <- length(unique(fig3_6$Resample))  #we previously asked for a 10-fold CV in trainControl funtion
#print(number_of_folds)
#boot1$Resample_num <- as.numeric(str_sub(boot1$Resample,-2,-1))
#boot1

# 3.4.3 The Bootstrap
set.seed(123)
boots0 <- bootstraps(swiss0, times = 10, apparent = TRUE)
boots0

datalist_boot = list()
boot_total <- data.frame()


for (i in 1:nrow(boots0)){
  B <- boots0[[1]][[i]][["in_id"]]
  new1 <- data.frame(range20[!range20 %in% B])
  names(new1) <- "rowIndex"
  new1$Resample <- sprintf("Fold%02d", i)
  boot_total <- rbind(boot_total,new1)
  #datalist_monte00[[i]] <- new1
}

boot_total
number_of_folds <- length(unique(boot_total$Resample))  #we previously asked for a 10-fold CV in trainControl funtion
print(number_of_folds)
boot_total$Resample_num <- as.numeric(str_sub(boot_total$Resample,-2,-1))
boot1 <- boot_total
boot1$Resample_num <- as.factor(boot1$Resample_num)
boot1$rowIndex <- as.factor(boot1$rowIndex)

# FIGURE 3.7
boot1_plot <- ggplot(boot1, aes(x=Resample_num, y=rowIndex)) + geom_point()
boot1_plot +  theme(axis.text.x = element_text(face="bold",  size=15),
                        axis.text.y = element_text(face="bold", size = 8))
boot1_plot + ggtitle("BOOTSTRAP") + 
  labs(subtitle="black dots represent the assessment set") + 
  scale_x_discrete(name = "Resamples") + 
  scale_y_discrete(name ="Training Set Samples", 
                   limits=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19", "20"))


# 3.4.4 Rolling Origin Forecasting

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

