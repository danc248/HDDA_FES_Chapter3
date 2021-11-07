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
swiss0 <- swiss[c(1:40),]
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

range40 <- as.character(1:40)
assessment_cv$Resample <- as.factor(assessment_cv$Resample)
assessment_cv_plot <- ggplot(assessment_cv, aes(x=Resample, y=rowIndex)) + geom_point() +  
  theme(axis.text.x = element_text(face="bold",  size=8),
        axis.text.y = element_text(face="bold", size = 8)) + 
  ggtitle("V-FOLD CV") + 
  labs(subtitle="black dots represent the assessment set") + 
  scale_x_discrete(name = "Resamples", breaks=c("Resample01","Resample02","Resample03","Resample04","Resample05","Resample06","Resample07","Resample08","Resample09","Resample10"),
                   labels=c("1","2","3","4","5","6","7","8","9","10")) + 
  scale_y_discrete(name ="Training Set Samples", 
                   limits=range40)

assessment_cv_plot


analysis_cv <- data.frame()
for (i in 1:length(model_cv[["resample"]][["Resample"]])) {
  names(model_cv[["control"]][["index"]][i])
  trial1 <- data.frame(model_cv[["control"]][["index"]][i])
  trial1$Resample <- names(model_cv[["control"]][["index"]][i])
  trial1$set <- "analysis"
  names(trial1) <- c("rowIndex", "Resample", "set")
  analysis_cv <- rbind(analysis_cv, trial1)
}

analysis_cv


complete_cv <- rbind(assessment_cv, analysis_cv)

#monte-carlo cv
swiss0 <- swiss[c(1:40),]  #it doesn't seem to work with only 20 data points
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

range40 <- as.character(1:40)
range40

assessment_mc$Resample <- as.factor(assessment_mc$Resample)
assessment_mc_plot <- ggplot(assessment_mc, aes(x=Resample, y=rowIndex)) + geom_point() +  
  theme(axis.text.x = element_text(face="bold",  size=8),
        axis.text.y = element_text(face="bold", size = 8)) + 
  ggtitle("MONTE CARLO CV") + 
  labs(subtitle="black dots represent the assessment set") + 
  scale_x_discrete(name = "Resamples", breaks=c("Resample01","Resample02","Resample03","Resample04","Resample05","Resample06","Resample07","Resample08","Resample09","Resample10"),
                   labels=c("1","2","3","4","5","6","7","8","9","10")) +
  scale_y_discrete(name ="Training Set Samples", 
                   limits=range40)

assessment_mc_plot

grid.arrange(assessment_cv_plot, assessment_mc_plot, nrow = 1, ncol = 2,
             top = textGrob("V-FOLD CV (LEFT) & MONTE CARLO CV (RIGHT)",gp=gpar(fontsize=10, fontface="bold")),
             bottom = textGrob("black dots represents the assessment set in each resample",gp=gpar(fontsize=10)))



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

#bootstrap
swiss0 <- swiss[c(1:20),]  
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
count_analysis_boot <- analysis_boot %>% count(Resample, rowIndex)
names(count_analysis_boot) <- c("Resample","rowIndex","Count")
count_analysis_boot
count_analysis_boot$Resample <- as.factor(count_analysis_boot$Resample)
ggplot(count_analysis_boot, aes(Resample, rowIndex, fill= Count)) + 
  scale_fill_gradient(low="white", high="blue") +
  scale_x_discrete(name = "Resamples", breaks=c("Resample01","Resample02","Resample03","Resample04","Resample05","Resample06","Resample07","Resample08","Resample09","Resample10"),
                   labels=c("1","2","3","4","5","6","7","8","9","10")) +
  scale_y_discrete(name ="Analysis Set", 
                   limits=1:20) +
  geom_tile()

complete_boot <- rbind(assessment_boot, analysis_boot)
