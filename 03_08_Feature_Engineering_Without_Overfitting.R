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

theme_set(theme_bw() + theme(legend.position = "top"))

# guarda i codici della sezione 01_03 del libro
# https://github.com/topepo/FES/tree/master/01_Introduction/1_03_A_More_Complex_Example 

path0 = "/Users/danielececcarelli/Documents/2 - High Dimensional Data Analysis/final_project_FES/"
file01 = "chicago.RData"
file02 = "lm_date_only.RData"
file03 = "boost_date_lag14.RData"
file11 = paste0(path0, file01)
file22 = paste0(path0, file02)
file33 = paste0(path0, file03)
load(file11)
load(file22)
load(file33)

date_pred_2014 <- date_pred[which(date_pred$date >= "2014-12-15" & date_pred$date <= "2014-12-30"), ]
date_pred_2015 <- date_pred[which(date_pred$date >= "2015-12-15" & date_pred$date <= "2015-12-30"), ]
date_pred_2014$holiday <- as.numeric(apply(date_pred_2014[,-1:-6], 1, sum))
date_pred_2015$holiday <- as.numeric(apply(date_pred_2015[,-1:-6], 1, sum))
date_pred_2014 <- date_pred_2014[, c("date", "holiday")]
date_pred_2015 <- date_pred_2015[, c("date", "holiday")]
date_pred_2014$holiday <- ifelse(date_pred_2014$holiday == 0, 0, 1)
date_pred_2015$holiday <- ifelse(date_pred_2015$holiday == 0, 0, 1)

date_pred_2014 <- data.frame(date_pred_2014)
date_pred_2015 <- data.frame(date_pred_2015)
names(date_pred_2014)[names(date_pred_2014) == 'date'] <- 'Resample'
names(date_pred_2015)[names(date_pred_2015) == 'date'] <- 'Resample'
date_pred_2014
date_pred_2015


# method0 <- lm_date_only[["control"]][["method"]]   # timeslice
# number0 <- lm_date_only[["control"]][["number"]]
# search0  <- lm_date_only[["control"]][["search"]]   #the author used "grid" search in its glm model
# initial0 <- lm_date_only[["control"]][["initialWindow"]]
# horizon0 <- lm_date_only[["control"]][["horizon"]]
# fixed0 <- lm_date_only[["control"]][["fixedWindow"]]
# skip0 <- lm_date_only[["control"]][["skip"]]
# save0 <- lm_date_only[["control"]][["savePredictions"]]
# return0 <- lm_date_only[["control"]][["returnResamp"]]
# verbose0 <- lm_date_only[["control"]][["verboseIter"]]
# return10 <- lm_date_only[["control"]][["returnData"]]
# 
# set.seed(4194)
# ctrl <- trainControl(method = method0,
#                      number = number0,
#                      search = search0,
#                      initialWindow = initial0,
#                      horizon = horizon0,
#                      fixedWindow = fixed0,
#                      skip = skip0,
#                      savePredictions = save0,
#                      returnResamp = return0,
#                      verboseIter = verbose0,
#                      returnData = return10
#                      )

pred_vars <- c(var_sets$dates, var_sets$lag14, var_sets$holidays)
set.seed(4194)
ctrl <- lm_date_only[["control"]]
set.seed(4194)
lm_date_lag14_hol <- train(s_40380 ~ .,
                           data = training[, c("s_40380", pred_vars)],
                           method = "lm",
                           metric = "RMSE",
                           maximize = FALSE,
                           model = FALSE,
                           trControl = ctrl)


unique(lm_date_lag14_hol$pred$Resample)
lm_date_lag14_hol$pred$Resample[lm_date_lag14_hol$pred$Resample == "2014-12-08"]
lm_date_lag14_hol$pred$Resample[lm_date_lag14_hol$pred$Resample == "2014-12-22"]
lm_date_lag14_hol$pred$Resample[lm_date_lag14_hol$pred$Resample == "2015-12-07"]
lm_date_lag14_hol$pred$Resample[lm_date_lag14_hol$pred$Resample == "2015-12-21"]

unique(lm_date_only$pred$Resample)
lm_date_only$pred$Resample[lm_date_only$pred$Resample == "2014-12-08"]
lm_date_only$pred$Resample[lm_date_only$pred$Resample == "2014-12-22"]
lm_date_only$pred$Resample[lm_date_only$pred$Resample == "2015-12-07"]
lm_date_only$pred$Resample[lm_date_only$pred$Resample == "2015-12-21"]

dec_2014 <- seq(as.Date("2014-12-15"), as.Date("2014-12-30"), by="days")
length(dec_2014)
dec_2015 <- seq(as.Date("2015-12-15"), as.Date("2015-12-30"), by="days")
length(dec_2015)

df_dec_2014 <- data.frame(dec_2014)
df_dec_2014$day <- weekdays(df_dec_2014$dec_2014, abbreviate = TRUE)
df_dec_2015 <- data.frame(dec_2015)
df_dec_2015$day <- weekdays(df_dec_2015$dec_2015, abbreviate = TRUE)
week0 <- unique(df_dec_2015$day)

df_dec_2014$weekday_end <- ifelse(df_dec_2014$day %in% c("Sat", "Sun"), "Weekend", "Weekday")
names(df_dec_2014)[names(df_dec_2014) == 'dec_2014'] <- 'Resample'
df_dec_2014
df_dec_2015$weekday_end <- ifelse(df_dec_2015$day %in% c("Sat", "Sun"), "Weekend", "Weekday")
names(df_dec_2015)[names(df_dec_2015) == 'dec_2015'] <- 'Resample'
df_dec_2015


lm_new <- lm_date_only$pred[,c("pred","obs", "Resample")]
lm_new$Resample <- as.Date(lm_new$Resample)
str(lm_new)

lm_new_00 <- lm_date_lag14_hol$pred[,c("pred","obs", "Resample")]
lm_new_00$Resample <- as.Date(lm_new00$Resample)
str(lm_new00)

lm_new_2014 <- lm_new[which(lm_new$Resample > "2014-12-06" & lm_new$Resample < "2015-01-05"), ]
d14 <- unique(lm_new_2014$Resample)[1]
d14 <- seq(d14, as.Date("2014-12-30"), by="days")
lm_new_2014 <- lm_new_2014[1:length(d14),]
lm_new_2014$Resample <- d14
lm_new_2014 <- lm_new_2014[which(lm_new_2014$Resample >= "2014-12-15" & lm_new_2014$Resample <= "2014-12-30"), ]
lm_new_2014 <- merge(lm_new_2014, df_dec_2014, by = "Resample")
lm_new_2014 <- merge(lm_new_2014, date_pred_2014)
names(lm_new_2014)[names(lm_new_2014) == 'pred'] <- 'model_original'
lm_new_2014 <- lm_new_2014[,c("Resample","day","weekday_end","holiday","obs","model_original")]
head(lm_new_2014)

lm_new_2014_00 <- lm_new_00[which(lm_new_00$Resample > "2014-12-06" & lm_new_00$Resample < "2015-01-05"), ]
d14 <- unique(lm_new_2014_00$Resample)[1]
d14 <- seq(d14, as.Date("2014-12-30"), by="days")
lm_new_2014_00 <- lm_new_2014_00[1:length(d14),]
lm_new_2014_00$Resample <- d14
lm_new_2014_00 <- lm_new_2014_00[which(lm_new_2014_00$Resample >= "2014-12-15" & lm_new_2014_00$Resample <= "2014-12-30"), ]
names(lm_new_2014_00)[names(lm_new_2014_00) == 'pred'] <- 'model_holidays'
lm_new_2014_01 <- lm_new_2014_00[,c("Resample", "model_holidays")]
#lm_new_2014_00 <- merge(lm_new_2014_00, df_dec_2014, by = "Resample")
lm_new_2014$obs == lm_new_2014_01$obs
head(lm_new_2014_01)

lm_new_2015 <- lm_new[which(lm_new$Resample > "2015-12-06" & lm_new$Resample < "2016-01-05"), ]
d15 <- unique(lm_new_2015$Resample)[1]
d15 <- seq(d15, as.Date("2015-12-30"), by="days")
lm_new_2015 <- lm_new_2015[1:length(d15),]
lm_new_2015$Resample <- d15
lm_new_2015 <- lm_new_2015[which(lm_new_2015$Resample >= "2015-12-15" & lm_new_2015$Resample <= "2015-12-30"), ]
lm_new_2015 <- merge(lm_new_2015, df_dec_2015, by = "Resample")
lm_new_2015 <- merge(lm_new_2015, date_pred_2015)
names(lm_new_2015)[names(lm_new_2015) == 'pred'] <- 'model_original'
lm_new_2015 <- lm_new_2015[,c("Resample","day","weekday_end","holiday","obs","model_original")]
head(lm_new_2015)

lm_new_2015_00 <- lm_new_00[which(lm_new_00$Resample > "2015-12-06" & lm_new_00$Resample < "2016-01-05"), ]
d15 <- unique(lm_new_2015_00$Resample)[1]
d15 <- seq(d15, as.Date("2015-12-30"), by="days")
lm_new_2015_00 <- lm_new_2015_00[1:length(d15),]
lm_new_2015_00$Resample <- d15
lm_new_2015_00 <- lm_new_2015_00[which(lm_new_2015_00$Resample >= "2015-12-15" & lm_new_2015_00$Resample <= "2015-12-30"), ]
names(lm_new_2015_00)[names(lm_new_2015_00) == 'pred'] <- 'model_holidays'
lm_new_2015_01 <- lm_new_2015_00[,c("Resample", "model_holidays")]
#lm_new_2015_00 <- merge(lm_new_2015_00, df_dec_2015, by = "Resample")
lm_new_2015$obs == lm_new_2015_01$obs
head(lm_new_2015_01)

lm_2014 <- merge(lm_new_2014,lm_new_2014_01)
lm_2015 <- merge(lm_new_2015,lm_new_2015_01)
head(lm_2014)
head(lm_2015)

lm_2014_melt <- melt(lm_2014, id.vars = c('Resample', 'day', 'weekday_end', 'holiday'), variable.name = 'series')
head(lm_2014_melt)

lm_2015_melt <- melt(lm_2015, id.vars = c('Resample', 'day', 'weekday_end', 'holiday'), variable.name = 'series')
head(lm_2015_melt)


lm_2014_plot <- ggplot(lm_2014_melt, aes(Resample,value)) + 
  geom_line(aes(colour = series)) +
  #geom_line(aes(linetype=series)) +
  #scale_linetype_manual(values=c("twodash", "dotted", "solid")) +
  geom_point(aes(shape=weekday_end, colour = day, size=0.2)) +
  theme(legend.position="top") +
  labs(title = "December 2014 Chicago s_40380 train rides",
       subtitle = "LINEAR MODEL") +
  xlab("Date") +
  ylab("Ridership")

lm_2014_plot

lm_2015_plot <- ggplot(lm_2015_melt, aes(Resample,value)) + 
  geom_line(aes(colour = series)) +
  #geom_line(aes(linetype=series)) +
  #scale_linetype_manual(values=c("twodash", "dotted", "solid")) +
  geom_point(aes(shape=weekday_end, colour = day, size=0.2)) +
  theme(legend.position="top") +
  labs(title = "December 2015 Chicago s_40380 train rides",
       subtitle = "LINEAR MODEL") +
  xlab("Date") +
  ylab("Ridership")

lm_2015_plot

grid.arrange(lm_2014_plot, lm_2015_plot, nrow = 2, ncol = 1,
             top = textGrob("2014 (top) & 2015 (bottom)",gp=gpar(fontsize=10, fontface="bold"))
            )
            #,
            #bottom = textGrob("black dots represents the assessment set in each resample",gp=gpar(fontsize=10)))

# ------------------------------------------------------------------------------

# SVM

method0 <- boost_date_lag14[["control"]][["method"]]   # timeslice
number0 <- boost_date_lag14[["control"]][["number"]]
search0  <- boost_date_lag14[["control"]][["search"]]   #the author used "grid" search in its glm model
initial0 <- boost_date_lag14[["control"]][["initialWindow"]]
horizon0 <- boost_date_lag14[["control"]][["horizon"]]
fixed0 <- boost_date_lag14[["control"]][["fixedWindow"]]
skip0 <- boost_date_lag14[["control"]][["skip"]]
save0 <- boost_date_lag14[["control"]][["savePredictions"]]
return0 <- boost_date_lag14[["control"]][["returnResamp"]]
verbose0 <- boost_date_lag14[["control"]][["verboseIter"]]
return10 <- boost_date_lag14[["control"]][["returnData"]]

# set.seed(4194)
# ctrl <- trainControl(method = method0,
#                      number = number0,
#                      search = search0,
#                      initialWindow = initial0,
#                      horizon = horizon0,
#                      fixedWindow = fixed0,
#                      skip = skip0,
#                      savePredictions = save0,
#                      returnResamp = return0,
#                      verboseIter = verbose0,
#                      returnData = return10
#                      )


set.seed(4194)
ctrl <- boost_date_lag14[["control"]]
rand_ctrl <- ctrl
rand_ctrl$search <- "random"

pred_vars <- c(var_sets$dates, var_sets$lag14)

set.seed(4194)
svmr_date_lag14 <- train(s_40380 ~ .,
                         data = training[, c("s_40380", pred_vars)],
                         method = "svmRadial",
                         preProc = c("center", "scale"),
                         tuneLength = 50,
                         metric = "RMSE",
                         maximize = FALSE,
                         trControl = rand_ctrl)


# ------------------------------------------------------------------------------

#holidays
pred_vars <- c(var_sets$dates, var_sets$lag14, var_sets$holidays)

set.seed(4194)
svmr_date_lag14_hol <- train(s_40380 ~ .,
                             data = training[, c("s_40380", pred_vars)],
                             method = "svmRadial",
                             preProc = c("center", "scale"),
                             tuneLength = 50,
                             metric = "RMSE",
                             maximize = FALSE,
                             trControl = rand_ctrl)



unique(svmr_date_lag14$pred$Resample)
unique(svmr_date_lag14_hol$pred$Resample)

svm_new <- svm_date_lag14$pred[,c("pred","obs", "Resample")]
svm_new$Resample <- as.Date(svm_new$Resample)
str(svm_new)

svm_new_00 <- svm_date_lag14_hol$pred[,c("pred","obs", "Resample")]
svm_new_00$Resample <- as.Date(svm_new00$Resample)
str(svm_new00)

svm_new_2014 <- svm_new[which(svm_new$Resample > "2014-12-06" & svm_new$Resample < "2015-01-05"), ]
d14 <- unique(svm_new_2014$Resample)[1]
d14 <- seq(d14, as.Date("2014-12-30"), by="days")
svm_new_2014 <- svm_new_2014[1:length(d14),]
svm_new_2014$Resample <- d14
svm_new_2014 <- svm_new_2014[which(svm_new_2014$Resample >= "2014-12-15" & svm_new_2014$Resample <= "2014-12-30"), ]
svm_new_2014 <- merge(svm_new_2014, df_dec_2014, by = "Resample")
svm_new_2014 <- merge(svm_new_2014, date_pred_2014)
names(svm_new_2014)[names(svm_new_2014) == 'pred'] <- 'model_original'
svm_new_2014 <- svm_new_2014[,c("Resample","day","weekday_end","holiday","obs","model_original")]
head(svm_new_2014)

svm_new_2014_00 <- svm_new_00[which(svm_new_00$Resample > "2014-12-06" & svm_new_00$Resample < "2015-01-05"), ]
d14 <- unique(svm_new_2014_00$Resample)[1]
d14 <- seq(d14, as.Date("2014-12-30"), by="days")
svm_new_2014_00 <- svm_new_2014_00[1:length(d14),]
svm_new_2014_00$Resample <- d14
svm_new_2014_00 <- svm_new_2014_00[which(svm_new_2014_00$Resample >= "2014-12-15" & svm_new_2014_00$Resample <= "2014-12-30"), ]
names(svm_new_2014_00)[names(svm_new_2014_00) == 'pred'] <- 'model_holidays'
svm_new_2014_01 <- svm_new_2014_00[,c("Resample", "model_holidays")]
#svm_new_2014_00 <- merge(svm_new_2014_00, df_dec_2014, by = "Resample")
svm_new_2014$obs == svm_new_2014_01$obs
head(svm_new_2014_01)

svm_new_2015 <- svm_new[which(svm_new$Resample > "2015-12-06" & svm_new$Resample < "2016-01-05"), ]
d15 <- unique(svm_new_2015$Resample)[1]
d15 <- seq(d15, as.Date("2015-12-30"), by="days")
svm_new_2015 <- svm_new_2015[1:length(d15),]
svm_new_2015$Resample <- d15
svm_new_2015 <- svm_new_2015[which(svm_new_2015$Resample >= "2015-12-15" & svm_new_2015$Resample <= "2015-12-30"), ]
svm_new_2015 <- merge(svm_new_2015, df_dec_2015, by = "Resample")
svm_new_2015 <- merge(svm_new_2015, date_pred_2015)
names(svm_new_2015)[names(svm_new_2015) == 'pred'] <- 'model_original'
svm_new_2015 <- svm_new_2015[,c("Resample","day","weekday_end","holiday","obs","model_original")]
head(svm_new_2015)

svm_new_2015_00 <- svm_new_00[which(svm_new_00$Resample > "2015-12-06" & svm_new_00$Resample < "2016-01-05"), ]
d15 <- unique(svm_new_2015_00$Resample)[1]
d15 <- seq(d15, as.Date("2015-12-30"), by="days")
svm_new_2015_00 <- svm_new_2015_00[1:length(d15),]
svm_new_2015_00$Resample <- d15
svm_new_2015_00 <- svm_new_2015_00[which(svm_new_2015_00$Resample >= "2015-12-15" & svm_new_2015_00$Resample <= "2015-12-30"), ]
names(svm_new_2015_00)[names(svm_new_2015_00) == 'pred'] <- 'model_holidays'
svm_new_2015_01 <- svm_new_2015_00[,c("Resample", "model_holidays")]
#svm_new_2015_00 <- merge(svm_new_2015_00, df_dec_2015, by = "Resample")
svm_new_2015$obs == svm_new_2015_01$obs
head(svm_new_2015_01)

svm_2014 <- merge(svm_new_2014,svm_new_2014_01)
svm_2015 <- merge(svm_new_2015,svm_new_2015_01)
head(svm_2014)
head(svm_2015)

svm_2014_melt <- melt(svm_2014, id.vars = c('Resample', 'day', 'weekday_end', 'holiday'), variable.name = 'series')
head(svm_2014_melt)

svm_2015_melt <- melt(svm_2015, id.vars = c('Resample', 'day', 'weekday_end', 'holiday'), variable.name = 'series')
head(svm_2015_melt)


svm_2014_plot <- ggplot(svm_2014_melt, aes(Resample,value)) + 
  geom_line(aes(colour = series)) +
  #geom_line(aes(linetype=series)) +
  #scale_linetype_manual(values=c("twodash", "dotted", "solid")) +
  geom_point(aes(shape=weekday_end, colour = day, size=0.2)) +
  theme(legend.position="top") +
  labs(title = "December 2014 Chicago s_40380 train rides",
       subtitle = "LINEAR MODEL") +
  xlab("Date") +
  ylab("Ridership")

svm_2014_plot

svm_2015_plot <- ggplot(svm_2015_melt, aes(Resample,value)) + 
  geom_line(aes(colour = series)) +
  #geom_line(aes(linetype=series)) +
  #scale_linetype_manual(values=c("twodash", "dotted", "solid")) +
  geom_point(aes(shape=weekday_end, colour = day, size=0.2)) +
  theme(legend.position="top") +
  labs(title = "December 2015 Chicago s_40380 train rides",
       subtitle = "LINEAR MODEL") +
  xlab("Date") +
  ylab("Ridership")

svm_2015_plot

grid.arrange(svm_2014_plot, svm_2015_plot, nrow = 2, ncol = 1,
             top = textGrob("2014 (top) & 2015 (bottom)",gp=gpar(fontsize=10, fontface="bold"))
             )
             #,
             #bottom = textGrob("black dots represents the assessment set in each resample",gp=gpar(fontsize=10)))


#weather
#pred_vars <- c(var_sets$dates, var_sets$lag14, var_sets$weather)

# set.seed(4194)
# svmr_date_lag14_weth <- train(s_40380 ~ .,
#                               data = training[, c("s_40380", pred_vars)],
#                               method = "svmRadial",
#                               preProc = c("center", "scale"),
#                               tuneLength = 50,
#                               metric = "RMSE",
#                               maximize = FALSE,
#                               trControl = rand_ctrl)



