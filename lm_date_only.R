library(caret)
library(sessioninfo)
library(doParallel)


path0 = "/Users/danielececcarelli/Documents/2 - High Dimensional Data Analysis/final_project_FES/"
file0 = "chicago.RData"
file1 = paste0(path0, file0)
load(file1)

set.seed(4194)
lm_date_only <- train(s_40380 ~ .,
                      data = training[, c("s_40380", var_sets$dates)],
                      method = "lm",
                      metric = "RMSE",
                      maximize = FALSE,
                      model = FALSE,
                      trControl = ctrl)

attr(lm_date_only$finalModel$terms, ".Environment") <- emptyenv()

# save(lm_date_only, file = "lm_date_only.RData")

# ------------------------------------------------------------------------------

session_info()

if(!interactive()) 
  q("no")
