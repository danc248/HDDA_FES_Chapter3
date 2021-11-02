# Code requires these packages: 

library(tidymodels)
library(gridExtra)

theme_set(theme_bw() + theme(legend.position = "top"))

# ------------------------------------------------------------------------------

# Load linear model results for Chicago data
path0 = "/Users/danielececcarelli/Documents/2 - High Dimensional Data Analysis/final_project_FES/"
file0 = "lm_date_only.RData"
file1 = paste0(path0, file0)
load(file1)

# ------------------------------------------------------------------------------

lm_date_only$pred %>% rmse(obs, pred)
lm_date_only$pred %>% rsq(obs, pred)

# ------------------------------------------------------------------------------

chi_xy <- ggplot(lm_date_only$pred, aes(x = obs, y = pred)) + 
  geom_abline(intercept = 0, slope = 1, col = "grey", lty = 1) + 
  geom_point(alpha = .5, size = .7) + 
  geom_smooth(method = lm, se = FALSE, col = rgb(0, 0, 1, .2), alpha = .5) + 
  xlab("Actual Train Ridership (thousands)") + 
  ylab("Predicted Ridership (thousands)") + 
  ggtitle("(a)")

chi_xy

chi_hst <- ggplot(lm_date_only$pred, aes(x = obs - pred)) + 
  geom_histogram(binwidth = 1, col = "#377EB8", fill = "#377EB8", alpha = .5) + 
  xlab("Ridership Residuals (thousands)") + 
  ggtitle("(b)")

chi_hst

grid.arrange(chi_xy, chi_hst, nrow = 1, ncol = 2)
