# Code requires these packages: 

library(tidymodels)
library(gridExtra)

theme_set(theme_bw() + theme(legend.position = "top"))

# ------------------------------------------------------------------------------

# Load logistic model results for OkC data

path0 = "/Users/danielececcarelli/Documents/2 - High Dimensional Data Analysis/final_project_FES/"
file0 = "okc_glm_keyword.RData"
file1 = paste0(path0, file0)
load(file1)

# ------------------------------------------------------------------------------

glm_keyword$pred %>% conf_mat(obs, pred)

class_metrics <- metric_set(accuracy, kap, sens, spec, precision)

glm_keyword$pred %>% class_metrics(obs, estimate = pred)

uncond_metrics <- metric_set(ppv, npv)
glm_keyword$pred %>% uncond_metrics(obs, estimate = pred, prevalence = 0.05)
glm_keyword$pred %>% uncond_metrics(obs, estimate = pred, prevalence = 0.05)

glm_keyword$pred %>% ppv(obs, pred, prevalence = 0.05)
glm_keyword$pred %>% npv(obs, pred, prevalence = 0.05)

# ------------------------------------------------------------------------------

# https://bookdown.org/max/FES/measuring-performance.html#tab:review-class-table
fig_3_2 <- 
  glm_keyword$pred %>% 
  ggplot(aes(x = stem)) + 
  geom_histogram(binwidth = 0.03, col = "#377EB8", fill = "#377EB8", alpha = .5) + 
  facet_wrap(~obs, ncol = 1) + 
  xlab("Pr[Profile is STEM]")

fig_3_2



# https://bookdown.org/max/FES/measuring-performance.html#fig:review-roc-pr-plot
# panel (a)
glm_keyword$pred %>% roc_auc(obs, stem)
fig_3_4_panel_a = glm_keyword$pred %>% roc_curve(obs, stem) %>% autoplot()



# https://bookdown.org/max/FES/measuring-performance.html#fig:review-roc-pr-plot
# panel (b)
glm_keyword$pred %>% pr_auc(obs, stem)
fig_3_4_panel_b = glm_keyword$pred %>% pr_curve(obs, stem) %>% autoplot()

grid.arrange(fig_3_4_panel_a, fig_3_4_panel_b, nrow = 1, ncol = 2)

# ------------------------------------------------------------------------------
