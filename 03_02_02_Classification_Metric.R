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

confusion0 <- glm_keyword$pred %>% conf_mat(obs, pred)   # table 3.1
print(confusion0)
autoplot(confusion0, type = "mosaic") #mosaic plot of the confusion matrix

class_metrics <- metric_set(accuracy, kap, sens, spec, precision, recall)

glm_keyword$pred %>% class_metrics(obs, estimate = pred)

uncond_metrics <- metric_set(ppv, npv)
glm_keyword$pred %>% uncond_metrics(obs, estimate = pred, prevalence = 0.05)
glm_keyword$pred %>% uncond_metrics(obs, estimate = pred, prevalence = 0.05)

glm_keyword$pred %>% ppv(obs, pred, prevalence = 0.05)
glm_keyword$pred %>% npv(obs, pred, prevalence = 0.05)

# figure 3.3. - let's change the probability cutoffs:
# first example: (a) 20% prob cutoff
ex1 <- glm_keyword$pred
ex1$pred[which(ex1$stem > 0.2)] = "stem"
print(ex1[3, ])
confusion1 <- ex1 %>% conf_mat(obs, pred)   # figure 3.3 mosaic plot (a)
print(confusion1)
conf1 <- autoplot(confusion1, type = "mosaic")   # figure 3.3 mosaic plot (a)
conf1

df1 <- ex1 %>% class_metrics(obs, estimate = pred)
df1 <- data.frame(df1)
df1 <- df1[, c(1,3)]
names(df1) <- c(".metric", ".estimate1_20%")

# second example: (b) 50% prob cutoff
ex2 <- glm_keyword$pred
ex2$pred[which(ex2$stem > 0.5)] = "stem"
print(ex2[1, ])
confusion2 <- ex2 %>% conf_mat(obs, pred)   # figure 3.3 mosaic plot (b)
print(confusion2)
conf2 <- autoplot(confusion2, type = "mosaic") # figure 3.3 mosaic plot (b)
conf2

df2 <- ex2 %>% class_metrics(obs, estimate = pred)
df2 <- data.frame(df2)
df2 <- df2[, c(1,3)]
names(df2) <- c(".metric", ".estimate2_50%")

# third example: (c) 80% prob cutoff
ex3 <- glm_keyword$pred
ex3$pred[which(ex3$stem <= 0.8)] = "other"
print(ex3[1, ])
confusion3 <- ex3 %>% conf_mat(obs, pred)   # figure 3.3 mosaic plot (c)
print(confusion3)
conf3 <- autoplot(confusion3, type = "mosaic") # figure 3.3 mosaic plot (c)
conf3

df3 <- ex3 %>% class_metrics(obs, estimate = pred)
df3 <- data.frame(df3)
df3 <- df3[, c(1,3)]
names(df3) <- c(".metric", ".estimate3_80%")

# as you can see in the following table, a higher probability cutoff leads to a higher precision
# higher precision --> less False Positives
final_table0 <- inner_join(df1, df2)
final_table1 <- inner_join(final_table0, df3)
final_table1       #metrics calculated with different probability cutoffs                         

grid.arrange(conf1, conf2, conf3, nrow = 1, ncol = 3)  # figure 3.3 - mosaic plots


# binomial log-likelihood statistic

ex44 <- data.frame(glm_keyword$pred)
ex44$correct_prediction <- ex44$obs == ex44$pred
ex44$correct_prediction = as.numeric(ex44$correct_prediction)
#head(ex44)
ex55 <- ex44[ , c("correct_prediction","stem", "other")]
ex55$y_log_stem <- log(ex55$stem) * ex55$correct_prediction
ex55$gini <- ex55$stem * ex55$other
ex55$entropy <- (log2(ex55$stem) * ex55$stem) + (log2(ex55$other) * ex55$other) *(-1)
head(ex55)

print(c("binomial log_likelihood statistic:",sum(ex55$y_log_stem)))  # binomial log-likelihood statistic of glm_keyword$pred
print(c("Gini criterion:",sum(ex55$gini))) # Gini criterion
print(c("entropy:",sum(ex55$entropy))) # entropy


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
