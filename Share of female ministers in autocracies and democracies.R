# Setup 

library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(tidymodels)
library(LiblineaR)

df <- 
  readr::read_csv("Data/df_consolidatingprogress_V1.csv") 



# Data Preparation

df_q4 <- 
  df_bmr_changed %>% 
  select(country_name) %>% # to filter the data frame to only include the country_name variable
  group_by(country_name) %>% 
  count() %>% 
  filter(n>1) %>% # to filter to have countries that appear more than once. 
  arrange(desc(n)) %>%
  knitr::kable()

print(df_q4) 

df_min_year <- 
  df_bmr_changed

df_min_year <- 
  df_min_year %>% 
  select(year, country_name) %>% 
  group_by(country_name) %>%
  mutate(min_year = min(year), max_year = max(year)) %>% 
  summarise(year = min(year),
              min_year = min(min_year),
              max_year = max(max_year)) %>% 
  select(country_name, min_year) %>% 
  arrange(desc(min_year)) %>% 
  knitr::kable()

df_min_year %>% head(10)

# Creation of a baseline model

# In the paper that published this data set, the authors used a linear regression model to predict `share_female`-related variables.

# Here, I will tackle this as a classification task. I aim to create a logistic regression model to predict whether the share of females in the cabinet will increase or decrease in the next year.

## Creation of a binary target variable and division of the data frame into a data set and a test test


df1 <- 
    df %>% 
    group_by(country_name) %>%
    arrange(year) %>% 
    mutate(
      lag_1_share_female = lag(share_female, 1),
      is_share_female_up = if_else(share_female > 1.1*lag_1_share_female, 1, 0),
      is_share_female_up = factor(is_share_female_up, levels=c("0", "1"))
    ) %>%
    drop_na(is_share_female_up) %>%
    arrange(country_name, year)

df1_train <- df1 %>% filter(year < 2021)
df1_test <- df1 %>% filter(year == 2021)

## Creation of a logistic regression model using a single predictor


logistic_model1 <-
  logistic_reg() %>%
  set_engine("glm") %>%
  fit(is_share_female_up ~ lag_1_share_female, data=df1)

plot_df1_train <- logistic_model1 %>% augment(df1_train)

min_x <- plot_df1_train %>% filter(lag_1_share_female == min(lag_1_share_female)) %>% slice(1)
max_x <- plot_df1_train %>% filter(lag_1_share_female == max(lag_1_share_female)) %>% slice(1)

g_train <- (
  ggplot(plot_df1_train, aes(x = lag_1_share_female, y=.pred_1, color=country_name)) +
    geom_point(size=1, alpha=0.3, stroke=1, show.legend = FALSE) + # I hide the legend (country names) because there are so many countries that it is not relevant anymore
    labs(x = "Share female 1 year ago", 
         y = "Probability of share female going up",
         title="Logistic regression is a good model for this problem",
         subtitle="The predictions are bounded between 0 and 1") + 
    theme_bw()
)
g_train


plot_df1_test <- logistic_model1 %>% augment(df1_test)

min_x <- plot_df1_test %>% filter(lag_1_share_female == min(lag_1_share_female)) %>% slice(1)
max_x <- plot_df1_test %>% filter(lag_1_share_female == max(lag_1_share_female)) %>% slice(1)

g_test <- (
  ggplot(plot_df1_test, aes(x = lag_1_share_female, y=.pred_1, color=country_name)) +
  geom_point(size=1, alpha=0.3, stroke=1, show.legend = FALSE) + # I hide the legend (country names) because there are so many countries that it is not relevant anymore
  labs(x = "Share female 1 year ago", 
       y = "Probability of share female going up",
       title="Logistic regression is a good model for this problem",
       subtitle="The predictions are bounded between 0 and 1") + 
  theme_bw()
)
g_test


## Evaluation of the model's performance
# For the training set: 
logistic_model1 %>% 
  augment(df1_train) %>%
  conf_mat(truth=is_share_female_up, estimate=.pred_class) %>%
  autoplot(type="heatmap") +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")

#For the testing set: 
logistic_model1 %>% 
  augment(df1_test) %>%
  conf_mat(truth=is_share_female_up, estimate=.pred_class) %>%
  autoplot(type="heatmap") +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")

logistic_model1 %>% 
  augment(df1_train) %>%
  conf_mat(truth=is_share_female_up, estimate=.pred_class) %>%
  summary(estimator="binary", event_level="second") %>%
  knitr::kable()

logistic_model1 %>% 
  augment(df1_test) %>%
  conf_mat(truth=is_share_female_up, estimate=.pred_class) %>%
  summary(estimator="binary", event_level="second") %>%
  knitr::kable()


## Explanation of the regression coefficients
logistic_model1$fit 

## Goodness-of-fit of the model and the model's predictive power

logistic_model1 %>% 
  augment(df1_train) %>%
  roc_curve(truth=is_share_female_up, .pred_1, event_level="second") %>%
  autoplot() + 
  geom_point(aes(x=1-specificity, y=sensitivity, color=.threshold)) + 
  scale_color_gradient(name="Threshold", low = "#c6733c", high="#3cc6b8", limits=c(0, 1)) + 
  labs(title="ROC curve",
       x="(1 - specificity) = 1 - TN/N",
       y="(sensitivity) = TP/P")

logistic_model1 %>% 
  augment(df1_test) %>%
  roc_curve(truth=is_share_female_up, .pred_1, event_level="second") %>%
  autoplot() + 
  geom_point(aes(x=1-specificity, y=sensitivity, color=.threshold)) + 
  scale_color_gradient(name="Threshold", low = "#c6733c", high="#3cc6b8", limits=c(0, 1)) + 
  labs(title="ROC curve",
       x="(1 - specificity) = 1 - TN/N",
       y="(sensitivity) = TP/P")

logistic_model1 %>% 
  augment(df1_train) %>%
  roc_auc(truth=is_share_female_up, .pred_1, event_level="second") %>%
  knitr::kable()

logistic_model1 %>% 
  augment(df1_test) %>%
  roc_auc(truth=is_share_female_up, .pred_1, event_level="second") %>% 
  knitr::kable()


# Improving the model's performance

## Part A: Adding dummy variables
rec_dummy <-
  recipe(is_share_female_up ~ lag_1_share_female + country_name, data=df1_train) %>%
  step_dummy(country_name) %>%
  prep()

logistic_model2_workflow <-
  workflow() %>%
  add_recipe(rec_dummy) %>%
  add_model(logistic_reg() %>% set_engine("glm")) %>%
  fit(df1_train)

logistic_model2 <- logistic_model2_workflow %>% extract_fit_parsnip()

logistic_model2$fit

logistic_model2_workflow %>% 
  augment(df1_train) %>%
  group_by(.pred_class, is_share_female_up) %>%
  tally() %>%
  tidyr::pivot_wider(values_from=n, names_from="is_share_female_up") %>% 
  knitr::kable()
 

logistic_model2 %>% 
  augment(rec_dummy %>% bake(df1_train)) %>% 
  conf_mat(truth=is_share_female_up, estimate=.pred_class) %>%
  autoplot(type="heatmap") +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")

logistic_model2 %>% 
  augment(rec_dummy %>% bake(df1_test)) %>% 
  conf_mat(truth=is_share_female_up, estimate=.pred_class) %>%
  autoplot(type="heatmap") +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")

logistic_model2 %>% 
  augment(rec_dummy %>% bake(df1_train)) %>% 
  roc_curve(truth=is_share_female_up, .pred_1, event_level="second") %>%
  autoplot() + 
  geom_point(aes(x=1-specificity, y=sensitivity, color=.threshold)) + 
  scale_color_gradient(name="Threshold", low = "#c6733c", high="#3cc6b8", limits=c(0, 1)) + 
  labs(title="ROC curve",
       x="(1 - specificity) = 1 - TN/N",
       y="(sensitivity) = TP/P")

logistic_model2 %>% 
  augment(rec_dummy %>% bake(df1_test)) %>% 
  roc_curve(truth=is_share_female_up, .pred_1, event_level="second") %>%
  autoplot() + 
  geom_point(aes(x=1-specificity, y=sensitivity, color=.threshold)) + 
  scale_color_gradient(name="Threshold", low = "#c6733c", high="#3cc6b8", limits=c(0, 1)) + 
  labs(title="ROC curve",
       x="(1 - specificity) = 1 - TN/N",
       y="(sensitivity) = TP/P")

logistic_model2 %>% 
  augment(rec_dummy %>% bake(df1_train)) %>% 
  roc_auc(truth=is_share_female_up, .pred_1, event_level="second") %>%
  knitr::kable()

logistic_model2 %>% 
  augment(rec_dummy %>% bake(df1_test)) %>% 
  roc_auc(truth=is_share_female_up, .pred_1, event_level="second") %>%
  knitr::kable()


## Part B: Finding the perfect threshold and comparing the model's performance with this threshold to a 0.5 threshold.


best_threshold <- 0 
best_score_train <- 0 

threshold_function <- function(my_threshold){ 
  metrics11 <- logistic_model2 %>% 
    augment(rec_dummy %>% bake(df1_test)) %>% 
    mutate(.pred_class= .pred_1 > my_threshold, 
           .pred_class=factor(.pred_class, 
                              labels=c("0","1"), 
                              levels=c(FALSE, TRUE),
                              ordered=TRUE)) %>%
    conf_mat(truth=is_share_female_up, estimate=.pred_class) %>% 
    summary(estimator="binary", event_level="second")
  print(metrics11)
  
  metrics11 %>%
    filter(.metric =='f_meas') %>% 
    pull(.estimate)
}

#use loop to test different thresholds

for (i in 1:99){ 
  my_threshold <- i/100
  current_score <- threshold_function(my_threshold)
  if(is.na(current_score)) {
    current_score=0
  }
  if(as.numeric(current_score) > as.numeric(best_score_train)) {
    best_score_train <- current_score
    best_threshold <- max(my_threshold, best_threshold)
  }
}

best_threshold


my_threshold <- 0.19

logistic_model2 %>% 
  augment(rec_dummy %>% bake(df1_train)) %>% 
  mutate(.pred_class = .pred_1 > my_threshold,
           .pred_class = factor(.pred_class, 
                                labels=c("0","1"), 
                                levels=c(FALSE, TRUE), 
                                ordered=TRUE)) %>%
    conf_mat(truth=is_share_female_up, estimate=.pred_class) %>%
    autoplot(type="heatmap") +
    scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")


my_threshold <- 0.19

logistic_model2 %>% 
  augment(rec_dummy %>% bake(df1_test)) %>% 
  mutate(.pred_class = .pred_1 > my_threshold,
           .pred_class = factor(.pred_class, 
                                labels=c("0","1"), 
                                levels=c(FALSE, TRUE), 
                                ordered=TRUE)) %>%
    conf_mat(truth=is_share_female_up, estimate=.pred_class) %>%
    autoplot(type="heatmap") +
    scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")


my_threshold <- 0.19

logistic_model2 %>% 
  augment(rec_dummy %>% bake(df1_train)) %>% 
  mutate(.pred_class = .pred_1 > my_threshold,
         .pred_class = factor(.pred_class, 
                              labels=c("0","1"), 
                              levels=c(FALSE, TRUE), 
                              ordered=TRUE)) %>%
  conf_mat(truth=is_share_female_up, estimate=.pred_class) %>%
  summary(estimator="binary", event_level="second") %>% 
  knitr::kable()


my_threshold <- 0.19

logistic_model2 %>% 
  augment(rec_dummy %>% bake(df1_test)) %>% 
  mutate(.pred_class = .pred_1 > my_threshold,
         .pred_class = factor(.pred_class, 
                              labels=c("0","1"), 
                              levels=c(FALSE, TRUE), 
                              ordered=TRUE)) %>%
  conf_mat(truth=is_share_female_up, estimate=.pred_class) %>%
  summary(estimator="binary", event_level="second") %>% 
  knitr::kable()

## Part C: Adding other predictors to the dummy variables model

df2 <- 
    df %>% 
    group_by(country_name) %>%
    arrange(year) %>% 
    mutate(
      lag_1_share_female = lag(share_female, 1),
      lag_2_share_female = lag(share_female, 2),
      lag_3_share_female = lag(share_female, 3),
      lag_4_share_female = lag(share_female, 4),
      lag_5_share_female = lag(share_female, 5),
      lag_6_share_female = lag(share_female, 6),
      lag_7_share_female = lag(share_female, 7),
      lag_8_share_female = lag(share_female, 8),
      lag_9_share_female = lag(share_female, 9),
      lag_10_share_female = lag(share_female, 10),
      is_share_female_up = if_else(share_female > 1.1*lag_1_share_female, 1, 0),
      is_share_female_up = factor(is_share_female_up, levels=c("0", "1"))
    ) %>%
    drop_na(is_share_female_up, lag_1_share_female, lag_2_share_female, lag_3_share_female, lag_4_share_female, lag_5_share_female, lag_6_share_female, lag_7_share_female, lag_8_share_female, lag_9_share_female, lag_10_share_female) %>%
    arrange(country_name, year)

df2_train <- df2 %>% filter(year < 2021)
df2_test <- df2 %>% filter(year == 2021)

rec_dummy_lag <-
  recipe(is_share_female_up ~ lag_1_share_female + lag_2_share_female + lag_3_share_female + lag_4_share_female + lag_5_share_female + lag_6_share_female + lag_7_share_female + lag_8_share_female + lag_9_share_female + lag_10_share_female + country_name, data=df2_train) %>%
  step_dummy(country_name) %>%
  prep()

logistic_model3_workflow <-
  workflow() %>%
  add_recipe(rec_dummy_lag) %>%
  add_model(logistic_reg() %>% set_engine("glm")) %>%
  fit(df2_train)

logistic_model3 <- logistic_model3_workflow %>% extract_fit_parsnip()

logistic_model3 %>% 
  augment(rec_dummy_lag %>% bake(df2_train)) %>% 
  roc_curve(truth=is_share_female_up, .pred_1, event_level="second") %>%
  autoplot() + 
  geom_point(aes(x=1-specificity, y=sensitivity, color=.threshold)) + 
  scale_color_gradient(name="Threshold", low = "#c6733c", high="#3cc6b8", limits=c(0, 1)) + 
  labs(title="ROC curve",
       x="(1 - specificity) = 1 - TN/N",
       y="(sensitivity) = TP/P")

logistic_model3 %>% 
  augment(rec_dummy_lag %>% bake(df2_test)) %>% 
  roc_curve(truth=is_share_female_up, .pred_1, event_level="second") %>%
  autoplot() + 
  geom_point(aes(x=1-specificity, y=sensitivity, color=.threshold)) + 
  scale_color_gradient(name="Threshold", low = "#c6733c", high="#3cc6b8", limits=c(0, 1)) + 
  labs(title="ROC curve",
       x="(1 - specificity) = 1 - TN/N",
       y="(sensitivity) = TP/P")

logistic_model3 %>% 
  augment(rec_dummy_lag %>% bake(df2_train)) %>% 
  roc_auc(truth=is_share_female_up, .pred_1, event_level="second") %>%
  knitr::kable()

logistic_model3 %>% 
  augment(rec_dummy_lag %>% bake(df2_test)) %>% 
  roc_auc(truth=is_share_female_up, .pred_1, event_level="second") %>%
  knitr::kable()

logistic_model3 %>% 
  augment(rec_dummy_lag %>% bake(df2_train)) %>% 
  conf_mat(truth=is_share_female_up, estimate=.pred_class) %>%
  autoplot(type="heatmap") +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")

logistic_model3 %>% 
  augment(rec_dummy_lag %>% bake(df2_test)) %>% 
  conf_mat(truth=is_share_female_up, estimate=.pred_class) %>%
  autoplot(type="heatmap") +
  scale_fill_gradient(low="#D6EAF8",high = "#2E86C1")







