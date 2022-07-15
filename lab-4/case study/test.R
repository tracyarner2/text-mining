library(naivebayes)
library(tidyverse)
library(tidymodels)
library(textrecipes)
library(discrim) #naive Bayes model is available in the tidymodels package discrim.


comments <- read_csv("data/comments.csv")

set.seed(123)

comments <- comments %>%
  mutate(more_info=as.factor(more_info))

comments_split <- initial_split(comments, strata = more_info)

comments_train <- training(comments_split)
comments_test <- testing(comments_split)

comments_rec <-
  recipe(more_info ~ comment, data = comments_train)

comments_rec <- comments_rec %>%
  step_tokenize(comment) %>%
  step_tokenfilter(comment, max_tokens = 1e3) %>%
  step_tfidf(comment)

set.seed(234)
comments_folds <- vfold_cv(comments_train)

comments_folds

svm_spec <- svm_poly (degree = 1) %>%
  set_mode("classification")%>%
  set_engine("kernlab")

svm_wf <- workflow() %>%
  add_recipe(comments_rec) %>%
  add_model(svm_spec)

svm_rs <- fit_resamples(
  svm_wf,
  comments_folds,
  control = control_resamples(save_pred = TRUE)
)
