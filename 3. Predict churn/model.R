## Load libraries
library(tidyverse)
library(readr)
library(here)
library(skimr)
library(dplyr)
library(data.table)
library(tidymodels)
library(doParallel)
library(knitr)
library(themis)

## Load data
url <- "https://raw.githubusercontent.com/hannahtabea/HR-analytics/8c7abc5ef610c1f7ecc4596cf0ce6f55a2ffccf1/WA_Fn-UseC_-HR-Employee-Attrition.csv"
raw_Data <- fread(url) %>%
  mutate(Attrition = ifelse(Attrition == "Yes", TRUE, FALSE)) 

## Wrangle data
processed_Data <- raw_Data %>% 
  # create CompensationRatio by joblevel
  # create CompensationRatio by joblevel
  group_by(JobLevel) %>%
  mutate(median_compensation = median(MonthlyIncome),
         CompensationRatio = (MonthlyIncome/median(MonthlyIncome)),
         CompensationLevel = case_when(
           between(CompensationRatio, 0.75,1.25) ~ "Average",
           between(CompensationRatio, 0, 0.75) ~ "Below",
           between(CompensationRatio, 1.25, 2) ~ "Above"
         )) %>%
  ungroup() %>%
  # convert all characters to factors
  mutate_if(is.character, as.factor) %>%
  mutate(Attrition = as.factor(Attrition))

processed_Data <- processed_Data %>%
  select(-Over18)

## Create train/test split
data_Split <- initial_split(processed_Data, prop = 7/10, strata = Attrition)

train <- data_Split %>%
  training()


test <- data_Split %>%
  testing()

# create data folds for cross validation - 10 folds
myFolds <- vfold_cv(train, repeats = 5,
                    strata = Attrition)

# Prepare for parallel processing
all_cores <- parallel::detectCores(logical = TRUE)
registerDoParallel(cores = all_cores)

## Prepare for preprocessing

train_rec <- train %>%
  recipe(Attrition ~ .) %>%
  # normalize all numeric predictors
  step_normalize(all_numeric()) %>%
  # create dummy variables 
  step_dummy(all_nominal(), - all_outcomes()) %>%
  # remove zero variance predictors
  step_nzv(all_predictors(), - all_outcomes()) %>%
  # remove highly correlated vars
  step_corr(all_numeric(), threshold = 0.75) %>%
  # deal with class imbalance
  step_rose(Attrition)

## Model fitting

# create model-specific recipes
log_spec <- 
  logistic_reg(penalty = tune(), # lambda
               mixture = tune()) %>% # alpha 
  set_engine("glmnet") 

xgb_spec <- 
  parsnip::boost_tree(mtry = tune(), # colsample_bytree
                      sample_size = tune(), # subsample
                      tree_depth = tune(), # max_depth
                      trees = 100, # n_rounds 
                      learn_rate = tune(), # eta
                      loss_reduction = tune(), # gamma
                      min_n = tune()) %>% # min_child_weight
  set_mode("classification")%>%
  set_engine("xgboost")


# Create params object for glmnet
glmnet_params <- 
  dials::parameters(list(
    penalty(), 
    mixture()
  ))


# Create params object for XGB
xgb_params <- 
  dials::parameters(list(
    min_n(),
    tree_depth(),
    learn_rate(),
    loss_reduction(),
    sample_size = sample_prop(),
    finalize(mtry(), train)
  ))


# Generate irregular grids
glmnet_grid <- grid_latin_hypercube(glmnet_params,
                                    size = 16 # like caret
)
xgbTree_grid <- grid_latin_hypercube(xgb_params, 
                                     size = 256 #like caret
)

# create a workflow SET
library(workflowsets)
my_models <- 
  workflow_set(
    preproc = list(train_rec),
    models = list(glmnet = log_spec,  xgbTree = xgb_spec),
    cross = TRUE
  ) %>%
  # add custom grid 
  option_add(grid = xgbTree_grid, id = "recipe_xgbTree") %>%
  option_add(grid = glmnet_grid, id = "recipe_glmnet") 

my_models

train_metrics <- metric_set(bal_accuracy, roc_auc, yardstick::sensitivity, yardstick::specificity, yardstick::precision, f_meas)

# actual training
model_race <- my_models %>% 
  #  option_add(param_info = rfe_param) %>%
  workflow_map("tune_grid", resamples = myFolds, verbose = TRUE,
               control = tune::control_grid(verbose = TRUE, extract = identity),
               metrics = train_metrics)

