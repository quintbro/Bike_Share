library(tidyverse)
library(tidymodels)
library(vroom)
library(MASS)
library(poissonreg)

setwd("C:/Users/rileyw/Bike_Share")
# Read in the training data set
vroom("train.csv") -> train
vroom("test.csv") -> test



# Use tidy models to do some feature engineering
myrecipe <- recipe(count ~ datetime + season + holiday + workingday + weather + temp + atemp + humidity + windspeed
                   , data = train) %>%
  step_mutate(hour = hour(datetime)) %>% # create an hour variable
  step_mutate(hour = as.numeric(hour)) %>% # change the hour to be numeric
  step_mutate(timeDay = case_when(hour >= 22 | hour <= 6 ~ "Night", # make a time of day variable
                             hour > 6 & hour < 22 ~ "Day")) %>%     # with night and day
  step_mutate(weather = if_else(weather == 4, 3, weather)) %>%
  step_mutate(season = as_factor(season),
              holiday = as_factor(holiday),
              workingday = as_factor(workingday),
              weather = as_factor(weather)) %>%
  step_rm(atemp) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_zv(all_predictors()) 




prepped_recipe <- prep(myrecipe)  

bake(prepped_recipe, test)

my_mod <- linear_reg() %>%
  set_engine("lm")

bike_workflow <- workflow() %>%
  add_recipe(myrecipe) %>%
  add_model(my_mod) %>%
  fit(data = train)

bike_predictions <- predict(bike_workflow,
                            new_data = test)
bike_predictions %>%
  mutate(datetime = test$datetime) %>%
  mutate(.pred  = if_else(.pred < 0 | is.na(.pred), 0, .pred)) %>%
  rename(count = .pred) %>%
  dplyr::select(datetime, count) %>%
  mutate(datetime = format(as_datetime(datetime), "%Y-%m-%d %H:%M:%S"))-> submission

vroom_write(submission, "submission.csv", delim = ",")

write.csv(submission, "first_submission.csv", row.names = F)


#-------------------- POISSON REGRESSION -----------------------

pois_mod <- poisson_reg() %>%
  set_engine("glm")
bike_pois_workflow <- workflow() %>%
  add_recipe(myrecipe) %>%
  add_model(pois_mod) %>%
  fit(data = train)


pois_preds <- predict(bike_pois_workflow, new_data = test)
pois_preds %>%
  mutate(datetime = test$datetime) %>%
  rename(count = .pred) %>%
  mutate(datetime = format(datetime)) %>%
  dplyr::select(datetime, count) -> submission

vroom_write(submission, "poisson_sub.csv", delim = ",")
extract_fit_engine(bike_pois_workflow)



#--------------Penalized Regression------------------------------------

setwd("C:/Users/rileyw/Bike_Share")
# Read in the training data set
vroom("train.csv") -> train
vroom("test.csv") -> test

# Log transform the count variable
log_train <- train %>%
  mutate(count = log(count))


# Use tidy models to do some feature engineering
myrecipe <- recipe(count ~ datetime + season + holiday + workingday + weather + 
                     temp + atemp + humidity + windspeed,
                   data = train) %>%
  step_mutate(hour = hour(datetime)) %>% # create an hour variable
  step_mutate(hour = as.numeric(hour)) %>% # change the hour to be numeric
  step_mutate(timeDay = case_when(hour >= 22 | hour <= 6 ~ "Night", # make a time of day variable
                                  hour > 6 & hour < 22 ~ "Day")) %>% # with night and day
  step_mutate(timeDay=factor(timeDay)) %>%
  step_mutate(weather = if_else(weather == 4, 3, weather)) %>%
  step_mutate(season = as_factor(season),
              holiday = as_factor(holiday),
              workingday = as_factor(workingday),
              weather = as_factor(weather)) %>%
  step_mutate(decimal = decimal_date(datetime)) %>%
  step_interact(terms = ~ weather*humidity) %>%
  step_rm(atemp) %>%
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>%      # We need to make them into dummies because the penalized regression methods don't like categorical variables
  step_normalize(all_numeric_predictors()) %>% # This is putting all of the variables on the same scale
  step_zv(all_predictors()) 

bake(prep(myrecipe), train)

preg_model <- linear_reg(penalty = 1, mixture = 0) %>%
  set_engine("glmnet")
preg_wf <- workflow() %>%
  add_recipe(myrecipe) %>%
  add_model(preg_model) %>%
  fit(data = train)

preds <- predict(preg_wf, new_data = test) %>%
  mutate(count = exp(.pred)) %>%
  mutate(datetime = format(test$datetime)) %>%
  mutate(count = if_else(count < 0, 0, count)) %>%
  dplyr::select(datetime, count)

vroom_write(preds, "pen.csv", delim = ",")

comb_train <- train %>%
  dplyr::select(-c(casual, registered)) %>%
  mutate(which = FALSE)

test %>%
  mutate(count = NA) %>%
  mutate(which = TRUE) %>%
  rbind(., comb_train) %>%
  arrange(datetime) -> comb

comb %>%
  mutate(lag = lag(count, n = 336)) %>%
  filter(which == T) -> new_test

#------------- Cross Validation -----------------

# 3 different flavors
# Monte Carlo
# K-fold
# Leave one out

# Metrics for Evaluating prediction
# RMSE
# Bias
# Mean Absolute Error

# Steps
# 1. Define a grid possible values of tuning parameter values
# 2. Run cross-validation on each possible combination
# 3. Use the parameters that minimize your function

library(tidymodels)
library(tidyverse)
library(vroom)

setwd("C:/Users/rileyw/Bike_Share")
# Read in the training data set
vroom("train.csv") -> train
vroom("test.csv") -> test

# Log transform the count variable
train <- train %>%
  mutate(count = log(count))

myrecipe <- recipe(count ~ datetime + season + holiday + workingday + weather + 
                     temp + atemp + humidity + windspeed,
                   data = train) %>%
  step_mutate(hour = hour(datetime)) %>% # create an hour variable
  step_mutate(hour = as.numeric(hour)) %>% # change the hour to be numeric
  step_mutate(timeDay = case_when(hour >= 22 | hour <= 6 ~ "Night", # make a time of day variable
                                  hour > 6 & hour < 22 ~ "Day")) %>% # with night and day
  step_mutate(timeDay=factor(timeDay)) %>%
  step_mutate(weather = if_else(weather == 4, 3, weather)) %>%
  step_mutate(season = as_factor(season),
              holiday = as_factor(holiday),
              workingday = as_factor(workingday),
              weather = as_factor(weather)) %>%
  step_interact(terms = ~ weather*humidity) %>%
  step_rm(atemp) %>%
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>%      # We need to make them into dummies because the penalized regression methods don't like categorical variables
  step_normalize(all_numeric_predictors()) %>% # This is putting all of the variables on the same scale
  step_zv(all_predictors()) 

bake(prep(myrecipe), train)

reg_model <- poisson_reg(penalty = tune(), 
                        mixture = tune()) %>%
  set_engine("glmnet")

reg_wf <- workflow() %>%
  add_recipe(myrecipe) %>%
  add_model(reg_model)

tuning_grid <- grid_regular(penalty(),
                            mixture(),
                            levels = 10)

fold_div <- vfold_cv(train, repeats = 1, v = 10)

Cross_val_results <- reg_wf %>%
  tune_grid(resamples = fold_div,
            grid = tuning_grid,
            metrics = metric_set(rmse, mae))

collect_metrics(Cross_val_results)

bestTune <- Cross_val_results %>%
  select_best("rmse")


final_wf <- reg_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data = train)

preds <- predict(final_wf, new_data = test) %>%
  mutate(count = exp(.pred)) %>%
  mutate(datetime = format(test$datetime)) %>%
  mutate(count = if_else(count < 0, 0, count)) %>%
  dplyr::select(datetime, count)

vroom_write(preds, "pen.csv", delim = ",")

