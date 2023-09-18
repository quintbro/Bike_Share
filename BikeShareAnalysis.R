library(tidyverse)
library(tidymodels)
library(vroom)
library(MASS)
library(poissonreg)

setwd("C:/Users/rileyw/Bike_Share")
# Read in the training data set
vroom("train.csv") -> train
vroom("test.csv") -> test

# Clean the data set by assigning weather = 4 to a different category because there
# is only one observation
train %>%
  mutate(weather = case_when(weather == 4 ~ 3,
                             weather < 4 ~ weather)) -> train

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

