library(tidyverse)
library(tidymodels)

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
  step_poly(hour, degree = 4) %>%
  step_zv(all_predictors()) 
prepped_recipe <- prep(myrecipe)  

bake(prepped_recipe, test)
  
  