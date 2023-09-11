library(vroom)
library(DataExplorer)
library(GGally)
train <- vroom("train.csv")

glimpse(train)

train %>%
  mutate(season = as_factor(season),
         holiday = as_factor(holiday),
         workingday = as_factor(workingday),
         weather = as_factor(weather)) -> train

glimpse(train)
trim(train)
plot_intro(train)
correlation <- plot_correlation(train)
plot_bar(train)
plot_histogram(train)
pairs(train)

ggpairs(train)

cor(train)



