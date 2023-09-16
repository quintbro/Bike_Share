library(vroom)
library(tidyverse)
library(DataExplorer)
library(GGally)
library(patchwork)



setwd("C:/Users/rileyw/Bike_Share")

vroom("train.csv")
train <- vroom("train.csv")

glimpse(train)

train %>%
  mutate(season = as_factor(season),
         holiday = as_factor(holiday),
         workingday = as_factor(workingday),
         weather = as_factor(weather)) -> train

glimpse(train)

plot_intro(train)
correlation <- plot_correlation(train)
plot_bar(train)
plot_histogram(train)


ggplot(train, aes(x = temp, y = count)) +
  geom_point() +
  geom_smooth(method = loess)

View(train)

train %>%
  mutate(day_of_week = wday(datetime, label = T)) %>%
  select(count, day_of_week, temp, season, humidity) -> new_train

plot_correlation(new_train)


boxplot <- ggplot(new_train, aes(x = day_of_week, y = count, fill = day_of_week)) +
  geom_boxplot()

cor(new_train$count, new_train$lagged, use='complete.obs')

train %>%
  mutate(lagged = lag(count, n = 168)) %>%
  select(count, lagged, temp, season, humidity) -> new_train
correlation <- plot_correlation(new_train, cor_args = list("use" = "pairwise.complete.obs"))


lag_plot <- ggplot(new_train, aes(x = lagged, y = count)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme(aspect.ratio = 1) +
  theme_bw() +
  labs(
    title = "Count vs Lagged Count",
    y = "Count",
    x = "Count from One Week Previous"
  )
  

train %>%
  mutate(hour = format(as.POSIXct(datetime), format = "%H")) %>%
  mutate(hour = as.numeric(hour)) %>%
  mutate(timeDay = case_when(hour >= 22 | hour <= 6 ~ "Night",
                             hour > 6 & hour < 22 ~ "Day")) -> hour_train

train %>%
  mutate(hour = hour(datetime)) %>% glimpse()

train %>%
  mutate(lagged = lag(count, n = 24)) %>%
  select(count, lagged, temp, season, humidity) %>%
  ggplot(aes(x = lagged, y = count)) +
  geom_point()

ggplot(hour_train, aes(x = hour, y = count)) +
  geom_point(position = "jitter") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  labs(
    title = "Box Plots by Time of Day",
    x = "Time of Day",
    y = "Count"
  ) #-> timeday_plot


plots <- (boxplot + lag_plot) / (timeday_plot + correlation)
ggsave("EDAPractice.png", device = "png")


vroom("test.csv")
train %>% view()
