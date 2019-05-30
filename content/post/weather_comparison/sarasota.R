library(dplyr)
library(readr)
library(visdat)
library(janitor)
library(measurements)
library(ggplot2)
library(lubridate)

sarasota <- read_csv("1577899.csv")

sarasota <- sarasota %>%
  clean_names()

sarasota <- sarasota %>%
  mutate(tavg = (tmax + tmin) / 2)

sarasota <- sarasota %>%
  mutate(knots = conv_unit(awnd, "m_per_sec", "knot"))

sarasota <- sarasota %>%
  mutate(day = day(date),
         month = month(date),
         year = year(date))

# General graph showing the average temparature
sarasota %>%
  ggplot(aes(date, tavg)) +
  geom_line()

sarasota <- sarasota %>%
  filter(month == 3)

# Graph showing the range of the temperatures throughout March
sarasota %>%
  group_by(day) %>%
  mutate(avr_temp = mean(tmax)) %>%
  ggplot(aes(day, avr_temp)) +
  geom_line()

sarasota %>%
  ggplot(aes(day, knots, group = year)) +
  geom_line()

sarasota <- sarasota %>%
  mutate(day_of_week = wday(date, label = TRUE))

sarasota %>%
  ggplot(aes(date, knots, group = year)) +
  geom_boxplot()

sarasota %>%
  filter(year == 2015, day > 19) %>%
  select(day, day_of_week, knots)

sarasota %>%
  ggplot(aes(date, tavg, group = year)) +
  geom_boxplot()

# What percentage of days in March are less than 5 knots?
sarasota %>%
  mutate(less_than_5_knots = knots < 5) %>%
  group_by(year) %>%
  mutate(how_many_days = sum(less_than_5_knots),
         prop = how_many_days / 31 * 100) %>%
  distinct(year, how_many_days, prop) %>%
  ungroup()
