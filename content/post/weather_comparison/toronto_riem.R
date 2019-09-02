library(riem)
library(dplyr)
library(measurements)
library(summarytools)
library(visdat)
library(lubridate)
library(beepr)
library(ggplot2)

riem_networks()

riem_stations(network = "CA_ON_ASOS") %>% View()


toronto_riem <- riem_measures(station = "CYTZ", date_start = "2000-01-01", date_end = "2018-12-21")


beep("complete.wav")

toronto_riem_cleaned <- toronto_riem %>%
  select(date = valid,
         air_temp_f = tmpf,
         wind_dir = drct,
         wind_speed = sknt) %>%
  mutate(air_temp_c = conv_unit(air_temp_f, "F", "C"),
         day = day(date),
         month = month(date),
         month_names = month(date, label = TRUE),
         year = year(date),
         hour = hour(date))

saveRDS(toronto_riem_cleaned, "toronto_riem.rds")

toronto_riem_cleaned %>%
  filter(between(hour, 10, 18)) %>%
  ggplot(aes(x = month_names, y = wind_speed, group = month)) +
  geom_boxplot() 


# Friday night races?
# Why isn't it filtering out the high wind speed nights properly?
toronto_riem_cleaned %>%
  mutate(day_of_week = weekdays(date),
         week = week(date)) %>%
  filter(day_of_week == "Friday",
         hour == 19,
         between(month, 5, 9),
         wind_speed < 200) %>% # Due to that storm we had in May
  ggplot(aes(x = week, y = wind_speed)) +
  geom_col() +
  facet_wrap(vars(year))
